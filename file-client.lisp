;;;; The file client will manage the inode abstraction 
;;;; Which includes read/write, and the management of indirect blocks
;;;; This file should be loaded after the rpc-clients.lisp file

;;;; The inode block layout
;;;; +--------------+-------+-------+-------+--------+------+
;;;; | size (32Bit) | atime | ctime | mtime | refcnt | ptrs |
;;;; +--------------+-------+-------+-------+--------+------+

(in-package :cl-yfs-clients)

(defparameter *num-of-ptr* 100)
(defparameter *num-of-l1-indirect* 1)
(defparameter *num-of-l2-indirect* 1)
(defparameter *block-size* 512)

(defclass file-client ()
  ((extent-client :accessor extent-client
		  :initform nil
		  :initarg :extent-client)
   (binded :accessor is-binded
	   :initform nil)
   (inum :accessor inode-num
	 :initform nil
	 :initarg :inum)
   (size :accessor file-size
	 :initform 0)
   (atime :accessor atime
	  :initform 0)
   (mtime :accessor mtime
	  :initform 0)
   (ctime :accessor ctime
	  :initform 0)
   (ptr :accessor entries
	:initform nil)))

(defgeneric bind-inode (client inum))

(defgeneric sync-inode (client))

(defgeneric read-content (client offset size))

(defgeneric write-content (client offset size content))

(defgeneric set-block-ptr (client index block-num)
  (:documentation "Set the data-block pointer, 0 means freeing the block"))

(defgeneric get-block-ptr (client index)
  (:documentation "Get the data-block pointer, 0 for not-in-use"))

(defgeneric get-nth-block (client block-index)
  (:documentation "Get the content of nth data-block of the inode, indirect block will be expanded"))

(defgeneric put-nth-block (client block-index content)
  (:documentation "put data-block, this method should manage the indirect block"))


;;; export symbols
(export 'file-client)
(export 'bind-inode)
(export 'sync-inode)
(export 'read-content)
(export 'write-content)

;;; ldb is used to find the nth byte of a number
  
(defun put-int32-to-string (num str pos)
  (loop for i from 0 to 3 do
       (setf (char str (+ pos i)) (code-char (ldb (byte 8 (* i 8)) num))))
  str)

(defun load-int32-from-string (str pos)
  (reduce (lambda (x y)
	    (+ (ash x 8) y))
	  (map 'list (lambda (index) 
		       (char-code (char str (+ pos index))))
	       '(3 2 1 0))))

(defmacro load-slot-value-from-string (client slot-name string-name offset)
  `(setf (slot-value ,client ,slot-name) (load-int32-from-string ,string-name ,offset)))

(defmacro store-slot-value-to-string (client slot-name string-name offset)
  `(put-int32-to-string (slot-value ,client ,slot-name) ,string-name ,offset))


;;; Metadata management functions

(defmethod bind-inode ((client file-client) inum)
  "bind the given inode number with the client and load metadata"
  (setf (inode-num client) inum)
  (let ((block-content (get-block (extent-client client) inum)))
    (load-slot-value-from-string client 'size block-content 0)
    (load-slot-value-from-string client 'atime block-content 4)
    (load-slot-value-from-string client 'ctime block-content 8)
    (load-slot-value-from-string client 'mtime block-content 12)
    (setf (entries client) (map 'list (lambda (offset)
					(load-int32-from-string block-content offset))
				(loop for i from 0 to (1- *num-of-ptr*) 
				   collect (+ (* 4 i) 16))))))

(defmethod sync-inode ((client file-client))
  "sync the inode, store the changes to the server"
  (cond ((eql nil (inode-num client)) nil)
	(t (let ((block-content (make-string *block-size*)))
	     (store-slot-value-to-string client 'size block-content 0)
	     (store-slot-value-to-string client 'atime block-content 4)
	     (store-slot-value-to-string client 'ctime block-content 8)
	     (store-slot-value-to-string client 'mtime block-content 12)
	     (loop for i from 0 to (1- *num-of-ptr*) do
		  (put-int32-to-string (nth i (entries client)) 
				       block-content (+ (* 4 i) 16)))
	     (put-block (extent-client client) (inode-num client) block-content)
	     t))))

;;; Data block management

(defun num-of-direct-block ()
  (- *num-of-ptr* (+ *num-of-l1-indirect* *num-of-l2-indirect*)))

(defun block-ptr-type (index)
  "Returns the type of the index, :direct or :indirect and the index of each level"
  (let ((num-direct (num-of-direct-block)) (num-of-entries (/ *block-size* 4)))
    (cond ((< index num-direct) :direct)
	  ((< index (+ num-direct (* num-of-entries *num-of-l1-indirect*)))
	   (values :indirect 1 (list (+ num-direct (floor (- index num-direct) num-of-entries)) (mod (- index num-direct) num-of-entries))))
	  ((< index (+ num-direct (* num-of-entries *num-of-l2-indirect*) (* (expt num-of-entries 2) *num-of-l2-indirect*)))
	   (let ((entries-per-l2 (expt num-of-entries 2)))
	     (values :indirect 2 
		     (list (+ num-direct 
			      *num-of-l1-indirect* 
			      (floor (- index num-direct (* num-of-entries *num-of-l1-indirect*)) entries-per-l2))
			   (floor (mod (- index num-direct (* num-of-entries *num-of-l1-indirect*)) entries-per-l2) num-of-entries)
			   (mod (- index num-direct (* num-of-entries *num-of-l1-indirect*)) num-of-entries)))))
	  (t nil))))


(defun hop-and-swap (ec block-num hop-list new-val)
  (cond ((eql nil (cdr hop-list)) (let ((content (get-block ec block-num)) (old-val 0))
				    (setf old-val (load-int32-from-string content (* 4 (car hop-list))))
				    (put-int32-to-string new-val content (* 4 (car hop-list)))
				    (put-block ec block-num content)
				    old-val))
	(t (let* ((content (get-block ec block-num)) (next-level (load-int32-from-string content (* 4 (car hop-list)))))
	     (when (eql 0 next-level)
	       (setf next-level (new-block ec))
	       (put-int32-to-string next-level content (* 4 (car hop-list)))
	       (put-block ec block-num content))
	     (hop-and-swap ec next-level (cdr hop-list) new-val)))))

(defmethod set-block-ptr ((client file-client) index block-num)
  (multiple-value-bind (index-type num-level index-list) (block-ptr-type index)
    (let ((old-block-num 
	     (cond ((eql :direct index-type) (let ((old-block-num (nth index (entries client))))
					       (setf (nth index (entries client)) block-num)
					       old-block-num))
		   ((eql :indirect index-type) (progn 
						 (when (eql 0 (nth (car index-list) (entries client)))
						   (setf (nth (car index-list) (entries client)) 
							 (new-block (extent-client client))))
						 (hop-and-swap (extent-client client) 
							       (nth (car index-list) (entries client)) 
							       (cdr index-list) 
							       block-num))))))
      (when (not (eql old-block-num 0))
	(free-block (extent-client client) old-block-num))
      old-block-num)))


(defun hop-and-find (ec block-num hop-list)
  (cond ((eql nil (cdr hop-list)) (let ((content (get-block ec block-num)))
				    (load-int32-from-string content (* 4 (car hop-list)))))
	(t (let ((content (get-block ec block-num)))
	     (hop-and-find ec (load-int32-from-string content (* 4 (car hop-list))) (cdr hop-list))))))

(defmethod get-block-ptr ((client file-client) index)
  (multiple-value-bind (index-type num-level index-list) (block-ptr-type index)
    (cond ((eql :direct index-type) (nth index (entries client)))
	  ((eql :indirect index-type) (hop-and-find (extent-client client) 
						    (nth (car index-list) (entries client)) 
						    (cdr index-list))))))