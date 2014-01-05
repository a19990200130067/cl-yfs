;;;; The file client will manage the inode abstraction 
;;;; Which includes read/write, and the management of indirect blocks
;;;; This file should be loaded after the rpc-clients.lisp file

;;;; The inode block layout
;;;; +--------------+-------+-------+-------+--------+------+
;;;; | size (32Bit) | atime | ctime | mtime | refcnt | ptrs |
;;;; +--------------+-------+-------+-------+--------+------+

(in-package :cl-yfs-clients)

(defclass file-client ()
  ((extent-client :accessor extent-client
		  :initform nil
		  :initarg :extent-client)
   (inum :accessor inum
	 :initform nil
	 :initarg :inum)))

(defgeneric read-content (client inum offset size))

(defgeneric write-content (client inum offset size content))

(defgeneric get-metadata (client inum))

(defgeneric put-metadata (client inum metadata))


;;; export symbols
(export 'file-client)
(export 'read-content)
(export 'write-content)
(export 'get-metadata)
(export 'put-metadata)


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

