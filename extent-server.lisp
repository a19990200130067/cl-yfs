;;;; The extent server package start with (start-extent-server port) function
;;;; Block size is decided by *block-size* 
;;;; Number of bitmap block is decided by *bitmap-block-number*

(require :s-xml-rpc)

(defpackage "CL-YFS-EXTENT-SERVER"
  (:use :common-lisp :s-xml-rpc :sb-thread)
  (:import-from :s-xml-rpc :start-xml-rpc-server)
  (:export :get_block :put_block :new_block :free_block))

(in-package :cl-yfs-extent-server)

(setf *xml-rpc-package* *package*)

(defparameter *server-name* nil)

(defparameter *extent-file-name* "extent-data.dat")
(defparameter *extent-mutex* (make-mutex :name "extent mutex"))
(defparameter *block-size* 512)

(defparameter *bitmap-block-number* 2)

(defparameter *file-stream* nil)

(defun max-block-num ()
  "Get the max number of blocks"
  (* *bitmap-block-number* *block-size* 8))


;;; file stream open/close wrapper

(defun open-stream (filename)
  (setf *file-stream* 
	(open filename :direction :io :if-exists :overwrite :if-does-not-exist :create)))

(defun close-stream ()
  (close *file-stream*))


(defun block-num->offset (block-num)
  "convert the block-number to the offset of file (file position)"
  (* (+ *bitmap-block-number* block-num) *block-size*))

(defun bitmap-location (block-num)
  "The bitmap location helper function Returns multiple value: block-num, bype-loc, bit-loc"
  (let ((bit-per-block (* 8 *block-size*)))
    (values (floor (/ block-num bit-per-block))
	    (floor (/ (mod block-num bit-per-block) 8))
	    (mod block-num 8))))

(defun is-block-free (block-num)
  (multiple-value-bind (block-loc byte-loc bit-loc) (bitmap-location block-num)
    (file-position *file-stream* (* block-loc *block-size*))
    (let ((str (make-string *block-size*)) (byte 0))
      (read-sequence str *file-stream* :end *block-size*)
      (setf byte (char-code (char str byte-loc)))
      (if (eql 0 (logand byte (ash 1 bit-loc))) t nil))))

(defun occupy-block (block-num)
  "Mark the given block as used"
  (multiple-value-bind (block-loc byte-loc bit-loc) (bitmap-location block-num)
    (file-position *file-stream* (* block-loc *block-size*))
    (let ((str (make-string *block-size*)) (byte 0))
      (read-sequence str *file-stream* :end *block-size*)
      (setf byte (char-code (char str byte-loc)))
      (setf byte (logior byte (ash 1 bit-loc)))
      (setf (char str byte-loc) (code-char byte))
      (file-position *file-stream* (* block-loc *block-size*))
      (write-sequence str *file-stream* :end *block-size*)
      t)))

;;; allocate and deallocate a block
;;; the allocate function will use random number

(defun new_block ()
  "allocate a free block"
  (let ((block-num 0) (count 0) (block-limit (max-block-num)))
    (loop do 
	 (setf block-num (random (max-block-num)))
	 (setf count (1+ count))
	 until (or (eql block-limit count) (is-block-free block-num)))
    (if (is-block-free block-num) 
	(progn (occupy-block block-num) block-num)
	nil)))

(defun free_block (block-num)
  "deallocate a block. T on success, nil on error"
  (multiple-value-bind (block-loc byte-loc bit-loc) (bitmap-location block-num)
    (file-position *file-stream* (* block-loc *block-size*))
    (let ((str (make-string *block-size*)) (byte 0))
      (read-sequence str *file-stream* :end *block-size*)
      (setf byte (char-code (char str byte-loc)))
      (setf byte (logxor (logior byte (ash 1 bit-loc)) (ash 1 bit-loc)))
      (setf (char str byte-loc) (code-char byte))
      (file-position *file-stream* (* block-loc *block-size*))
      (write-sequence str *file-stream* :end *block-size*)
      t)))

;;; Getting and putting data from/to a block with given block number

(defun get_block (block-num)
  (let ((str (make-string *block-size*)))
    (file-position *file-stream* (block-num->offset block-num))
    (read-sequence str *file-stream* :end *block-size*)
    str))

(defun put_block (block-num str)
  (file-position *file-stream* (block-num->offset block-num))
  (write-sequence str *file-stream* :end (min (length str) *block-size*)))


;;; The start-extent-server and stop-extent-server function
;;; Use these two to manage the rpc server

(defun start-extent-server (port)
  (open-stream *extent-file-name*)
  (setf *server-name* (start-xml-rpc-server :port port)))

(defun stop-extent-server ()
  (s-xml-rpc:stop-server *server-name*)
  (close-stream))