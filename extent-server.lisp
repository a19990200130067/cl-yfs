;;;; The extent server package start with (start-extent-server port) function
;;;; Block size is decided by *block-size* 
;;;; Number of bitmap block is decided by *bitmap-block-number*

(require :s-xml-rpc)

(defpackage "CL-YFS-EXTENT-SERVER"
  (:use :common-lisp :s-xml-rpc :sb-thread)
  (:import-from :s-xml-rpc :start-xml-rpc-server)
  (:export :get_block :put_block :new_block :delete_block))

(in-package :cl-yfs-extent-server)

(setf *xml-rpc-package* *package*)

(defparameter *server-name* nil)

(defparameter *extent-file-name* "extent-data.dat")
(defparameter *extent-mutex* (make-mutex :name "extent mutex"))
(defparameter *block-size* 512)

(defparameter *bitmap-block-number* 2)

(defparameter *file-stream* nil)


;;; file stream open/close wrapper

(defun open-stream (filename)
  (setf *file-stream* 
	(open filename :direction :io :if-exists :overwrite :if-does-not-exist :create)))

(defun close-stream ()
  (close *file-stream*))


(defun block-num->offset (block-num)
  "convert the block-number to the offset of file (file position)"
  (* (+ *bitmap-block-number* block-num) *block-size*))


;;; allocate and deallocate a block
;;; the allocate function will use random number

(defun new_block ()
  "allocate a free block"
  nil)

(defun delete_block (block-num)
  "deallocate a block. T on success, nil on error"
  nil)

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