;;;; The yfs-client class
;;;; should be loaded after the file-client.lisp

(in-package :cl-yfs-clients)

(defclass yfs-client ()
  ((extent-client :accessor extent-client
		  :initform nil
		  :initarg :extent-client)
   (lock-client :accessor lock-client
		:initform nil
		:initarg :lock-client)))

(defmacro with-remote-lock ((client lock-name) &body body)
  `(progn 
     (lock (lock-client ,client) ,lock-name)
     ,@body
     (unlock (lock-client ,client) ,lock-name)))

