;;;; The file client will manage the inode abstraction 
;;;; Which includes read/write, and the management of indirect blocks
;;;; This file should be loaded after the rpc-clients.lisp file

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