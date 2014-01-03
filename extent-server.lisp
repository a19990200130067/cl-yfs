(require :s-xml-rpc)

(defpackage "CL-YFS-EXTENT-SERVER"
  (:use :common-lisp :s-xml-rpc :sb-thread)
  )

(in-package :cl-yfs-extent-server)

(defparameter *extent-file-name* "data.raw")
(defparameter *extent-mutex* (make-mutex :name "extent mutex"))

