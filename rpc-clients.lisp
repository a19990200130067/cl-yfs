(require :s-xml-rpc)

(defpackage "CL-YFS-CLIENTS"
  (:use :common-lisp :sb-thread :s-xml-rpc)
  (:export :rpc-client :lock :unlock))

(in-package :cl-yfs-clients)

(defclass rpc-client ()
  ((host :accessor host-name
	 :initform "localhost"
	 :initarg :host)
   (port :accessor port
	 :initform 8080
	 :initarg :port)))

(defclass lock-client (rpc-client) ())
(defclass extent-client (rpc-client) ())


(defmethod rpc-call ((rpcc rpc-client) encoded)
  (s-xml-rpc:xml-rpc-call encoded
			  :host (host-name rpcc) :port (port rpcc)))

(defmacro encode-rpc-call (call-name &rest args)
  `(s-xml-rpc:encode-xml-rpc-call ,call-name ,@args))

(defmethod lock ((lc lock-client) lock-num)
  (loop until (string= (rpc-call lc (encode-rpc-call "LOCK" lock-num)) "T"))
  t)

(defmethod unlock ((lc lock-client) lock-num)
  (rpc-call lc (encode-rpc-call "UNLOCK" lock-num))
  t)


(defmethod get-block ((ec extent-client) block-num)
  (rpc-call ec (encode-rpc-call "GET_BLOCK" block-num)))

(defmethod put-block ((ec extent-client) block-num content)
  (rpc-call ec (encode-rpc-call "PUT_BLOCK" block-num content)))

(defmethod new-block ((ec extent-client))
  (rpc-call ec (encode-rpc-call "NEW_BLOCK")))

(defmethod free-block ((ec extent-client) block-num)
  (rpc-call ec (encode-rpc-call "FREE_BLOCK" block-num))
  t)