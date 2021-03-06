(require :s-xml-rpc)
(require :cffi)

(defpackage "CL-YFS-CLIENTS"
  (:use :common-lisp :sb-thread :s-xml-rpc :cffi)
  (:export :rpc-client 
	   :lock-client :lock :unlock
	   :extent-client :get-block :put-block :new-block :free-block))

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


;;; Generic methods

(defgeneric rpc-call (client encoded-rpc)
  (:documentation "call the encoded rpc with given client"))

(defgeneric lock (client lock-num)
  (:documentation "Lock the given lock-num"))

(defgeneric unlock (client lock-num)
  (:documentation "Unlock the given lock-num"))

(defgeneric get-block (client block-num)
  (:documentation "get the block content"))

(defgeneric put-block (client block-num content)
  (:documentation "put the content to the given block-num, the content should have length <= *block-size*, will overwrite the old contents but not clear the entire block"))

(defgeneric new-block (client))

(defgeneric free-block (client block-num))


;;; The implementation of generic methods

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
