(require :s-xml-rpc)

(defpackage "CL-YFS-LOCK-SERVER"
  (:use :common-lisp :sb-thread :s-xml-rpc)
  (:export :lock :unlock))

(in-package :cl-yfs-lock-server)

(setf s-xml-rpc::*xml-rpc-package* *package*)

(defparameter *locks* (make-hash-table))
(defparameter *hash-mutex* (make-mutex :name "hash table mutex"))

(defun lock (lock-name)
  (with-mutex (*hash-mutex*)
    (multiple-value-bind (val hasKey) (gethash lock-name *locks*)
      (cond ((or (eql nil hasKey) (eql :free val))
	     (progn (setf (gethash lock-name *locks*) :in-use)
		    t))
	    (t nil)))))

(defun unlock (lock-name)
  (with-mutex (*hash-mutex*) 
    (let ((val (gethash lock-name *locks*)))
      (cond ((eql :in-use val)
	     (progn (setf (gethash lock-name *locks*) :free)
		    t))
	    (t nil)))))

