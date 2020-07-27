(defpackage jamal/methods
  (:use :cl)
  (:export :test))
(in-package :jamal/methods)

(defun rpcreq-test (ws req)
  (list ws req "ok"))

(defun rpcnotreq-test (ws req)
  (list ws req "ok"))
