(defpackage jamal/util
  (:use :cl :trivia :trivia.ppcre)
  (:export :parse-method))
(in-package :jamal/util)

(defun parse-method (method)
  (match method
    ((ppcre "^(\\w+)/(\\w+)$" module name) (values module name))
    ((ppcre "^(\\w+)$" name) (values nil name))
    ((ppcre "^\\$(\\w+)$" name) (values nil name))
    (otherwise (error "broken method name: ~S" method))))
