(defpackage jamal/req
  (:use :cl :cl-annot.class))
(in-package :jamal/req)

(annot:enable-annot-syntax)

@export-structure
(defstruct req id params (cancel nil) dap-p type)
