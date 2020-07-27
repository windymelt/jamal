(defpackage jamal/core
  (:use :cl))
(in-package :jamal/core)

(defparameter *workspace* nil)

(defun call-rpc-method (&key req method id)
  "Calls RPC method. Methods are defined in JAMAL/METHODS."
  (multiple-value-bind (module name) (jamal/util:parse-method method)
    (let* ((method-base-package "jamal/methods")
           (method-lisp-package-designator (string-upcase
                                            (if module
                                                (format nil
                                                        "~A/~A"
                                                        method-base-package
                                                        module)
                                                method-base-package)))
           (method-lisp-package (find-package method-lisp-package-designator))
           (method-name (string-upcase
                         (if id
                             (format nil "rpcreq-~A" name)
                             (format nil "rpcnotreq-~A" name)))))
      (unless method-lisp-package
        (error "method package not found: ~S" method-lisp-package-designator))
      (let ((func (find-symbol method-name method-lisp-package)))
        (unless func
          (error "couldn't find method: ~S in ~S" method-name method-lisp-package))
        (funcall func *workspace* req)))))

(defun process-req (&key id req-data)
  ;; TODO: register running process as job by ID
  ;; XXX: synchronous run now
  (let* ((req-type (getf req-data :|type|))
         (dap-p (not (null req-type)))
         (req-type-detail (and req-type (if id "request" "notification")))

         (params (if dap-p (getf req-data :|arguments|) (getf req-data :|params|)))
         (req jamal/req:make-req :id id :dap-p dap-p :type req-type-detail :params params))
    ;; TODO: register req as running req by id
    (let ((out-json
            (handler-case
                (let* ((res (call-rpc-method
                             :req req
                             :method (if dap-p
                                         (getf req-data :|command|)
                                         (getf req-data :|method|))
                             :id id))
                       (id (if res id nil)))
                  (if dap-p
                      `(:|request_seq| ,(- 0 id)
                         :|seq| ,(- 0 id)
                        :|command| ,(getf req-data :|command|)
                         :|success| t
                        :|type| "response"
                        ,@(when res (list :|body| res)))
                      `(:|id| ,id
                         :|jsonrpc| "2.0"
                         ,@(when res (list :|result| res)))))
              (error (c) (if dap-p
                             `(:|request_seq| ,(- 0 id)
                                :|command| ,(getf req-data :|command|)
                               :|success| :false
                                :|message| ,(format nil "~A" c)
                               :|type| "response")
                             `(:|id| ,id
                                :|jsonrpc| "2.0"
                               :|error| (:|code| -32001 :|message| ,(format nil "~A" c)))))))
          (out-data (jojo:to-json out-json)))
      (when id
        ;; TODO: acquire output semaphore lock
        ;; TODO: build header
        ;; TODO: write to socket
        ;; TODO: release lock
        )
      )
    )
  ;; TODO: yield thread
  )

