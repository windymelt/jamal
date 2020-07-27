(defpackage jamal/tcp
  (:use :cl :cl-async :bordeaux-threads :trivia :trivia.ppcre)
  (:export :start-tcp-server))
(in-package :jamal/tcp)

(defun start-tcp-server (port receive-callback)
  "Starts TCP server. RECEIVE-CALLBACK is called with ID and JSON."
  (format *error-output* "Bootstrapping TCP server...~%")
  (bt:make-thread
   (lambda ()
     (as:start-event-loop
      (lambda ()
        (format *error-output* "Entered Event loop (port ~A).~%" port)
        (let ((*standard-output-m* *standard-output*)
              (*error-output-m* *error-output*))
          (as:tcp-server
           nil;"127.0.0.1"
           port
           #'(lambda (sock stream)
               (declare (ignorable sock))
               (let ((*standard-output* *standard-output-m*)
                     (*error-output* *error-output-m*))
                 ;; Data receive handler.
                 (format *error-output* "RECV~%")
                 (lsp-stream-receive stream receive-callback)))
           :connect-cb #'(lambda (sock)
                           (format *error-output* "Connected!~%"))
           :stream t)))))
   :name "TCP Server Thread"))

;;; Variables

;; Define receive buffer.
;; Initial size is 8192 octets.
;; Can be extended when overflown.
;; Cursor (fill-pointer) is initially 0.
(defparameter *tcp-buf*
  (make-array 8192
              :element-type '(unsigned-byte 8)
              :fill-pointer 0
              :adjustable t))
(deftype tcp-data-state-state ()
  '(member
    :tcp-data-state-receiving-header
    :tcp-data-state-receiving-body))
(alexandria:define-constant
    +R+ "(?>\\r\\n|\\n|\\x0b|\\f|\\r|\\x85)"
  :test #'string=)
(defstruct tcp-data-state
  (state :tcp-data-state-receiving-header :type tcp-data-state-state)
  (header nil)
  (body nil))

;; Singleton
(defparameter *tcp-data-state* (make-tcp-data-state))

;;; Receive function

(defun lsp-stream-receive (stream receive-callback &key (loop-p nil))
  "Stores received data from STREAM and calls method on complete."
  ;; Store data into buffer
  ;; TODO: Acquire lock!
  (loop for octet = (read-byte stream nil :EOT)
        until (eq octet :EOT)
        do (vector-push-extend octet *tcp-buf*)
        (format *error-output* "."))
  (when (> (length *tcp-buf*) 65535)
    ;; Limit buffer size.
    (error "tcp buffer run out"))
  (ecase (tcp-data-state-state *tcp-data-state*)
    (:tcp-data-state-receiving-header
     (tcp-try-to-read-header!))
    (:tcp-data-state-receiving-body
     (tcp-try-to-read-body!)))
  (when (tcp-data-state-body *tcp-data-state*)
    ;; Body is available: everything is fine!
    ;; Calls callback function with JSON decoded data
    (let* ((json (jojo:parse (tcp-data-state-body *tcp-data-state*)))
           (id (if (getf json :|type|)
                   (- 0 (getf json :|seq|))
                   (getf json :|id|))))
      ;; Cleanup -- Reset state
      (setf (tcp-data-state-state *tcp-data-state*)
            :tcp-data-state-receiving-header)
      (setf (tcp-data-state-header *tcp-data-state*)
            nil)
      (setf (tcp-data-state-body *tcp-data-state*)
            nil)
      ;; MAY: Move tcp buffer clearing to here
      ;; Calls callback
      (bt:make-thread #'(lambda () (funcall receive-callback id json)))
      (force-output *error-output*)
      (when loop-p (lsp-stream-receive stream receive-callback :loop-p t)))))

(defun tcp-try-to-read-header! ()
  "Try to read buffer from *TCP-BUF*. If succeed, *TCP-BUF* and *TCP-DATA-STATE* will be modified."
  (let ((buffer-string
          (handler-case
              (babel:octets-to-string *tcp-buf* :errorp t)
            ;; Seems like malformed (incomplete) utf-8. SKIP.
            (error nil))))
    (when buffer-string
      (multiple-value-bind (de to des tos)
          (ppcre:scan (format nil "^(.*?)~A" +R+) buffer-string)
        (declare (ignorable des))
        (when de ;; When matches regex, de is non-nil.
          (if (zerop (length (subseq buffer-string de (elt tos 0))))
              (progn
                ;; Empty lines input.
                ;; <=> Header exhausted
                (setf (tcp-data-state-state *tcp-data-state*)
                      :tcp-data-state-receiving-body)
                ;; Rewind fill pointer
                (setf (fill-pointer *tcp-buf*) 0)
                (format *error-output* "State: End of headers~%"))
              (progn
                ;; Header detected
                ;; 1. Register header into *TCP-DATA-STATE*.
                (push (parse-header (subseq buffer-string de (elt tos 0)))
                      (tcp-data-state-header *tcp-data-state*))
                (format *error-output* "State: Header detected: ~S~%" (tcp-data-state-header *tcp-data-state*))
                ;; 2. Delete header from buffer and rewind buffer.
                ;; XXX: inefficient!
                (let ((original-buf-length (length *tcp-buf*))
                      (header-end-offset
                        (length (babel:string-to-octets
                                 (subseq buffer-string de to)))))
                  ;; Copy body into very beginning of *TCP-BUF*
                  (replace *tcp-buf* *tcp-buf* :start2 header-end-offset)
                  ;; Move fill pointer backward
                  (setf (fill-pointer *tcp-buf*)
                        (- original-buf-length header-end-offset)))
                )))))))

(defun tcp-try-to-read-body! ()
  "Try to read buffer from *TCP_BUF*. If succeed, *TCP-BUF* and *TCP-DATA-STATE* will be modified."
  (let ((content-length
          (parse-integer
           (cdr (assoc "Content-Length"
                       (tcp-data-state-header *tcp-data-state*)
                       :test #'string=)))))
    (if (>= (length *tcp-buf*) content-length)
        ;; Case 1: Entire body included in 1 receive
        ;; <=> resting *TCP-BUF* length >= content-length
        (let ((body (babel:octets-to-string *tcp-buf*
                                            :end content-length)))
          (setf (tcp-data-state-body *tcp-data-state*) body)
          (format *error-output* "Body received: ~S~%" body)
          (setf (fill-pointer *tcp-buf*) 0))
        ;; Case 2: Not received entire body yet
        ;; <=> resting *TCP-BUF* length < content-length
        ;; Keep receiving. NOP.
        nil)))

(defun parse-header (header-string)
  (match header-string
    ((ppcre "(.+?):\\s*(.+)" key val)
     (cons key val))
    (otherwise (error "Broken header format: ~S~%" header-string))))
