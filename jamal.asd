(defsystem "jamal"
  :version "0.1.0"
  :author "windymelt"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:cl-async :cl-annot :trivia :trivia.ppcre :jonathan :jamal/all)
  :description ""
  :in-order-to ((test-op (test-op "jamal/tests"))))

(defsystem "jamal/tests"
  :author "windymelt"
  :license "MIT"
  :depends-on ("jamal"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for jamal"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(register-system-packages :cl-async '(:as))
(register-system-packages :jonathan '(:jojo))
