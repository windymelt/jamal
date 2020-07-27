(defpackage jamal/tests/main
  (:use :cl
        :jamal
        :rove))
(in-package :jamal/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :jamal)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
