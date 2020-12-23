(defpackage cl-scm/tests/main
  (:use :cl
        :cl-scm
        :rove))
(in-package :cl-scm/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-scm)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
