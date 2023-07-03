(modularize:define-module modularize-test-module
  (:use #:cl #:modularize)
  (:export #:greet))

(in-package #:modularize-test-module)

(defun greet ()
  (format T "~&Hello from ~a!~%" (current-module)))

(greet)
