#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(define-module modularize-test-module
  (:use #:cl #:modularize)
  (:export #:greet))

(in-package #:modularize-test-module)

(defun greet ()
  (format T "~&Hello from ~a!~%" (current-module)))

(greet)
