#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(define-module test-module
  (:use #:cl #:modularize)
  (:export #:greet))

(in-package #:modularize.mod.test-module)

(defun greet ()
  (format T "~&Hello from ~a!~%" (current-module)))

(greet)