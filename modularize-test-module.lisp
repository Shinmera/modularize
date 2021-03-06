#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(modularize:define-module modularize-test-module
  (:use #:cl #:modularize)
  (:export #:greet))

(in-package #:modularize-test-module)

(defun greet ()
  (format T "~&Hello from ~a!~%" (current-module)))

(greet)
