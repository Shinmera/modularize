#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *modularize-hooks* (make-hash-table :test 'eql))
(defvar *delete-hooks* (make-hash-table :test 'eql))

(defun modularize-hook (identifier)
  (gethash identifier *modularize-hooks*))

(defun (setf modularize-hook) (function identifier)
  (setf (gethash identifier *modularize-hooks*) function))

(defun remove-modularize-hook (identifier)
  (remhash identifier *modularize-hooks*))

(defun call-modularize-hooks (package)
  (loop for hook being the hash-values of *modularize-hooks*
        do (funcall hook package)))

(defmacro define-modularize-hook ((modulevar &optional identifier) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (modularize-hook
            (or ,identifier
                (intern (package-name *package*) "KEYWORD")))
           #'(lambda (,modulevar) ,@body))))

(defun delete-hook (identifier)
  (gethash identifier *delete-hooks*))

(defun (setf delete-hook) (function identifier)
  (setf (gethash identifier *delete-hooks*) function))

(defun remove-delete-hook (identifier)
  (remhash identifier *delete-hooks*))

(defun call-delete-hooks (package)
  (loop for hook being the hash-values of *delete-hooks*
        do (funcall hook package)))

(defmacro define-delete-hook ((modulevar &optional identifier) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (delete-hook
            (or ,identifier
                (intern (package-name *package*) "KEYWORD")))
           #'(lambda (,modulevar) ,@body))))
