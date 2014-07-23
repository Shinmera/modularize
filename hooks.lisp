#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *setup-hooks* (make-hash-table :test 'eql))
(defvar *delete-hooks* (make-hash-table :test 'eql))

(defun setup-hook (identifier)
  (gethash identifier *setup-hooks*))

(defun (setf setup-hook) (function identifier)
  (setf (gethash identifier *setup-hooks*) function))

(defun remove-setup-hook (identifier)
  (remhash identifier *setup-hooks*))

(defun call-setup-hooks (package)
  (loop for hook being the hash-values of *setup-hooks*
        do (funcall hook package)))

(defmacro define-setup-hook ((modulevar &optional identifier) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (setup-hook
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
