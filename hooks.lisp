#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *modularize-hooks* (make-hash-table :test 'eql)
  "Table mapping the modularization hook names to their functions.")
(defvar *delete-hooks* (make-hash-table :test 'eql)
  "Table mapping the deletion hook names to their functions.")

(defun modularize-hook (identifier)
  "Returns the modularization hook function associated with the identifier or NIL."
  (gethash identifier *modularize-hooks*))

(defun (setf modularize-hook) (function identifier)
  "Associates the identifier with the given modularization hook function."
  (setf (gethash identifier *modularize-hooks*) function))

(defun remove-modularize-hook (identifier)
  "Removes the modularization hook named by the identifier."
  (remhash identifier *modularize-hooks*))

(defun call-modularize-hooks (package)
  "Calls all modularization hooks on the package."
  (loop for hook being the hash-values of *modularize-hooks*
        do (funcall hook package)))

(defmacro define-modularize-hook ((modulevar &optional identifier) &body body)
  "Defines a new modularization hook. 
The identifier is defaulted to a keyword representation of the current package name."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (modularize-hook
            (or ,identifier
                (intern (package-name *package*) "KEYWORD")))
           #'(lambda (,modulevar) ,@body))))

(defun delete-hook (identifier)
  "Returns the deletion hook function associated with the identifier or NIL."
  (gethash identifier *delete-hooks*))

(defun (setf delete-hook) (function identifier)
  "Associates the identifier with the given deletion hook function."
  (setf (gethash identifier *delete-hooks*) function))

(defun remove-delete-hook (identifier)
  "Removes the deletion hook named by the identifier."
  (remhash identifier *delete-hooks*))

(defun call-delete-hooks (package)
  "Calls all deletion hooks on the package."
  (loop for hook being the hash-values of *delete-hooks*
        do (funcall hook package)))

(defmacro define-delete-hook ((modulevar &optional identifier) &body body)
  "Defines a new deletion hook. 
The identifier is defaulted to a keyword representation of the current package name."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (delete-hook
            (or ,identifier
                (intern (package-name *package*) "KEYWORD")))
           #'(lambda (,modulevar) ,@body))))
