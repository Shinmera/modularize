#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *module-storages* (make-hash-table :test 'eql))

(define-condition module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Module ~s requested but not found." (requested c)))))

(defun extract-name (identifier)
  (subseq identifier (1+ (position #\. identifier :from-end T))))

(defun make-identifier (name)
  (format NIL "MODULARIZE.MOD.~a" (string name)))

(defun module (&optional identifier)
  (let ((package
          (etypecase identifier
            (null *package*)
            (string (find-package identifier))
            (symbol (find-package identifier))
            (package identifier))))
    (if (and package (module-p package))
        package
        (error 'module-not-found :requested identifier))))

(defun module-p (object)
  (etypecase object
    (null NIL)
    (package (not (null (gethash object *module-storages*))))
    (string (module-p (find-package object)))
    (symbol (module-p (find-package object)))))

(defun module-storage (module &optional (key NIL k-s))
  (if k-s
      (gethash key (module-storage module))
      (gethash (module module) *module-storages*)))

(defun (setf module-storage) (value module &optional (key NIL k-s))
  (if k-s
      (setf (gethash key (module-storage module)) value)
      (setf (gethash (module module) *module-storages*) value)))

(defun module-storage-remove (module key)
  (remhash key (module-storage module)))

(defun module-identifier (module)
  (module-storage module :identifier))

(defun module-name (module)
  (module-storage module :name))

(defmacro current-module ()
  (module))

(defgeneric expand-option (type package args))

(defmacro define-option-expander (name (package &rest arguments) &body body)
  (let ((args (gensym "ARGS")))
    `(defmethod expand-option ((,(gensym "TYPE") (eql ,(intern (string name) "KEYWORD"))) ,package ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

(defmacro expand-module (name &rest options)
  (let ((package (find-package name)))
    (unless package
      (error "Cannot expand options for ~s: No such package." name))
    `(progn
       ,@(loop for (type . args) in options
               collect (expand-option type package args)))))

(defmacro define-module (name &body options)
  (let ((name (string name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defpackage ,(make-symbol name))
       (modularize :package (find-package ,name) :name ,name)
       (expand-module ,name ,@options))))

(defmacro define-module-extension ((module name) &body options)
  (let ((module (module module))
        (name (string name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew ,name (module-storage ,module :extensions) :test #'string=)
       (pushnew ,(make-identifier name) (module-storage ,module :extensions) :test #'string=)
       ;; Add nicknames so the extension can be IN-PACKAGEd as if it
       ;; were a proper module.
       (extend-package ,module '((:nicknames
                                  ,(make-symbol name)
                                  ,(make-symbol (make-identifier name)))))
       (expand-module ,name ,@options)
       ,module)))

(defun modularize (&key (package *package*) (name (package-name package)))
  (let ((identifier (make-identifier name)))
    ;; Push the identifier onto the nicks list
    (let ((ident-package (find-package identifier)))
      (if ident-package
          (unless (eql package ident-package)
            (error "Cannot modularize: ~a is a taken name by ~a." identifier ident-package))
          (add-package-nickname package identifier)))
    ;; Register on the storage
    (unless (module-p package)
      (setf (gethash package *module-storages*) (make-hash-table :test 'eql)))
    (setf (module-storage package :identifier) identifier
          (module-storage package :name) name)
    (call-modularize-hooks package)
    package))

(defun demodularize (module)
  (remhash (module module) *module-storages*)
  module)

(defun delete-module (module)
  (let* ((package (module module))
         (identifier (module-identifier module))
         (extensions (module-storage package :extensions)))
    (when (and (stringp module) (find module extensions :test #'string=))
      (error "Cannot delete a module extension! Please delete the proper module ~a instead." (module-identifier package)))
    (call-delete-hooks package)
    (demodularize package)
    (unbind-and-delete-package package)
    (values identifier extensions)))

(defun map-modules (function)
  (maphash #'(lambda (key module)
               (declare (ignore key))
               (funcall function module))
           *module-storages*))
