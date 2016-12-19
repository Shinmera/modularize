#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize)

(defvar *module-storages* (make-hash-table :test 'eql))
(defvar *module-deferrals* (make-hash-table :test 'equal))

(define-condition module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Module ~s requested but not found." (requested c)))))

(define-condition not-a-module (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Module ~s requested but while the package exists, it is not a module." (requested c)))))

(defun extract-name (identifier)
  (subseq identifier (1+ (position #\. identifier :from-end T))))

(defun make-identifier (name)
  (format NIL "MODULARIZE.MOD.~a" (string name)))

(defun resolve-module (object)
  (typecase object
    (null NIL)
    (package (loop for package = object then next
                   for next = (gethash (package-name package) *module-deferrals*)
                   do (when (or (null next) (eql package next))
                        (return next))))
    (string (resolve-module (find-package object)))
    (symbol (resolve-module (find-package object)))))

(defun module-p (object)
  (typecase object
    (null NIL)
    (package (resolve-module object))
    (string (module-p (find-package object)))
    (symbol (module-p (find-package object)))
    (T NIL)))

(defun module (&optional identifier)
  (with-package (package (or identifier *package*))
    (cond ((not package)
           (error 'module-not-found :requested identifier))
          (T
           (or (resolve-module package)
               (error 'not-a-module :requested package))))))

(defmacro with-module ((var &optional (module var)) &body body)
  `(let ((,var (module ,module)))
     ,@body))

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

(defun module-packages (module)
  (module-storage module 'packages))

(defun (setf module-packages) (packages module)
  (let ((new-names (loop for package in packages
                         for name = (etypecase package
                                      ((or string symbol) (string package))
                                      (package (package-name package)))
                         do (when (module-p name)
                              (error "Cannot add ~s as a package to ~s as it is already a module."
                                     name module))
                            (when (gethash name *module-deferrals*)
                              (error "Cannot add ~s as a package to ~s as it is already registered as a package for ~s."
                                     name module (resolve-module name)))
                         unless (string= name (package-name module))
                         collect name))
        (old-names (module-packages module)))
    (dolist (name old-names)
      (remhash name *module-deferrals*))
    (dolist (name new-names)
      (setf (gethash name *module-deferrals*) module))
    (setf (module-storage module 'packages) new-names)
    packages))

(defmacro current-module ()
  `(find-package ,(package-name (module))))

(defgeneric expand-option (type package args))

(defmacro define-option-expander (name (package &rest arguments) &body body)
  (let ((args (gensym "ARGS")))
    `(defmethod expand-option ((,(gensym "TYPE") (eql ,(intern (string name) "KEYWORD"))) ,package ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

(defun expand-module (package options)
  (with-package (package)
    (loop for (type . args) in options
          do (expand-option type package args))))

(defmacro define-module (name &body options)
  (let ((name (string name))
        (package (gensym "PACKAGE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,package (or (find-package ,name)
                           (make-package ,name :use ()))))
         (modularize :package ,package :name ,name)
         (expand-module ,name ',options)
         ,package))))

(defmacro define-module-extension ((module-identifier name) &body options)
  (let ((module (gensym "MODULE"))
        (name (string name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (with-module (,module ,module-identifier)
         (pushnew ,name (module-storage ,module :extensions) :test #'string=)
         (pushnew ,(make-identifier name) (module-storage ,module :extensions) :test #'string=)
         ;; Add nicknames so the extension can be IN-PACKAGEd as if it
         ;; were a proper module.
         (extend-package ,module '((:nicknames
                                    ,(make-symbol name)
                                    ,(make-symbol (make-identifier name)))))
         (expand-module ,module ',options)
         ,module))))

(defun modularize (&key (package *package*) (name (package-name package)))
  (let ((identifier (make-identifier name)))
    ;; Push the identifier onto the nicks list
    (let ((ident-package (find-package identifier)))
      (if ident-package
          (unless (eql package ident-package)
            (error "Cannot modularize: ~a is a taken name by ~a." identifier ident-package))
          (add-package-nickname package identifier)))
    ;; Register on the storage
    (unless (gethash package *module-storages*)
      (setf (gethash (package-name package) *module-deferrals*) package)
      (setf (gethash package *module-storages*) (make-hash-table :test 'eql)))
    (setf (module-storage package :identifier) identifier
          (module-storage package :name) name)
    (call-modularize-hooks package)
    package))

(defun demodularize (module)
  (let ((module (module module)))
    (setf (module-packages module) NIL)
    (remhash (package-name module) *module-deferrals*)
    (remhash module *module-storages*)
    module))

(defun delete-module (module)
  (let* ((package (module module))
         (identifier (module-identifier module))
         (extensions (module-storage package :extensions)))
    (when (and (stringp module) (find module extensions :test #'string=))
      (error "Cannot delete a module extension! Please delete the proper module ~a instead." (module-identifier package)))
    (call-delete-hooks package)
    (dolist (package (module-storage module 'packages))
      (unbind-and-delete-package package))
    (demodularize package)
    (unbind-and-delete-package package)
    (values identifier extensions)))

(defun map-modules (function)
  (maphash (lambda (module storage)
             (declare (ignore storage))
             (funcall function module))
           *module-storages*))

(defun list-modules ()
  (let ((modules ()))
    (map-modules (lambda (module) (push module modules)))
    modules))
