#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *module-storages* (make-hash-table :test 'eql))

(defun extract-name (identifier)
  (subseq identifier (1+ (position #\. identifier :from-end T))))

(defun make-identifier (name)
  (format NIL "MODULARIZE.MOD.~a" (string name)))

(defun module (&optional identifier)
  (let ((package
          (etypecase identifier
            (string (find-package identifier))
            (symbol (find-package identifier))
            (package identifier)
            (null *package*))))
    (if (and package (module-p package))
        package
        (error "No module found."))))

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

(defmacro define-module (name &body options)
  (let ((name (string name))
        (package (gensym "PACKAGE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defpackage ,(make-symbol name)
         ,@options)
       (let ((,package (find-package ,name)))
         (modularize :package ,package :name ,name)
         (call-setup-hooks ,package)))))

(defmacro define-module-extension ((module name) &body options)
  (let ((module (module module))
        (name (string name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (extend-package ,module ',options)
       ;; Add nicknames so the extension can be IN-PACKAGEd as if it
       ;; were a proper module.
       (extend-package ,module '(:nicknames
                                 ,(make-symbol name)
                                 ,(make-symbol (make-identifier name)))))))

(defun modularize (&key (package *package*) (name (package-name package)))
  (let ((table (make-hash-table :test 'eql))
        (identifier (make-identifier name)))
    ;; Push the identifier onto the nicks list
    (let ((ident-package (find-package identifier)))
      (if ident-package
          (unless (eql package ident-package)
            (error "Cannot modularize: ~a is a taken name by ~a." identifier ident-package))
          (add-package-nickname package identifier)))
    ;; Register on the storage
    (setf (gethash :identifier table) identifier
          (gethash :name table) name
          (gethash package *module-storages*) table)
    package))

(defun demodularize (module)
  (remhash (module module) *module-storages*)
  module)

(defun delete-module (module)
  (let* ((module (module module))
         (identifier (module-identifier module)))
    (call-delete-hooks module)
    (demodularize module)
    (delete-package module)
    identifier))

(defmacro do-modules ((var &optional result-form) &body body)
  `(loop for ,var being the hash-keys of *module-storages*
         do (progn ,@body)
         finally (return ,result-form)))
