#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize)

(defvar *module-storages* (make-hash-table :test 'eql)
  "Table mapping module packages to their storage tables.")

(define-condition module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Module ~s requested but not found." (requested c))))
  (:documentation "Condition raised when a module is requested but not found."))

(define-condition not-a-module (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Module ~s requested but while the package exists, it is not a module." (requested c))))
  (:documentation "Condition raised when a module is requested but only a package of the requested name exists."))

(defun extract-name (identifier)
  "Extracts the name from an identifier, which is to say the string after the last dot."
  (subseq identifier (1+ (position #\. identifier :from-end T))))

(defun make-identifier (name)
  "Turns a name into an identifier by prepending MODULARIZE.MOD. to it."
  (format NIL "MODULARIZE.MOD.~a" (string name)))

(defun module-p (object)
  "Returns T if the passed object is or resolves to a module package, otherwise NIL."
  (typecase object
    (package (not (null (gethash object *module-storages*))))
    (string (module-p (find-package object)))
    (symbol (module-p (find-package object)))
    (T NIL)))

(defun module (&optional identifier)
  "Attempts to return the matching module package.
If not possible, a condition of type MODULE-NOT-FOUND is raised.

You may pass a STRING, SYMBOL or PACKAGE as the identifier.
If NIL is passed, the current *PACKAGE* is used instead."
  (with-package (package (or identifier *package*))
    (cond ((not package)
           (error 'module-not-found :requested identifier))
          ((not (module-p package))
           (error 'not-a-module :requested package))
          (T package))))

(defmacro with-module ((var &optional (module var)) &body body)
  "Binds the resolved MODULE to VAR."
  `(let ((,var (module ,module)))
     ,@body))

(defun module-storage (module &optional (key NIL k-s))
  "Returns the module storage table of the module or a field from it if a key is passed."
  (if k-s
      (gethash key (module-storage module))
      (gethash (module module) *module-storages*)))

(defun (setf module-storage) (value module &optional (key NIL k-s))
  "Sets either the module storage table directly or a field on it if a key is passed."
  (if k-s
      (setf (gethash key (module-storage module)) value)
      (setf (gethash (module module) *module-storages*) value)))

(defun module-storage-remove (module key)
  "Removes a key from the module storage table."
  (remhash key (module-storage module)))

(defun module-identifier (module)
  "Returns the identifier of the module."
  (module-storage module :identifier))

(defun module-name (module)
  "Returns the name of the module."
  (module-storage module :name))

(defmacro current-module ()
  "Macro that expands to the module in the current package.
Useful to establish a module context."
  `(find-package ,(package-name (module))))

(defgeneric expand-option (type package args)
  (:documentation "Called to expand module options into forms."))

(defmacro define-option-expander (name (package &rest arguments) &body body)
  "Defines a new option expander that is called whenever the option is used in the module definition.
This should run whatever is necessary to accomplish the desired option effect. The expanders are run
AFTER the modularize call, so you can use the module storage in your expansions."
  (let ((args (gensym "ARGS")))
    `(defmethod expand-option ((,(gensym "TYPE") (eql ,(intern (string name) "KEYWORD"))) ,package ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

(defun expand-module (package options)
  "Expands the module options into their proper forms using EXPAND-OPTION for each."
  (with-package (package)
    (loop for (type . args) in options
          do (expand-option type package args))))

(defmacro define-module (name &body options)
  "Defines a new module.

This essentially defines a new package with the given name,
calls MODULARIZE on it and then expands all options to extend
the package/module."
  (let ((name (string name))
        (package (gensym "PACKAGE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,package (or (find-package ,name)
                           (make-package ,name :use ()))))
         (modularize :package ,package :name ,name)
         (expand-module ,name ',options)
         ,package))))

(defmacro define-module-extension ((module-identifier name) &body options)
  "Defines a module extension.

This gives the existing module new nicknames and expands the
given options on it to add functionality."
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
  "Turns the given package into one that is identified as a module.

What this does is register the package on the module storage table,
add the name and identifiers to it, and then call the modularize hooks."
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
  "Removes the module from the module storage table, essentially
returning it back to a normal package. Any additional effects by
module options or modularization hooks can of course not be undone
by this."
  (remhash (module module) *module-storages*)
  module)

(defun delete-module (module)
  "Attempts to completely delete the given module.

This first calls the delete hooks, then demodularizes the package,
 unbinds all symbols in the package from values and functions, and
finally deletes the package."
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
  "Calls the function once with each module in no particular order."
  (maphash #'(lambda (module storage)
               (declare (ignore storage))
               (funcall function module))
           *module-storages*))

(defun list-modules ()
  "Returns a list of all modules in no particular order."
  (let ((modules ()))
    (map-modules #'(lambda (module) (push module modules)))
    modules))
