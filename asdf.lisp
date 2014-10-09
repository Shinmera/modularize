#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *virtual-module-map* (make-hash-table :test 'equal)
  "Map of virtual module names to the asdf system instances of the virtual modules.")

(defun virtual-module (identifier)
  "Returns the virtual module instance associated with the identifier if one was found, otherwise NIL."
  (gethash (string identifier) *virtual-module-map*))

(defun (setf virtual-module) (module identifier)
  "Associates the given module with the identifier so it may be retrieved later."
  (setf (gethash (string identifier) *virtual-module-map*) module))

(defun remove-virtual-module (identifier)
  "Removes the association of the name with a virtual module."
  (remhash identifier *virtual-module-map*))

(define-condition virtual-module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Virtual module ~s requested but not found." (requested c))))
  (:documentation "Condition raised when a virtual module was requested but could not be found."))

(defclass module (asdf:system)
  ((module-name :initform NIL :initarg :module-name :accessor virtual-module-name))
  (:documentation "ASDF System subclass that serves as the class for virtual modules."))

(defmethod virtual-module-name ((module module))
  "Returns the module name associated with this virtual module."
  (or (or (slot-value module 'module-name)
          (string-upcase (asdf:component-name module)))))

(defun register-virtual-module (module)
  "Registers the given module in the virtual module map."
  (let ((name (string (virtual-module-name module))))
    (setf (virtual-module name) module
          (virtual-module (make-identifier name)) module)))

(defmethod initialize-instance :after ((module module) &key)
  (register-virtual-module module))

(defmethod reinitialize-instance :after ((module module) &key)
  (register-virtual-module module))

(defmethod asdf::resolve-dependency-combination ((module module) (combinator (eql :module)) args)
  "Simple dependency combination resolver for virtual modules so you can write :depends on ((:module name)) in your system definitions."
  (or (virtual-module (first args))
      (error 'virtual-module-not-found :requested (first args))))

(defmethod asdf:operate :around ((op asdf:load-op) (module module) &key)
  "Binds the package to *MODULARIZE-USER* so define-module can be used directly within the file definition."
  (let ((*package* (find-package "MODULARIZE-USER")))
    (call-next-method)))

(defun load-module (identifier)
  "Attempts to find the module named by identifier and load its ASDF system."
  (asdf:load-system (or (virtual-module identifier)
                        (error 'virtual-module-not-found :requested identifier))))
