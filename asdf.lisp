#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defvar *virtual-module-map* (make-hash-table :test 'equal)
  "")

(defun virtual-module (identifier)
  (gethash (string identifier) *virtual-module-map*))

(defun (setf virtual-module) (module identifier)
  (setf (gethash (string identifier) *virtual-module-map*) module))

(define-condition virtual-module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Virtual module ~s requested but not found." (requested c)))))

(defclass module (asdf:system)
  ((module-name :initform NIL :initarg :module-name :accessor virtual-module-name))
  (:documentation ""))

(defmethod virtual-module-name ((module module))
  (or (or (slot-value module 'module-name)
          (string-upcase (asdf:component-name module)))))

(defun register-virtual-module (module)
  (let ((name (string (virtual-module-name module))))
    (setf (virtual-module name) module
          (virtual-module (make-identifier name)) module)))

(defmethod initialize-instance :after ((module module) &key)
  (register-virtual-module module))

(defmethod reinitialize-instance :after ((module module) &key)
  (register-virtual-module module))

(defmethod asdf::resolve-dependency-combination ((module module) (combinator (eql :module)) args)
  (or (virtual-module (first args))
      (error 'virtual-module-not-found :requested (first args))))

(defmethod asdf:operate :around ((op asdf:load-op) (module module) &key)
  (let ((*package* (find-package "MODULARIZE-USER")))
    (call-next-method)))

(defun load-module (identifier)
  (asdf:load-system (or (virtual-module identifier)
                        (error 'virtual-module-not-found :requested identifier))))
