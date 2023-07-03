(in-package #:org.shirakumo.radiance.lib.modularize)

(defvar *virtual-module-map* (make-hash-table :test 'equal))

(defmethod virtual-module (identifier)
  (gethash (string identifier) *virtual-module-map*))

(defmethod virtual-module ((identifier package))
  (virtual-module (module-name identifier)))

(defmethod (setf virtual-module) (module identifier)
  (setf (gethash (string identifier) *virtual-module-map*) module))

(defmethod (setf virtual-module) (module (identifier package))
  (setf (virtual-module (module-name identifier)) module))

(defmethod remove-virtual-module (identifier)
  (remhash identifier *virtual-module-map*))

(defmethod remove-virtual-module ((identifier package))
  (remove-virtual-module (module-name identifier)))

(define-condition virtual-module-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Virtual module ~s requested but not found." (requested c)))))

(defclass virtual-module (asdf:system)
  ((module-name :initform NIL :initarg :module-name :accessor virtual-module-name)))

;; Backwards compat.
(defclass module (virtual-module)
  ())

(defmethod virtual-module-name ((virtual-module virtual-module))
  (or (slot-value virtual-module 'module-name)
      (string-upcase (asdf:component-name virtual-module))))

(defun register-virtual-module (module)
  (let ((name (string (virtual-module-name module))))
    (setf (virtual-module name) module
          (virtual-module (make-identifier name)) module)))

(defmethod initialize-instance :after ((virtual-module virtual-module) &key)
  (register-virtual-module virtual-module))

(defmethod reinitialize-instance :after ((virtual-module virtual-module) &key)
  (register-virtual-module virtual-module))

(defmethod asdf::resolve-dependency-combination ((virtual-module virtual-module) (combinator (eql :module)) args)
  (or (virtual-module (first args))
      (error 'virtual-module-not-found :requested (first args))))

(defun load-module (identifier)
  (asdf:load-system (or (virtual-module identifier)
                        (error 'virtual-module-not-found :requested identifier))))
