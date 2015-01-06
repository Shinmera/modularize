#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:modularize
  (:use #:cl)
  (:nicknames #:org.shirakumo.radiance.lib.modularize #:radiance-modularize)
  ;; asdf.lisp
  (:export
   #:module
   #:virtual-module
   #:remove-virtual-module
   #:virtual-module-name
   #:virtual-module-not-found
   #:register-virtual-module
   #:load-module)
  ;; hooks.lisp
  (:export
   #:modularize-hook
   #:remove-modularize-hook
   #:call-modularize-hooks
   #:define-modularize-hook
   #:delete-hook
   #:remove-delete-hook
   #:call-delete-hooks
   #:define-delete-hook)
  ;; module.lisp
  (:export
   #:module-not-found
   #:not-a-module
   #:module-p
   #:module
   #:with-module
   #:module-storage
   #:module-storage-remove
   #:module-identifier
   #:module-name
   #:current-module
   #:define-option-expander
   #:expand-option
   #:define-module
   #:define-module-extension
   #:modularize
   #:demodularize
   #:delete-module
   #:map-modules
   #:list-modules))

(defpackage #:org.shirakumo.radiance.lib.modularize.user
  (:use #:cl #:modularize)
  (:nicknames #:modularize-user))
