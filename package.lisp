#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize
  (:use #:cl)
  (:nicknames #:modularize #:radiance-modularize)
  ;; module.lisp
  (:export
   #:module
   #:module-p
   #:module-storage
   #:module-storage-remove
   #:module-identifier
   #:module-name
   #:current-module
   #:define-module
   #:define-module-extension
   #:modularize
   #:demodularize
   #:module-cleanup
   #:remove-module
   #:do-modules)
  ;; asdf.lisp
  (:export
   #:module
   #:virtual-module-name
   #:virtual-module
   #:virtual-module-not-found
   #:register-virtual-module
   #:load-module))
