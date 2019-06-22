#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem modularize-test-module
  :class "modularize:virtual-module"
  :components ((:file "modularize-test-module"))
  :defsystem-depends-on (:modularize)
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test module system for modularize.")

