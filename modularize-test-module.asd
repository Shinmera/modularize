(defsystem modularize-test-module
  :class "modularize:virtual-module"
  :components ((:file "modularize-test-module"))
  :defsystem-depends-on (:modularize)
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test module system for modularize.")

