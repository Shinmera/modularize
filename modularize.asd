#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem modularize
  :name "Modularize"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A modularization framework"
  :homepage "https://github.com/Shinmera/modularize"
  :serial T
  :components ((:file "package")
               (:file "package-toolkit")
               (:file "hooks")
               (:file "module")
               (:file "options")
               (:file "asdf")
               (:file "documentation"))
  :depends-on (:documentation-utils))
