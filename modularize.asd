#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem modularize
  :name "Modularize"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A modularization framework"
  :homepage "https://Shinmera.github.io/modularize/"
  :bug-tracker "https://github.com/Shinmera/modularize/issues"
  :source-control (:git "https://github.com/Shinmera/modularize.git")
  :serial T
  :components ((:file "package")
               (:file "package-toolkit")
               (:file "hooks")
               (:file "module")
               (:file "options")
               (:file "asdf")
               (:file "documentation"))
  :depends-on (:documentation-utils))
