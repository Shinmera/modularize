(defsystem modularize
  :name "Modularize"
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A modularization framework"
  :homepage "https://shinmera.com/docs/modularize/"
  :bug-tracker "https://shinmera.com/project/modularize/issues"
  :source-control (:git "https://shinmera.com/project/modularize.git")
  :serial T
  :components ((:file "package")
               (:file "package-toolkit")
               (:file "hooks")
               (:file "module")
               (:file "options")
               (:file "asdf")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-package-local-nicknames))
