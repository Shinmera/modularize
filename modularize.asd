#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.modularize.asdf)

(defsystem modularize
  :name "Modularize"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A modularization framework"
  :long-description ""
  :serial T
  :components ((:file "package")
               (:file "package-toolkit")
               (:file "module")
               (:file "asdf"))
  :depends-on ())
