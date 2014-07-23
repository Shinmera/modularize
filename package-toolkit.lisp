#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defun add-package-nickname (package nickname)
  (let ((nicks (package-nicknames package)))
    (unless (member nickname nicks :test #'string=)
      (rename-package package (package-name package)
                      (cons nickname nicks)))))

(defun collect-symbols-from (package symbols)
  (loop with importing-package = (find-package package)
        for symbol in symbols
        collect (find-symbol (symbol-name symbol)
                             importing-package)))

(defun extend-package (package definition-options)
  (setf package
        (etypecase package
          (package package)
          (string (find-package package))
          (symbol (find-package package))))
  (assert (not (null package)))
  (loop for (option . args) in definition-options
        do (ecase option
             (:nicknames
              (loop for nick in args do (add-package-nickname package nick))) 
             (:documentation
              (setf (documentation package T) (format NIL "~a -- ~a" (documentation package T) (car args))))
             (:use
              (use-package args package))
             (:shadow
              (shadow args package))
             (:shadowing-import-from
              (shadowing-import (collect-symbols-from (car args) (cdr args)) package))
             (:import-from
              (import (collect-symbols-from (car args) (cdr args)) package))
             (:export
              (export args package))
             (:intern
              (loop for symbol in args do (intern (symbol-name symbol) package)))
             (:size
              (error "SIZE option not applicable to EXTEND-PACKAGE.")))))
