#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defun dump-package (package)
  "Creates a form that is dumpable in macro-expansions and will evaluate to the given PACKAGE."
  `(ensure-package ,(package-name (ensure-package package))))

(defmacro with-package-dump-binding ((var &optional (package var)) &body body)
  "Creates a binding form for PACKAGE with the gensym for the package bound to VAR."
  `(let ((,var (gensym "PACKAGE")))
     `(let ((,,var ,(dump-package ,package)))
        ,,@body)))

(define-option-expander nicknames (package &rest nicknames)
  (with-package-dump-binding (package)
    `(progn
       ,@(loop for nick in nicknames
               collect `(add-package-nickname ,package ',nick)))))

(define-option-expander documentation (package documentation)
  `(setf (documentation ,(dump-package package) T)
         ,documentation))

(define-option-expander use (package &rest packages)
  `(use-package ',packages
                ,(dump-package package)))

(define-option-expander shadow (package &rest symbols)
  `(shadow ',symbols
           ,(dump-package package)))

(define-option-expander shadowing-import-from (package import-package &rest symbols)
  `(shadowing-import (collect-symbols-from ',import-package ',symbols)
                     ,(dump-package package)))

(define-option-expander import-from (package import-package &rest symbols)
  `(import (collect-symbols-from ',import-package ',symbols)
           ,(dump-package package)))

(define-option-expander export (package &rest symbols)
  (with-package-dump-binding (package)
    `(export (mapcar #'(lambda (s) (intern (string s) ,package)) ',symbols)
             ,package)))

(define-option-expander intern (package &rest symbols)
  (with-package-dump-binding (package)
    `(mapcar #'(lambda (s) (intern (string s) ,package)) ',symbols)))

(define-option-expander size (package n)
  (declare (ignore n))
  (error "SIZE option not applicable."))
