#|
 This file is a part of Modularize
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(define-option-expander nicknames (package &rest nicknames)
  `(progn
     ,@(loop for nick in nicknames
             collect `(add-package-nickname ,package ,nick))))

(define-option-expander documentation (package documentation)
  `(setf (documentation ,package T)
         ,(format NIL "~a -- ~a" (documentation package T) documentation)))

(define-option-expander use (package &rest packages)
  `(use-package ',packages ,package))

(define-option-expander shadow (package &rest symbols)
  `(shadow ',symbols ,package))

(define-option-expander shadowing-import-from (package import-package &rest symbols)
  `(shadowing-import ',(collect-symbols-from import-package symbols) ,package))

(define-option-expander import-from (package import-package &rest symbols)
  `(import ',(collect-symbols-from import-package symbols) ,package))

(define-option-expander export (package &rest symbols)
  `(export ',(loop for symbol in symbols
                   collect (intern (string symbol) package)) ,package))

(define-option-expander intern (package &rest symbols)
  `(progn
     ,@(loop for symbol in symbols
             collect `(intern ',(symbol-name symbol) ,package))))

(define-option-expander size (package n)
  (declare (ignore n))
  (error "SIZE option not applicable."))