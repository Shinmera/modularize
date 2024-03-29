(in-package #:org.shirakumo.radiance.lib.modularize)

(define-option-expander nicknames (package &rest nicknames)
  (loop for nick in nicknames
        do (add-package-nickname package nick)))

(define-option-expander documentation (package documentation)
  (setf (documentation package T) documentation))

(define-option-expander use (package &rest packages)
  (use-package packages package))

(define-option-expander shadow (package &rest symbols)
  (shadow symbols package))

(define-option-expander shadowing-import-from (package import-package &rest symbols)
  (shadowing-import (collect-symbols-from import-package symbols) package))

(define-option-expander import-from (package import-package &rest symbols)
  (import (collect-symbols-from import-package symbols) package))

(define-option-expander export (package &rest symbols)
  (export (mapcar #'(lambda (s) (intern (string s) package)) symbols) package))

(define-option-expander intern (package &rest symbols)
  (mapcar #'(lambda (s) (intern (string s) package)) symbols))

(define-option-expander size (package n)
  (declare (ignore n))
  (error "SIZE option not applicable."))

(define-option-expander local-nicknames (package &rest nicknames)
  (loop for (nickname name) in nicknames
        do (trivial-package-local-nicknames:add-package-local-nickname nickname name package)))

(define-option-expander packages (module &rest names)
  (setf (module-packages module) names))
