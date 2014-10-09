#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize)

(defun add-package-nickname (package nickname)
  "Adds the nickname onto the package's nicknames list."
  (let ((nicks (package-nicknames package)))
    (unless (member nickname nicks :test #'string=)
      (rename-package package (package-name package)
                      (cons nickname nicks)))))

(defun collect-symbols-from (package symbols)
  "Collects all given symbols from the package."
  (loop with importing-package = (find-package package)
        for symbol in symbols
        collect (or (find-symbol (string symbol)
                                 importing-package)
                    (error "Symbol ~s not found in ~s" symbol package))))

(defun extend-package (package definition-options)
  "Extends the package with the package definition options.
Any option except for the SIZE option is allowed.
Note that this only ADDS onto the package and does not
remove any options defined prior. As in, nicknames are
only added on, but not removed."
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

(defun unbind-and-delete-package (package)
  "Unbinds all symbols in the package from their functions and values
and finally deletes the package."
  (do-symbols (symbol package)
    (when (eql (symbol-package symbol) package)
      (when (fboundp symbol)
        (fmakunbound symbol))
      (when (boundp symbol)
        (makunbound symbol))))
  (delete-package package))
