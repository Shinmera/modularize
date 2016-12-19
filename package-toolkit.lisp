#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize)

(defun ensure-package (thing)
  (etypecase thing
    (package thing)
    ((or string symbol)
     (or (find-package thing)
         (error "No such package ~s" thing)))))

(defmacro with-package ((var &optional (package var)) &body body)
  `(let ((,var (ensure-package ,package)))
     ,@body))

(defun add-package-nickname (package nickname)
  (with-package (package)
    (let ((nicks (package-nicknames package)))
      (unless (member nickname nicks :test #'string=)
        (rename-package package (package-name package)
                        (cons nickname nicks))))))

(defun collect-symbols-from (package symbols)
  (loop with importing-package = (ensure-package package)
        for symbol in symbols
        collect (or (find-symbol (string symbol) importing-package)
                    (error "Symbol ~s not found in ~s" symbol package))))

(defun extend-package (package definition-options)
  (with-package (package)
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
                (error "SIZE option not applicable to EXTEND-PACKAGE."))))))

(defun unbind-and-delete-package (package)
  (with-package (package)
    (do-symbols (symbol package)
      (when (eql (symbol-package symbol) package)
        (when (fboundp symbol)
          (fmakunbound symbol))
        (when (boundp symbol)
          (makunbound symbol))))
    (delete-package package)))
