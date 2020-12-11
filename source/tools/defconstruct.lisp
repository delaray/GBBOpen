;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/defconstruct.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr  8 05:43:47 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *              Defconstruct -- 2-field cons-based "structure"
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-10-10 Separated from ../tools/atable.lisp.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defconstruct)))             ; not documented yet

;;; ---------------------------------------------------------------------------

(defmacro defconstruct (name fields)
  (destructuring-bind (name &key constructor)
      (ensure-list name)
    (destructuring-bind (field1 field2) 
        fields
      (let* ((name-string (string name))
             (field1-string (string field1))
             (field2-string (string field2))
             (constructor-sym
              (intern 
               (if constructor
                   (string constructor)
                   (concatenate 'string 
                     (symbol-name '#:make-) name-string))))
             (field1-sym
              (intern (concatenate 'string 
                        name-string "-" field1-string)))
             (field2-sym
              (intern (concatenate 'string 
                        name-string "-" field2-string))))
        `(progn
           (defun ,constructor-sym (,field1 ,field2)
             (cons ,field1 ,field2))
           (defcm ,constructor-sym (,field1 ,field2)
             `(cons ,,field1 ,,field2))
           ;; Field1 accessor:
           #+(or cmu scl)
           (declaim (inline ,field1-sym (setf ,field1-sym)))
           (defun ,field1-sym (,name)
             (car ,name))
           #-(or cmu scl)
           (defcm ,field1-sym (,name)
             `(car (the cons ,,name)))
           (defun (setf ,field1-sym) (nv ,name)
             (setf (car ,name) nv))
           #-(or cmu scl)
           (defcm (setf ,field1-sym) (nv ,name)
             `(setf (car (the cons ,,name)) ,nv))
           ;; Field2 accessor:
           #+(or cmu scl)
           (declaim (inline ,field2-sym (setf ,field2-sym)))
           (defun ,field2-sym (,name)
             (cdr ,name))
           #-(or cmu scl)
           (defcm ,field2-sym (,name)
             `(cdr (the cons ,,name)))
           (defun (setf ,field2-sym) (nv ,name)
             (setf (cdr ,name) nv))
           #-(or cmu scl)
           (defcm (setf ,field2-sym) (nv ,name)
             `(setf (cdr (the cons ,,name)) ,nv)))))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
