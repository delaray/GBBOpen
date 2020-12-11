;;;; -*- Mode:Common-Lisp; Package:AGENDA-SHELL; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/control-shells/agenda-shell-metaclasses.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:10:56 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Agenda-Shell Metaclasses
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-30-05 Split out from agenda-shell.lisp.  (Corkill)
;;;  10-05-06 Added define-ks-class and define-ksa-class macros.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':agenda-shell)
    (make-package ':agenda-shell 
                  :use '(:common-lisp :gbbopen-tools :portable-threads 
                         :gbbopen))))

(in-package :agenda-shell)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(define-ks-class
            define-ksa-class
            standard-ksa-class)))

;;; ===========================================================================
;;;   Standard-ksa-class
;;;
;;; This KSA-specific metaclass is needed to manage the rating slot (see
;;; below).

(define-class standard-ksa-class (standard-unit-class)
  ())

;;; ---------------------------------------------------------------------------
;;;
;;; This is a heavy-handed mechanism to provide a specialized :after method on
;;; (setf slot-value-using-class) for the rating slot.  Implementations of the
;;; MOP don't provide an easy way to specify a method with an (eql eslotd)
;;; specializer, and some implementations (such as CMUCL) have problems with
;;; the attempt.  So, instead, we create a special effective-slot-definition
;;; metaclass for the rating slot and dispatch on this special eslotd class.

(define-class effective-rating-slot-definition 
    (effective-nonlink-slot-definition)
  ())

;;; ---------------------------------------------------------------------------
;;; Some syntactic sugar to help ensure that user-defined KSA classes
;;; have the desired standard-ksa-class metaclass:

(defmacro define-ksa-class (ksa-class-name 
                            direct-superclass-names
                            direct-slots &rest options)
  `(define-unit-class ,ksa-class-name ,(or direct-superclass-names '(ksa))
     ,direct-slots          
     ,@(if (member :metaclass options :test #'eq :key #'first)
           options
           (cons `(:metaclass standard-ksa-class) options))))

;;; ---------------------------------------------------------------------------
;;; We provide similar syntactic sugar for user-defined KS classes for 
;;; consistency (and possible future extension), even though there's no
;;; special metaclass currently needed for KS classes:

(defmacro define-ks-class (ks-class-name 
                           direct-superclass-names
                           direct-slots &rest options)
  `(define-unit-class ,ks-class-name ,(or direct-superclass-names '(ks))
     ,direct-slots          
     ,@options))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
