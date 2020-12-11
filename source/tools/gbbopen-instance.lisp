;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/gbbopen-instance.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:56:45 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Standard GBBopen Instance
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-06-06 File created.  (Corkill)
;;;  06-20-09 Added PRINT-INSTANCE-SLOT-VALUE.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(print-instance-slots
            print-instance-slot-value
            standard-gbbopen-instance)))

(defgeneric print-instance-slots (instance stream)
  (:documentation 
   "Extend PRINT-OBJECT printing of STANDARD-GBBOPEN-INSTANCE objects to include
    additional slot values"))

(defgeneric print-instance-slot-value (instance slot-name stream &key)
  (:documentation
   "Print the value of the specified slot or <unbound>, if it is unbound."))

;;; ---------------------------------------------------------------------------

(define-class standard-gbbopen-instance ()
  ()
  (:export-class-name t)
  (:documentation
   "The base class for GBBopen extensions; superclass of DELETED-UNIT-INSTANCE, 
    STANDARD-EVENT-INSTANCE, STANDARD-LINK-POINTER, and STANDARD-UNIT-INSTANCE."))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((instance standard-gbbopen-instance) stream)
  (cond (*print-readably* (call-next-method))
        (t (print-unreadable-object (instance stream :type nil)
             (print-instance-slots instance stream))
           ;; Print-object must return object:
           instance)))

;;; ---------------------------------------------------------------------------
;;;  Print instance slots (extension of PRINT-OBJECT for
;;;  STANDARD-GBBOPEN-INSTANCE objects)

(defmethod print-instance-slots ((instance standard-gbbopen-instance) stream)  
  (format stream "~s" (type-of instance))
  nil)

;;; ---------------------------------------------------------------------------
;;;  Print instance slot value

(defmethod print-instance-slot-value ((instance standard-gbbopen-instance) 
                                      slot-name
                                      stream
                                      &key function no-space)
  (unless no-space (write-char #\Space stream))
  (if (slot-boundp instance slot-name)
      (let ((slot-value (slot-value instance slot-name)))
        (prin1 (if function 
                   (funcall function slot-value)
                   slot-value)
               stream))
      (princ "[Unbound]" stream)))

;;; ===========================================================================
;;; End of File
;;; ===========================================================================
