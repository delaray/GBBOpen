;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/duplicate-instance.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jun 23 12:08:37 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          Duplicate Instance
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-18-08 File created.  (Corkill)
;;;  03-13-09 Added DUPLICATE-INSTANCE-CHANGING-CLASS.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-duplicate-instance 
            make-duplicate-instance-changing-class
            unduplicated-slot-names)))

;;; ===========================================================================

(defgeneric make-duplicate-instance (instance unduplicated-slot-names
                                     &rest initargs))
(defgeneric make-duplicate-instance-changing-class (instance new-class
                                                    unduplicated-slot-names
                                                    &rest initargs))
(defgeneric initialize-duplicated-instance (instance slots slot-values 
                                            slot-names-to-initialize))
(defgeneric unduplicated-slot-names (instance))

;;; ---------------------------------------------------------------------------

(defmethod unduplicated-slot-names ((instance standard-object))
  #+ecl (declare (ignore instance))
  nil)

;;; ---------------------------------------------------------------------------

(defmethod initialize-duplicated-instance ((instance standard-object)
                                           slots slot-values 
                                           slot-names-to-initialize)
  (let ((class (class-of instance)))
    (loop for slot in slots
        and value in slot-values do 
          ;; Lispworks can't use slot-value-using-class here
          ;; (prior to shared-initialize?):
          #+lispworks
          (setf (slot-value (slot-definition-name slot) slot-name) value)
          #-lispworks
          (setf (slot-value-using-class class instance slot) 
                value)))
  ;; Initialize any missing slots (but not existing slots that are to remain
  ;; unbound):
  (when slot-names-to-initialize
    (let ((*%%skip-gbbopen-shared-initialize-method-processing%%* 't))
      (shared-initialize instance slot-names-to-initialize))))

;;; ---------------------------------------------------------------------------

(defun lookup-initarg-value (indicators initargs not-found-value)
  ;; Return the value associated with an indicator (in `indicators') from
  ;; `initargs' or `not-found-value' if none is found:
  (dolist (indicator indicators not-found-value)
    (let ((result (getf initargs indicator not-found-value)))
      (unless (eq result not-found-value)
        (return-from lookup-initarg-value result)))))

;;; ---------------------------------------------------------------------------

(defun duplicate-instance (instance new-class unduplicated-slot-names initargs)
  ;;; Common internal function for MAKE-DUPLICATE-INSTANCE and
  ;;; MAKE-DUPLICATE-INSTANCE-CHANGING-CLASS
  (let* ((old-class (class-of instance))
         (old-class-slots (class-slots old-class))
         (new-instance (allocate-instance new-class))
         (slots nil)
         (slot-values nil)
         (slot-names-to-initialize nil)
         (computed-unduplicated-slot-names (unduplicated-slot-names instance))
         (not-found-value '#.(gensym)))
    (declare (dynamic-extent slots slot-values slot-names-to-initialize))
    (dolist (slot (class-slots new-class))
      (let* ((slot-name (slot-definition-name slot))
             (old-slot 
              ;; (car (member ...)) with :key often optimizes better than
              ;; (find ...):
              (car (member slot-name old-class-slots 
                           :key #'slot-definition-name)))
             ;; See if we are setting via an initarg:
             (maybe-initarg-value
              (lookup-initarg-value 
               (slot-definition-initargs slot) initargs not-found-value)))
        (cond 
         ;; An initarg was specified for the slot:
         ((not (eq maybe-initarg-value not-found-value))
          (push slot slots)
          (push maybe-initarg-value slot-values))
         ;; Record as a slot-name-to-initialize, if asked not to duplicate it
         ;; (either specified in `unduplicated-slot-names' or computed by
         ;; UNDUPLICATED-SLOT-NAMES method) or if this slot is defined only in
         ;; the new class:
         ((or (not old-slot)
              (memq slot-name computed-unduplicated-slot-names)
              (memq slot-name unduplicated-slot-names))
          (push slot-name slot-names-to-initialize))
         ;; Slot is unbound (and should stay so in the duplicated instance):
         ((not (slot-boundp-using-class old-class instance old-slot)))
         ;; Not an initarg or an unduplicated-slot-name or an unbound slot,
         ;; use the slot-value from the old instance:
         (t (push slot slots)
            (push (slot-value-using-class old-class instance old-slot)
                  slot-values)))))
    (initialize-duplicated-instance 
     new-instance slots slot-values slot-names-to-initialize)
    (values new-instance slots)))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance ((instance standard-object) 
                                    unduplicated-slot-names
                                    &rest initargs)
  (declare (dynamic-extent initargs))
  (duplicate-instance instance (class-of instance) 
                      unduplicated-slot-names initargs))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance-changing-class ((instance standard-object) 
                                                   (new-class symbol)
                                                   unduplicated-slot-names
                                                   &rest initargs)
  (declare (dynamic-extent initargs))
  ;; We must call MAKE-DUPLICATE-INSTANCE-CHANGING-CLASS with the new-class
  ;; object (rather than calling duplicate-instance directly):
  (apply 'make-duplicate-instance-changing-class instance
         (find-class new-class)
         unduplicated-slot-names
         initargs))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance-changing-class ((instance standard-object) 
                                                   (new-class class)
                                                   unduplicated-slot-names
                                                   &rest initargs)
  (declare (dynamic-extent initargs))
  (duplicate-instance instance new-class unduplicated-slot-names initargs))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
