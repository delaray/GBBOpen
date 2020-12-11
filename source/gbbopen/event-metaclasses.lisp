;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/event-metaclasses.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb 21 14:48:35 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  GBBopen Event-Metaclass Definitions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-13-05 Split out from events.lisp.  (Corkill)
;;;  02-21-11 Added evstreamers to EVFN-BLK (for mirroring).  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::clear-flag
            gbbopen-tools::set-flag)))

;;; ===========================================================================
;;;   EVFN-BLK -- Event-Function-Block
;;;
;;;  printing-flags:
;;;     event-printing-suspended
;;;     event-printing-enabled
;;;     propagate-enabled-to-subevents
;;;     propagate-enabled-to-subclasses
;;;     propagate-suspended-to-subevents
;;;     propagate-suspended-to-subclasses

(defstruct (evfn-blk 
            (:conc-name #.(dotted-conc-name 'evfn-blk))
            (:copier nil))
  event-class
  unit-class
  slot-or-space-qualifier
  (printing-flags 0 :type fixnum)
  (evfns nil :type list)
  (evstreamers nil :type list)
  ;; used by control-shells:
  (ks-triggers nil))

;;; ---------------------------------------------------------------------------
;;; The flag accessors:

(macrolet 
    ((do-flag (flag mask index)
       `(progn
	  (eval-when (:compile-toplevel :load-toplevel :execute)
	    (defconstant ,mask ,(ash 1 index)))
	  
          (defun ,flag (evfn-blk)
            (declare (type evfn-blk evfn-blk))
            (flag-set-p (evfn-blk.printing-flags evfn-blk) ,index))

          (defcm ,flag (evfn-blk)
            `(flag-set-p (evfn-blk.printing-flags (the evfn-blk ,evfn-blk))
                         ,,index))
          
          (defun (setf ,flag) (nv evfn-blk)
            (declare (type evfn-blk evfn-blk))
            (setf (evfn-blk.printing-flags evfn-blk)
                  (if nv
                      (set-flag (evfn-blk.printing-flags evfn-blk) ,index)
                      (clear-flag (evfn-blk.printing-flags evfn-blk) ,index)))
            nv))))
  (do-flag evfn-blk.event-printing-enabled printing-enabled-mask 0)
  (do-flag evfn-blk.event-printing-suspended printing-suspended-mask 1)
  (do-flag evfn-blk.propagate-enabled-to-subevents 
           propagate-enabled-to-subevents-mask 2)
  (do-flag evfn-blk.propagate-enabled-to-subclasses
           propagate-enabled-to-subclasses-mask 3)
  (do-flag evfn-blk.propagate-suspended-to-subevents
           propagate-suspended-to-subevents-mask 4)
  (do-flag evfn-blk.propagate-suspended-to-subclasses
           propagate-suspended-to-subclasses-mask 5))

;;; ---------------------------------------------------------------------------
;;;
;;; Optimized check for enabled printing (not suspended or enabled):

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant do-printing-mask #b11))

(defun do-event-printing (evfn-blk)
  (declare (type evfn-blk evfn-blk))
  (=& 1 (logand (evfn-blk.printing-flags evfn-blk) #.do-printing-mask)))

(defcm do-event-printing (evfn-blk)
  `(=& 1 (logand (evfn-blk.printing-flags (the evfn-blk ,evfn-blk)) 
		 #.do-printing-mask)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun set-evfn-printing-flags (evfn-blk flags 
				  plus-subevents plus-subclasses)
    (declare (type evfn-blk evfn-blk)
	     (type fixnum flags))
    ;; the `flags' value indicates enabling versus suspending:
    (ecase flags
      (#.printing-enabled-mask
       (when plus-subevents
	 (setf flags (logior #.propagate-enabled-to-subevents-mask flags)))
       (when plus-subclasses
	 (setf flags (logior #.propagate-enabled-to-subclasses-mask flags))))
      (#.printing-suspended-mask
       (when plus-subevents
	 (setf flags (logior #.propagate-suspended-to-subevents-mask flags)))
       (when plus-subclasses
	 (setf flags (logior #.propagate-suspended-to-subclasses-mask 
			     flags)))))
    (setf (evfn-blk.printing-flags evfn-blk)
	  (logior (evfn-blk.printing-flags evfn-blk) 
		  flags))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun clear-evfn-printing-flags (evfn-blk flags)
    (declare (type evfn-blk evfn-blk)
	     (type fixnum flags))
    (setf (evfn-blk.printing-flags evfn-blk)
	  (logandc2 (evfn-blk.printing-flags evfn-blk) 
		    flags))))

;;; ===========================================================================
;;;   Standard-event-class and subclasses (event metaclasses)

(with-generate-accessors-format (:prefix)
  
  (define-class standard-event-class (standard-class)
    ((abstract :initform nil :type boolean)
     (system-event :initform nil :type boolean)
     (event-printing :initform nil)
     ;; holds explict :metaclass (a.k.a :event-metaclass) specification for
     ;; propagation/validation:
     (event-metaclass :initform nil))
    (:export-class-name t))
  
  (define-class non-instance-event-class (standard-event-class)
    ((evfn-blk))
    (:export-class-name t))
  
  (define-class instance-event-class (standard-event-class)
    ()
    (:export-class-name t))
  
  (define-class space-instance-event-class (standard-event-class)
    ((path-event-functions :initform nil))
    (:export-class-name t))
  
  (define-class nonlink-slot-event-class (standard-event-class)
    ()
    (:export-class-name t))
  
  (define-class link-slot-event-class (standard-event-class)
    ()
    (:export-class-name t))
  
  ;; Close with-generate-accessors-format:
  )

;;; ---------------------------------------------------------------------------

(defmethod validate-superclass ((class standard-event-class) 
				(superclass standard-class))
  #+ecl (declare (ignore class superclass))
  't)

;;; Event and unit classes are not compatible:

(defmethod validate-superclass ((class standard-event-class) 
				(superclass standard-unit-class))
  #+ecl (declare (ignore class superclass))
  nil)

(defmethod validate-superclass ((class standard-unit-class) 
				(superclass standard-event-class))
  #+ecl (declare (ignore class superclass))
  nil)

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :around ((class standard-event-class)
				      slot-names &rest initargs &key)
  (apply #'call-next-method class slot-names
	 (loop for (indicator value) on initargs by #'cddr
	     nconc
	       (case indicator
		 ;; single-value class options:
		 ((:abstract :event-metaclass :system-event)
		  (list indicator (sole-element value)))
		 (otherwise (list indicator value))))))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((class non-instance-event-class)
				     slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (setf (non-instance-event-class.evfn-blk class)
	(make-evfn-blk :event-class class)))

;;; ---------------------------------------------------------------------------
;;;  Handle Lispwork's non-standard use of slot-name rather than slot-object.
;;;  (Setf forms must be handled individually.)

#+lispworks
(defmethod slot-value-using-class ((class standard-event-class) 
				   instance 
				   (slot standard-slot-definition))
  (slot-value-using-class class instance (slot-definition-name slot)))
 
#+lispworks
(defmethod slot-boundp-using-class ((class standard-event-class) 
				    instance
				    (slot standard-slot-definition))
  (slot-boundp-using-class class instance (slot-definition-name slot)))
 
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

