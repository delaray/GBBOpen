;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/preamble.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb 21 14:01:40 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            GBBopen Preamble
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-17-02 File created.  (Corkill)
;;;  09-26-04 Renamed INSTANCE-HASH-TABLE-TEST to more descriptive
;;;           INSTANCE-NAME-COMPARISON-TEST.  (Corkill)
;;;  11-05-05 Added *COERCE-INTERVAL-RATIONALS-TO-FLOATS*.  (Corkill)
;;;  07-30-10 Added WITH-BLACKBOARD-REPOSITORY-LOCKED.  (Corkill)
;;;  02-21-11 Added streamer argument to ADDTO-/RMFROM-EVFN-BLOCK-USING-CLASS.
;;;           (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :portable-threads))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*gbbopen-install-root*      ; re-export, not documented
            *warn-about-unusual-requests*
     	    draw-instance-on-bb-widget  ; used for bb-widget drawing
	    gbbopen-graphics-started-p
	    gbbopen-implementation-version
	    instance-name-comparison-test
            with-blackboard-repository-locked)))

(unless (boundp '*gbbopen-install-root*)
  (error "~s was not set." '*gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  GBBopen version (read from ../../VERSION file)

(defun gbbopen-implementation-version ()
  (with-open-file (version-file 
                   (make-pathname
                    :name "VERSION"
                    :type nil
                    :defaults *gbbopen-install-root*))
    (read version-file)))

;;; Added to *features* in epilogue.lisp:
(defparameter *gbbopen-version-keyword* 
    ;; Support cross-case mode CLs:
    (read-from-string (format nil ":gbbopen-~a" 
                              (gbbopen-implementation-version))))

;;; ---------------------------------------------------------------------------
;;;  Control warnings when:
;;;   1. an instance is added to a space instance when it is already there
;;;   2. an instance is removed from a space instance when it is not there
;;;   3. an instance is added to a space instance that shares no dimensions 
;;;      with it

(defvar *warn-about-unusual-requests* 't)

;;; ---------------------------------------------------------------------------

;;; Although heavy-weight, we use one master lock to synchronize all
;;; unit-instance & space-instance activities:

(defvar *master-instance-lock* 
    (make-recursive-lock :name "Master instance lock"))

(defmacro with-blackboard-repository-locked
    ((&key (whostate "With Blackboard Repository Locked"))
     &body body)
  `(with-lock-held (*master-instance-lock* :whostate ,whostate)
     ,@body))

;;; ---------------------------------------------------------------------------

(defun print-gbbopen-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  GBBopen ~a
;;;
;;;    GBBopen is open-source software
;;;    (see ~a)
;;; ~72,,,'-<-~>~2%"
          (gbbopen-implementation-version)
          (let ((this-file *load-truename*))
            (namestring
             (make-pathname
              :directory (butlast (pathname-directory this-file) 2)
              :name "LICENSE"
              :type nil
              :defaults this-file)))))
  
(eval-when (:load-toplevel)
  (print-gbbopen-herald))

;;; ---------------------------------------------------------------------------

(defun internal-error (datum &rest args)
  (declare (dynamic-extent args))
  (apply #'error (concatenate 'string "Internal GBBopen Error => " datum)
	 args))

;;; ===========================================================================
;;;  GBBopen Core Generic Functions

(defgeneric addto-evfn-using-class (fn event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths 
                                    permanent priority
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args))
(defgeneric add-instance-to-space-instance (instance space-instance))
(defgeneric add-instance-to-storage (instance storage verbose))
(defgeneric check-instance-storage-locators (instance storage))
(defgeneric class-instances-count (unit-class-name-or-class))
(defgeneric class-instances-summary (unit-class-name-or-class))
(defgeneric delete-instance (instance))
(defgeneric delete-space-instance (space-instance))
(defgeneric deleted-instance-class (instance))
(defgeneric describe-instance (instance &optional stream))
(defgeneric describe-instance-slot-value (instance slot-name value 
                                          &optional stream))
(defgeneric describe-space-instance (space-instance))
(defgeneric describe-space-instance-storage (space-instance))
(defgeneric describe-unit-class (unit-class-name-or-class))
(defgeneric describe-unit-slot (unit-class slot))
(defgeneric ds-evfn-using-class (fn event-class &rest args))
(defgeneric do-evfns (evfn-blk event-class args))
(defgeneric draw-instance-on-bb-widget (instance bb-widget &optional filter))
(defgeneric event-printer (class stream &rest args))
(defgeneric evfn-describer (evfn-blk fn))
(defgeneric hidden-nonlink-slot-names (instance))
(defgeneric initial-class-instance-number (unit-class-name-or-class))
(defgeneric instance-name-comparison-test (unit-class))
(defgeneric link-slot-p (slot))
(defgeneric link-instance-of (instance-or-link-object))
(defgeneric (setf link-instance-of) (nv instance-or-link-object))
(defgeneric map-all-instances-on-storage (fn storage 
					  disjunctive-dimensional-extents  
					  verbose))
(defgeneric map-marked-instances-on-storage (fn storage 
					     disjunctive-dimensional-extents 
                                             verbose))
(defgeneric next-class-instance-number (unit-class-name-or-class))
(defgeneric propagate-event-evfns/printing (event-class
                                            super-event-classes))
(defgeneric remove-instance-from-space-instance (instance space-instance))
(defgeneric remove-instance-from-storage (instance storage old-dimension-values 
                                          dimensions-being-changed verbose))
(defgeneric reset-unit-class (unit-class-name-or-class))
(defgeneric rmfrom-evfn-using-class (fn event-class plus-subevents 
                                     unit-class/instance plus-subclasses
                                     slot-names paths permanent 
                                     printing streamer
                                     evfn-blk-fn evfn-blk-fn-args))
(defgeneric setup-instance-storage (space-instance instance-mapping))
(defgeneric signal-event-using-class (event-class &rest args))
(defgeneric validate-event-metaclass (event-metaclass-name
                                      super-event-metaclasses))
(defgeneric validate-class-option (metaclass option))

;;; ---------------------------------------------------------------------------
;;;   Multinode Interface Generic Functions (predefined here)

(defgeneric save-gbbopen-node-state (node-state))
(defgeneric restore-gbbopen-node-state (node-state))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


