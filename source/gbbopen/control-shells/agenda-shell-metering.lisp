;;;; -*- Mode:Common-Lisp; Package:AGENDA-SHELL; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/control-shells/agenda-shell-metering.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:11:04 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         Agenda-Shell Metering
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-22-06 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :agenda-shell)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-control-shell-stats	; not yet documented
	    without-control-shell-stats ; not yet documented
	    )))

;;; ---------------------------------------------------------------------------
;;;  Control shell stats

(defvar *control-shell-stats* nil)

;;; ---------------------------------------------------------------------------
;;;  KS stats

(defun stat-count (stat)
  (car stat))

(defun (setf stat-count) (nv stat)
  (setf (car stat) nv))

(defun stat-runtime (stat)
  (cdr stat))

(defun (setf stat-runtime) (nv stat)
  (setf (cdr stat) nv))

;;; ---------------------------------------------------------------------------

(defstruct (agenda-shell-ks-stats
            (:conc-name #.(dotted-conc-name 'agenda-shell-ks-stats))
            (:copier nil))
  ks
  (activation (cons 0 0) :type cons)
  (activation-predicate (cons 0 0) :type cons)
  (precondition-function (cons 0 0) :type cons)
  (retrigger-function (cons 0 0) :type cons)
  (revalidation-predicate (cons 0 0) :type cons)
  (obviation-predicate (cons 0 0) :type cons)
  (execution (cons 0 0) :type cons))

;;; ---------------------------------------------------------------------------

(defun make-control-shell-stats ()
  ;;; Control-shell-stats is a list:
  ;;;   (<cycle-counter> <start-runtime> <ks-stat>*)
  (let ((result nil))
    (do-instances-of-class (ks '(ks :plus-subclasses))
      (push (make-agenda-shell-ks-stats :ks ks) result))
    (list* 0 (get-internal-run-time) result)))

;;; ---------------------------------------------------------------------------

(defun report-control-shell-stats (&key (reset nil))
  (let ((stats *control-shell-stats*)
	(units-per-second #.(float internal-time-units-per-second)))
    (cond
     ((null stats)
      (format *trace-output* 
	      "~&;; No find/map statistics are available.~%"))
     (t (format *trace-output* 
		"~2&;; Control Shell Statistics (~s cycle~:p, ~s seconds):~%"
		(first stats)
		(/ (- (get-internal-run-time) (second stats))
		   units-per-second))
	;; Skip over the cycle count and start-runtime:
	(dolist (ks-stat (cddr stats))
	  (format *trace-output*
		  ";;~5t~s~%"
		  (instance-name-of (agenda-shell-ks-stats.ks ks-stat)))
	  (flet ((show-it (label accessor show-runtime)
		   (let* ((stat (funcall accessor ks-stat))
			  (stat-count (stat-count stat)))
		     (unless (zerop stat-count)
		       (format *trace-output* 
			       ";;~7t~a: ~s~:[~; (~s seconds)~]~%"
			       label
			       stat-count
			       show-runtime
			       (/ (stat-runtime stat)
				  units-per-second))))))
	    (show-it "Activations"
		     #'agenda-shell-ks-stats.activation nil)
	    (show-it "Activation predicate" 
		     #'agenda-shell-ks-stats.activation-predicate 't)
	    (show-it "Precondition function" 
		     #'agenda-shell-ks-stats.precondition-function 't)
	    (show-it "Retrigger function" 
		     #'agenda-shell-ks-stats.retrigger-function 't)
	    (show-it "Revalidation predicate" 
		     #'agenda-shell-ks-stats.revalidation-predicate 't)
	    (show-it "Obviation predicate" 
		     #'agenda-shell-ks-stats.obviation-predicate 't)
	    (show-it "Executions"
		     #'agenda-shell-ks-stats.execution 't)))		      
	(when reset (setq *control-shell-stats* 
		      (make-control-shell-stats))))))
  (values))

;;; ---------------------------------------------------------------------------

(defun init-control-shell-stats (initialize)
  (if initialize 
      (make-control-shell-stats)
      (or *control-shell-stats*
	  (progn
	    (warn "No control-shell-stats being recorded, ~
                   :initialize 't assumed.")
	    (make-control-shell-stats)))))  

;;; ---------------------------------------------------------------------------

(defmacro with-update-stat ((accessor ks) &body body)
  (with-gensyms (stat start runtime)
    `(flet (,@(when body `((do-body () ,@body))))
       (cond
	(*control-shell-stats*
	 (let ((,stat (funcall 
		       #',accessor 
                       (find
                        ,ks
                        ;; Skip over the cycle count and start-runtime:
                        (cddr *control-shell-stats*)
                        :test #'eq
                        :key #'agenda-shell-ks-stats.ks))))
	   (incf (stat-count ,stat))
	   ,@(when body
	       `((let ((,start (get-internal-run-time)))
		   (multiple-value-prog1 (do-body)
		     (let ((,runtime (- (get-internal-run-time) ,start)))
		       (when (plusp ,runtime)
			 (incf (stat-runtime ,stat) ,runtime)))))))))
	,@(when body
	    '((t (do-body))))))))

;;; ---------------------------------------------------------------------------

(defmacro with-control-shell-stats ((&key (initialize 't) (report 't))
				    &body body)
  `(let ((*control-shell-stats* (init-control-shell-stats ,initialize)))
     (multiple-value-prog1 
	 (progn ,@body)
       (when ,report (report-control-shell-stats)))))

;;; ---------------------------------------------------------------------------

(defmacro without-control-shell-stats (&body body)
  `(let ((*control-shell-stats* nil))
     ,@body))  

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
