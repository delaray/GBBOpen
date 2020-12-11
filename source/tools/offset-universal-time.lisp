;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/offset-universal-time.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Nov 16 14:15:02 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   Offset Universal Time Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2010, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; This file is self-contained and can be used stand-alone.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-07 File created.  (Corkill)
;;;  04-30-09 Added :ot REPL command.  (Corkill)
;;;  05-25-09 Added PRINTVOT macro.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':gbbopen-tools)
    (make-package ':gbbopen-tools 
                  :use '(:common-lisp))))

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*ot-base*
            check-ot-base
            ot2ut
            printvot
            set-ot-base
            ut2ot)))

;;; ===========================================================================
;;;   Offset Universal Time
;;;
;;; Standard Common Lisp has three time representations: Decoded Time,
;;; Universal Time, and Internal Time.  We add a fourth representation: Offset
;;; Universal Time (OT).  OT is Universal Time (UT) that is offset by an
;;; integer time-base value so that the most often used OT values in an
;;; application are fixnums.
;;;
;;; Nearly all CL implementations provide fixnums of at least 29 bits (34
;;; years of time range) or more, but CLISP on 32-bit machines provides only
;;; 24 bits (388 days).  The ANSI standard requires an implementation to
;;; provide fixnums with at least 16 bits (only 18 hours), but fortunately CL
;;; implementations are considerably more generous!
;;;
;;; Applications that are representing time values that exceed the fixnum
;;; range need to consider what value of *ot-base* works best in reducing
;;; bignum costs.  Of course, existing OT values will appear shifted if
;;; *ot-base* is changed.

(defvar *ot-base* "You must set the offset-universal-time base (set-ot-base).")

;;; ---------------------------------------------------------------------------
;;; Warn if the CL implementation doesn't have at least 29-bit fixnums:

(defun small-fixnum-ot-warning ()
  (let ((fixnum-size #.(1+ (integer-length most-positive-fixnum))))
    (warn "The ~s-bit fixnums on ~a (~a) provide only ~,1f days of fixnum ~
           OT values."
          fixnum-size
          (lisp-implementation-type) 
          (machine-type)
          (/ (expt 2 fixnum-size) #.(float (* 60 60 24))))))

(let ((fixnum-size (1+ (integer-length most-positive-fixnum))))
  ;; Suppress unreachable code warning in CMUCL and SCL:
  #+(or cmu scl)
  (declare (optimize (extensions:inhibit-warnings 3)))
  (when (< fixnum-size 29)
    (small-fixnum-ot-warning)))

;;; ---------------------------------------------------------------------------

(defun ut2ot (&optional (universal-time (get-universal-time)))
  (- universal-time *ot-base*))

(defcm ut2ot (&optional (universal-time '(get-universal-time)))
  `(- ,universal-time *ot-base*))

;;; ---------------------------------------------------------------------------

(defun ot2ut (offset-universal-time)
  (+ offset-universal-time *ot-base*))

(defcm ot2ut (offset-universal-time)
  `(+ ,offset-universal-time *ot-base*))

;;; ---------------------------------------------------------------------------

(defun printvot-trans-fn (values)
  (if (and (list-length-1-p values)
           (integerp (first values)))
      (cons
       (full-date-and-time (ot2ut (first values)) :include-seconds 't)
       (rest values))
      values))

;;; ---------------------------------------------------------------------------

(defmacro printvot (&rest forms)
  ;;; Like printv, except that any form producing a single-valued integer
  ;;; result is assumed to be an offset-time value and is printed in
  ;;; FULL-DATE-AND-TIME format (including seconds).
  (printv-expander forms #'printvot-trans-fn))

;;; ---------------------------------------------------------------------------

(defun check-ot-base (&optional suppress-warning)
  ;;; Return true if the current time can be represented as a fixnum;
  ;;; otherwise issue a warning and return nil.
  (or (typep (ut2ot) 'fixnum)
      (unless suppress-warning
        (warn "The current time represented as an offset-universal-time is ~
               not a fixnum."))))

;;; ---------------------------------------------------------------------------

(defun set-ot-base (&optional date month year (time-zone 0))
  ;;; Sets the offset-universal-time base to today's date (no supplied args)
  ;;; or to a specified date:
  (let ((base-ut (multiple-value-bind (current-second current-minute current-hour
                                       current-date current-month current-year)
                     (decode-universal-time (get-universal-time) time-zone)
                   (declare (ignore current-second current-minute current-hour))
                   (encode-universal-time 0 0 0 
                                          (or date current-date)
                                          (or month current-month)
                                          (or year current-year)
                                          time-zone))))
    (setf *ot-base*
          (+ base-ut
             (expt 2 24)))
    (check-ot-base)
    (values *ot-base* base-ut)))

;;; ===========================================================================
;;;  Add :ot REPL command (available if using GBBopen's initiate.lisp)

(defun do-ot-repl-command (arg)
  (let ((maybe-ot
         ;; Handle evaluating REPLs:
         (if (integerp arg)
             ;; Already evaluated:
             arg
             ;; Try evaluating:
             (ignore-errors (eval arg)))))
    (when maybe-ot
      (funcall 'full-date-and-time (ot2ut maybe-ot) 
               :include-seconds 't
               :destination *standard-output*)
      maybe-ot)))

;;; ---------------------------------------------------------------------------

(when (fboundp 'define-repl-command)
  (eval `(let ((*current-system-name* ':gbbopen-tools))
           (declare (special *current-system-name*))
           
           (define-repl-command :ot (&rest args)
             "Describe offset-universal-time value"
             (do-ot-repl-command (sole-element args))))))

;;; ===========================================================================
;;; End of File
;;; ===========================================================================


