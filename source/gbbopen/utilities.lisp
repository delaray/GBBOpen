;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/utilities.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:08:55 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen Utility Functions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-18-03 File created.  (Corkill)
;;;  05-21-04 Export INTERVAL-START, -END, and -VALUES.  (Corkill)
;;;  06-07-04 Added BOUNDED-UNIFORM-BUCKET-INDEX and
;;;           BOUNDED-UNIFORM-BUCKET-INTERVAL-INDEXES.  (Corkill)
;;;  11-05-05 Added EXPAND-INTERVAL and SHIFT-INTERVAL.  (Corkill)
;;;  06-13-07 Extend BOUNDED-UNIFORM-BUCKET-INTERVAL to handle unbound
;;;           value indexing.  (Corkill)
;;;  06-01-08 Added EXPAND-POINT.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*coerce-contracted-interval-rationals-to-floats*
	    copy-interval
            expand-interval
            expand-point
            ;; --- declared-type operators:
            expand-point&
            expand-point$&
            expand-point$
            expand-point$$
            expand-point$$$
            expand-point%
	    infinite-interval
	    interval-start
	    interval-end
	    interval-values
            make-interval
            nexpand-interval
            nshift-interval
	    shift-interval)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Some implementations (SBCL) are very strict on eql constant redefinition,
  ;; so avoid redefinition by checking for a bound value:
  (unless (boundp 'infinite-interval)
    (defconstant infinite-interval '#.(cons -infinity infinity))))

;;; ---------------------------------------------------------------------------
;; Control automatic coercion of non-integer rationals to floats when an
;; interval is contracted into a non-integral point range by expand-interval
;; and nexpand-interval:

(defvar *coerce-contracted-interval-rationals-to-floats* 't)

;;; ===========================================================================
;;;  Interval Functions:
;;;
;;;  Intervals are one of: (start end), (start . end), or #(start end).
;;;  We don't worry about length or (<= start end) checking here.

(defun invalid-interval-value (interval)
  (error "Invalid interval ~s" interval))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun interval-start (interval)
    ;;; Returns the start value of `interval'
    (if (consp interval)
	(car (the cons interval)) 
	(without-cmucl/sbcl/scl-optimization-warnings
	  (elt (the (simple-array * (2)) interval) 0)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun (setf interval-start) (nv interval)
    ;;; Sets the start value of `interval'
    (if (consp interval)
	(setf (car (the cons interval)) nv)
	(without-cmucl/sbcl/scl-optimization-warnings
 	  (setf (elt (the (simple-array * (2)) interval) 0) nv)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun interval-end (interval)
    ;;; Returns the end value of `interval'
    (if (consp interval) 
	(let ((maybe-end (cdr (the cons interval))))
	  (if (consp maybe-end)
	      (sole-element maybe-end)
	      maybe-end))
	(without-cmucl/sbcl/scl-optimization-warnings
	    (elt (the (simple-array * (2)) interval) 1)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun (setf interval-end) (nv interval)
    ;;; Sets the end value of `interval'
    (if (consp interval) 
	(let ((maybe-end (cdr (the cons interval))))
	  (if (consp maybe-end)
	      (setf (car maybe-end) nv)
	      (setf (cdr interval) nv)))
	(without-cmucl/sbcl/scl-optimization-warnings
	   (setf (elt (the (simple-array * (2)) interval) 1) nv)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun make-interval (start end &optional (type-specifier 'cons))
    (ecase type-specifier
      (cons (cons start end))
      (list (list start end))
      (array
       (without-cmucl/sbcl/scl-optimization-warnings
        (let ((array (make-array '(2))))
          (setf (elt (the (simple-array * (2)) array) 0) start)
          (setf (elt (the (simple-array * (2)) array) 1) end)
          array))))))

(defcm make-interval (&whole whole start end 
                             &optional (type-specifier ''cons))
  ;; In-line when `type-specifier' is a compile-time constant:
  (if (and (consp type-specifier)
           (eq (first type-specifier) 'quote))
      (ecase (sole-element (rest type-specifier))
        (cons `(cons ,start ,end))
        (list `(list ,start ,end))
        (array `(vector ,start ,end)))
      ;; otherwise, compile the normal make-interval call:
      whole))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun copy-interval (interval)
    (if (consp interval)
        (let ((maybe-end (cdr (the cons interval))))
          (if (consp maybe-end)
              (list (car interval) (sole-element maybe-end))
              (cons (car interval) maybe-end)))
        (without-cmucl/sbcl/scl-optimization-warnings
         (let ((array
                (make-array '(2) :element-type (array-element-type interval))))
           (setf (elt (the (simple-array * (2)) array) 0)
                 (elt (the (simple-array * (2)) interval) 0))
           (setf (elt (the (simple-array * (2)) array) 1)
                 (elt (the (simple-array * (2)) interval) 1))
           array)))))

;;; ---------------------------------------------------------------------------

(defun compute-expand-interval-values (start end expand-amount)
  (let ((new-start (- start expand-amount))
        (new-end (+ end expand-amount)))
    (if (> new-start new-end)
        (let ((point-value (/ (+ new-start new-end) 2)))
          ;; Coerce non-integral rationals, if requested:
          (when (and (typep point-value 'ratio)
                     *coerce-contracted-interval-rationals-to-floats*)
            (setf point-value (coerce point-value 'float)))
          (values point-value point-value))
        (values new-start new-end))))

;;; ---------------------------------------------------------------------------

(defun expand-interval (interval expand-amount)
  (if (consp interval)
      (let ((maybe-end (cdr (the cons interval))))
        (if (consp maybe-end)
            (multiple-value-call 
                #'list (compute-expand-interval-values 
                        (car interval)
                        (sole-element maybe-end)
                        expand-amount))
            (multiple-value-call
                #'cons (compute-expand-interval-values
                        (car interval)
                        maybe-end
                        expand-amount))))
      (let ((array 
             (make-array '(2) :element-type (array-element-type interval))))
        (multiple-value-setf ((elt (the (simple-array * (2)) array) 0)
                              (elt (the (simple-array * (2)) array) 1))
          (compute-expand-interval-values
           (elt (the (simple-array * (2)) interval) 0)
           (elt (the (simple-array * (2)) interval) 1)
           expand-amount))
        array)))
      
;;; ---------------------------------------------------------------------------

(defun nexpand-interval (interval expand-amount)
  ;; Destructive version of expand-interval:
  (if (consp interval)
      (let ((maybe-end (cdr (the cons interval))))
        (if (consp maybe-end)
            (multiple-value-setf ((first interval) (second interval))
              (compute-expand-interval-values 
               (car interval)
               (sole-element maybe-end)
               expand-amount))
            (multiple-value-setf ((car interval) (cdr interval))
              (compute-expand-interval-values
               (car interval)
               maybe-end
               expand-amount))))
      (multiple-value-setf ((elt (the (simple-array * (2)) interval) 0)
                            (elt (the (simple-array * (2)) interval) 1))
        (compute-expand-interval-values
         (elt (the (simple-array * (2)) interval) 0)
         (elt (the (simple-array * (2)) interval) 1)
         expand-amount)))
  ;; Return the expanded interval:
  interval)
      
;;; ---------------------------------------------------------------------------

(defun shift-interval (interval shift-amount)
  (if (consp interval)
      (let ((maybe-end (cdr (the cons interval))))
	(if (consp maybe-end)
	    (list (+ (car interval) shift-amount)
		  (+ (sole-element maybe-end) shift-amount))
	    (cons (+ (car interval) shift-amount)
		  (+ maybe-end shift-amount))))
      (let ((array 
             (make-array '(2) :element-type (array-element-type interval))))
        (setf (elt (the (simple-array * (2)) array) 0) 
              (+ (elt (the (simple-array * (2)) interval) 0) shift-amount))
        (setf (elt (the (simple-array * (2)) array) 1) 
              (+ (elt (the (simple-array * (2)) interval) 1) shift-amount))
        array)))

;;; ---------------------------------------------------------------------------

(defun nshift-interval (interval shift-amount)
  ;; Destructive version of shift-interval:
  (cond 
   ((consp interval)
    (let ((maybe-end (cdr (the cons interval))))
      (cond 
       ((consp maybe-end)
        (incf (first interval) shift-amount)
        (setf (second interval) (+ (sole-element maybe-end) shift-amount)))
       (t
        (incf (car interval) shift-amount)
        (setf (cdr interval) (+ maybe-end shift-amount))))))
   (t (incf (elt (the (simple-array * (2)) interval) 0) shift-amount)
      (incf (elt (the (simple-array * (2)) interval) 1) shift-amount)))
  ;; Return the shifted interval:
  interval)

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun interval-values (interval)
    ;;; Return the start and end values of `interval' as multiple values
    ;;; (facilitates interval processing):
    (cond ((consp interval)
           (values (car (the cons interval))
                   (let ((maybe-end (cdr (the cons interval))))
                     (if (consp maybe-end)
                         (sole-element maybe-end)
                         maybe-end))))
          (t (without-cmucl/sbcl/scl-optimization-warnings
              (values 
               (elt (the (simple-array * (2)) interval) 0) 
               (elt (the (simple-array * (2)) interval) 1)))))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun %set-interval-values (destination-interval source-interval)
    ;;; Setter for (setf (interval-values <place>) interval)
    (multiple-value-bind (start end)
        (interval-values source-interval)
      (cond 
       ((consp destination-interval)
        (setf (car destination-interval) start)
        (let ((maybe-end (cdr (the cons destination-interval))))
          (if (consp maybe-end)
              (setf (car maybe-end) end)
              (setf (cdr destination-interval) end))))
       (t (without-cmucl/sbcl/scl-optimization-warnings
           (setf (elt (the (simple-array * (2)) destination-interval) 0) start)
           (setf (elt (the (simple-array * (2)) destination-interval) 1) end)))))
    ;; Return the original format `interval values':
    source-interval))

(defsetf interval-values %set-interval-values)

;;; ---------------------------------------------------------------------------

(defun expand-point (point expand-amount &optional (type-specifier 'cons))
  (make-interval (- point expand-amount)
                 (+ point expand-amount)
                 type-specifier))

(defcm expand-point (point expand-amount &optional (type-specifier ''cons))
  (with-once-only-bindings (point expand-amount)
    `(make-interval (- ,point ,expand-amount)
                    (+ ,point ,expand-amount)
                    ,type-specifier)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %def-expand-point (dn-op)
    (let ((op (intern (concatenate 'simple-string
                        (symbol-name 'expand-point) (symbol-name dn-op))
                      ':gbbopen))
          (+-op (intern (concatenate 'simple-string
                          "+" (symbol-name dn-op))
                        ':gbbopen))
          (--op (intern (concatenate 'simple-string
                          "-" (symbol-name dn-op))
                        ':gbbopen)))
      `(progn
         (defun ,op (point expand-amount &optional (type-specifier 'cons))
           (make-interval (,--op point expand-amount)
                          (,+-op point expand-amount)
                          type-specifier))
      
         (defcm ,op (point expand-amount &optional (type-specifier ''cons))
           (with-once-only-bindings (point expand-amount)
             `(make-interval (,',--op ,point ,expand-amount) 
                             (,',+-op ,point ,expand-amount)
                             ,type-specifier)))))))

(%def-expand-point &)
(%def-expand-point $&)
(%def-expand-point $)
(%def-expand-point $$)
(%def-expand-point $$$)
(%def-expand-point %)

;;; ===========================================================================
;;;   Uniform-bucket index computations:

(defun uniform-bucket-index (value start size)
  ;;; Compute the bucket index for `value' for uniform buckets of `size':
  ;;;  - a negative index preceeds the buckets
  ;;;
  ;;;  Although the result is a fixnum, the internal calculations are general
  ;;;  numeric with infinity values.
  (case value
    (#.-infinity most-negative-fixnum)
    (#.infinity most-positive-fixnum)
    (otherwise (floor (- value start) size))))

;;; ---------------------------------------------------------------------------

(defun bounded-bucket-index (value start size number-of-buckets)
  ;;; Compute the bounded uniform bucket index for `value' for
  ;;; `number-of-buckets' uniform buckets of `size':
  ;;;  - a value that precedes the buckets returns an index of 0
  ;;;  - a value that follows the buckets returns an index of
  ;;;    (+ 1 number-of-buckets)
  ;;;  - other values return the "natural" index incremented by 1
  ;;;
  ;;;  Although the result is a fixnum, the internal calculations are general
  ;;;  numeric with infinity values.
  (let ((bucket-index (floor (- value start) size)))
    (declare (type fixnum bucket-index))
    (cond ((minusp& bucket-index) 0)
          ((>& bucket-index number-of-buckets)
           (1+& number-of-buckets))
          (t (1+& bucket-index)))))

;;; ---------------------------------------------------------------------------

(defun bounded-uniform-bucket-index (value start size number-of-buckets)
  ;;; Compute the bounded uniform bucket index for `value' for
  ;;; `number-of-buckets' uniform buckets of `size':
  ;;;  - a value that precedes the buckets returns an index of 0
  ;;;  - a value that follows the buckets returns an index of
  ;;;    (+ 1 number-of-buckets)
  ;;;  - an "unbound" value returns an index of (+ 2 number-of-buckets)
  ;;;  - other values return the "natural" index incremented by 1
  (case value
    (#.-infinity 0)
    (#.infinity (1+& number-of-buckets))
    (#.unbound-value-indicator (+& 2 number-of-buckets))
    (otherwise (bounded-bucket-index value start size number-of-buckets))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun bounded-uniform-bucket-interval-indexes (start-value end-value
                                                  start size number-of-buckets)
    ;;; Compute the bounded uniform bucket interval indexes for
    ;;; `start-value' through `end-value' for `number-of-buckets'
    ;;; uniform buckets of `size':
    ;;;  - a value that precedes the buckets includes an index of 0
    ;;;  - a value that follows the buckets includes an index of
    ;;;    (+ 1 number-of-buckets)
    ;;;  - other values return the "natural" indexes are incremented by 1
    (values 
     (bounded-uniform-bucket-index start-value start size number-of-buckets)
     (bounded-uniform-bucket-index end-value start size number-of-buckets))))
  
;;; ===========================================================================
;;;  Formatted output helpers:

(defun format-column (tab-column values &optional (ctrl-string "~s")
                                                  (key #'identity))
  (if values
      (let ((key (coerce key 'function)))
	(dolist (value values)
	  (multiple-value-call
	      #'format t "~&~vt~@?" tab-column ctrl-string 
	      (funcall key value))))
      (format t "~vtNone" tab-column)))
  
;;; ---------------------------------------------------------------------------

(defun maybe-format-labeled-entry (tab-column test label 
                                   &optional (value test))
  (when test
    (format t "~&~vt~s ~w" tab-column label value)))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

