;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/find.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jul 30 09:23:07 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *        Find/Filter Instances & Map-on-Space-Instance Functions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-21-03 File created.  (Corkill)
;;;  03-10-04 Simple find/filter patterns completed.  (Corkill)
;;;  03-15-04 Range find/filter patterns completed.  (Corkill)
;;;  03-17-04 Added missing unit-classes-spec checks to map & finds.  (Corkill)
;;;  05-07-04 Added verbose find-instance printing.  (Corkill)
;;;  05-21-04 Added boolean dimension support.  (Corkill)
;;;  06-03-04 Added boolean eqv match operator.  (Corkill)
;;;  06-05-04 Added pattern/space-instance compatibility checking.  (Corkill)
;;;  06-07-04 Fix intersect-ordered-extent bug when adding to multiple 
;;;           extents.  (Corkill)
;;;  06-08-04 Quick fix for negated-region bug in forming disjunctive extents;
;;;           proper repair will require reworking of extent formation to
;;;           operate in all dimensions of an operator together.  (Corkill)
;;;  06-08-04 Add extent-generator-fn for ordered pattern operators.  (Corkill)
;;;  04-25-05 Fix interval instance, interval pattern operator.  (Corkill)
;;;  05-26-05 Add :all pattern support for find-instances and
;;;           map-instances-on-space-instances.  (Corkill)
;;;  08-20-06 Added DO-INSTANCES-ON-SPACE-INSTANCES syntactic sugar.  (Corkill)
;;;  03-01-08 Added abuts ordered-match operator.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::clear-flag
            gbbopen-tools::flag-set-p
            gbbopen-tools::set-flag)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*find-verbose*
            *use-marking*
            abuts
            ;; --- declared-type operators:
            abuts&
            abuts$
            abuts$&
            abuts$$
            abuts$$$
            abuts%
            covers
            ;; --- declared-type operators:
            covers&
            covers$
            covers$&
            covers$$
            covers$$$
            covers%
            ;; ---
            do-instances-on-space-instances
            ends
            ;; --- declared-type operators:
            ends&
            ends$&
            ends$
            ends$$
            ends$$$
            ends%
            ;; ---
            eqv
            false
            filter-instances
            find-instances
            is
            ;; --- declared-type operators:
            is-eq
            is-eql
            is-equal
            is-equalp
            ;; ---
            map-instances-on-space-instances
            overlaps
            ;; --- declared-type operators:
            overlaps&
            overlaps$&
            overlaps$
            overlaps$$
            overlaps$$$
            overlaps%
            ;; ---
            report-find-stats
            starts
            ;; --- declared-type operators:
            starts&
            starts$&
            starts$
            starts$$
            starts$$$
            starts%
            ;; ---
            true
            with-find-stats
            within
            ;; --- declared-type operators:
            within&
            within$&
            within$
            within$$
            within$$$
            within%
            ;; ---
            without-find-stats)))

;;; ---------------------------------------------------------------------------

(defvar *find-verbose* nil)

(defvar *use-marking* 't)

;;; Dynamic bindings used during pattern optimization

(defvar *%%pattern%%*)
(defvar *%%pattern-clause%%*)
(defvar *%%pattern-extents%%*)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Some implementations (SBCL) are very strict on eql constant redefinition,
  ;; so avoid redefinition by checking for a bound value:
  (unless (boundp 'infinite-extent)
    (defconstant infinite-extent '#.(cons -infinity infinity))))

;;; ---------------------------------------------------------------------------

(defstruct (find-stats
            (:conc-name #.(dotted-conc-name 'find-stats))
            (:copier nil))
  (number-of-finds 0 :type integer)
  (number-using-marking 0 :type integer)
  (run-time 0 :type integer)
  (bucket-count 0 :type integer)
  (instances-touched 0 :type integer)
  (instances-considered 0 :type integer)
  (instances-accepted 0 :type integer))

(defvar *find-stats* nil)

;;; ---------------------------------------------------------------------------

(defun find-verbose-operation (operation)
  (format *trace-output* "~2&;; Performing ~s...~%" operation))

;;; ---------------------------------------------------------------------------

(defun find-verbose-preamble (pattern optimized-pattern storage-objects
                              space-instances)
  (format *trace-output* "~&;; Search pattern:  ~:@w~%" pattern)
  (format *trace-output* 
          "~&;; Space instances: ~:[None~%~;~:*~@<~{~w~^~:@_~}~:>~]" 
          (mapcar #'instance-name-of (find-space-instances space-instances)))
  (let ((disjunctive-dimensional-extents
         (optimized-pattern.disjunctive-dimensional-extents 
          optimized-pattern)))
    (format *trace-output* "~&;; Dimensional extents: ~
                            ~:[None~%~;~:[~2:*~<~{~w~^~:@_~}~:>~;All~%~]~]"
            disjunctive-dimensional-extents
            (eq disjunctive-dimensional-extents 't)))       
  (format *trace-output* "~&;; Using ~s storage object~:p~%"
          (length storage-objects)))

;;; ---------------------------------------------------------------------------

(defun find-verbose-begin-match (instance)
  (format *trace-output* "~&;;~5tMatching instance ~s~%"
          instance))

;;; ---------------------------------------------------------------------------

(defun find-verbose-match-success (instance)
  (format *trace-output* "~&;;~7tMatch succeeds on ~s~%"
          instance))

;;; ---------------------------------------------------------------------------

(defun find-verbose-match-failure (instance)
  (format *trace-output* "~&;;~7tMatch fails on ~s~%"
          instance))

;;; ---------------------------------------------------------------------------

(defun find-verbose-dimension-failure (instance dimension-name op-fn
                                       pattern-value dimension-value)
  (format *trace-output* "~&;;~7tMatch fails on ~s~%~
                            ;;~9tDimension: ~s~%~
                            ;;~9tOperator: ~w~%~
                            ;;~9tDimension value: ~w~%~
                            ;;~9tPattern value: ~w~%"
          instance
          dimension-name
          (or (nth-value 2 (function-lambda-expression op-fn)) op-fn)
          dimension-value
          pattern-value))

;;; ---------------------------------------------------------------------------
;;;  Syntax Reminder --
;;;
;;;  <unit-classes-spec> :== t | <single-class-spec> | (<single-class-spec>+)
;;;  <single-class-spec> :== <class-spec> | (<class-spec> <subclass-indicator>)
;;;  <class-spec> :==  <class> | <class-name>
;;;  <subclass-indicator> :==  :plus-subclasses | :no-subclasses | + | =
;;;
;;; ---------------------------------------------------------------------------

(defvar *memoized-atomic-unit-classes-check-functions*
  (make-et))

(defvar *memoized-atomic-unit-classes-lock*
    (make-lock :name "Memoized atomic unit classes"))

(defvar *memoized-plus-subclasses-check-functions*
  (make-hash-table :test 'equal))

(defvar *memoized-plus-subclasses-lock*
    (make-lock :name "Memoized plus subclasses"))

(defun determine-unit-class-check (unit-classes-spec)
  ;;; Return a function that returns true if an instance satisfies
  ;;; `unit-classes-spec'.
  (macrolet
      ((memoized-atomic-check-fn (unit-class closure)
         ;; `unit-class' is a simple form (not once-only'ed)
         `(with-full-optimization ()
            (or (get-et
                 ,unit-class
                 *memoized-atomic-unit-classes-check-functions*)
                ;; We lock only writes (at the slight risk of making an
                ;; extra closure):
                (with-lock-held (*memoized-atomic-unit-classes-lock*)
                  (setf (get-et
                         ,unit-class
                         *memoized-atomic-unit-classes-check-functions*)
                        ,closure)))))
       (memoized-plus-subclasses-check-fn (unit-classes closure)
         ;; `unit-classes' is a simple form (not once-only'ed)
         `(with-full-optimization ()
            (or (gethash
                 ,unit-classes
                 *memoized-plus-subclasses-check-functions*)
                ;; We lock only writes (at the slight risk of making an
                ;; extra closure):
                (with-lock-held (*memoized-plus-subclasses-lock*)
                  (setf (gethash
                         ,unit-classes
                         *memoized-plus-subclasses-check-functions*)
                        ,closure))))))
    (cond 
     ((eq unit-classes-spec 't)
      (memoized-atomic-check-fn 
       't
       #'(lambda (instance) 
           (declare (ignore instance)) 
           't)))
     ((atom unit-classes-spec)
      (memoized-atomic-check-fn 
       unit-classes-spec
       #'(lambda (instance)
           (eq (type-of instance) unit-classes-spec))))
     ((memq (second unit-classes-spec) '(:plus-subclasses +))
      (let ((unit-class (first unit-classes-spec)))
        (memoized-plus-subclasses-check-fn 
         unit-class
         #'(lambda (instance) 
             (locally #+(or cmu sbcl scl)
                      (declare (notinline typep))
                      (typep instance unit-class))))))
     ((memq (second unit-classes-spec) '(:no-subclasses =))
      (let ((unit-class (first unit-classes-spec)))
        (memoized-atomic-check-fn 
         unit-class
         #'(lambda (instance) 
             (eq (type-of instance) unit-class)))))
     ;; must be (<single-class-spec>+) -- unroll the easy case:
     ((every #'atom unit-classes-spec)
      (memoized-plus-subclasses-check-fn 
       unit-classes-spec
       #'(lambda (instance) 
           (memq (type-of instance) unit-classes-spec))))
     ;; (<single-class-spec>+) -- the general case:
     (t (with-full-optimization ()
          #'(lambda (instance)
              (let ((instance-type (type-of instance)))
                (flet ((fn (spec)
                         (cond ((atom spec)
                                (eq instance-type spec))
                               ((memq (second spec) '(:plus-subclasses +))
                                (locally #+(or cmu sbcl scl)
                                         (declare (notinline typep))
                                         (typep instance (first spec))))
                               ((memq (second spec) '(:no-subclasses =))
                                (eq instance-type (first spec)))
                               (t (ill-formed-unit-classes-spec unit-classes-spec)))))
                  (declare (dynamic-extent #'fn))
                  (some #'fn (the list unit-classes-spec))))))))))
  
;;; ===========================================================================
;;;  Pattern Operators

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-numeric-dispatch (match-op-name)
    (labels ((specific-match-op-name (suffix)
               (intern (concatenate 'simple-string 
                         (symbol-name match-op-name) suffix)
                       ':gbbopen))
             (do-type (suffix)
               `(return-from ,match-op-name
                  (,(specific-match-op-name suffix) 
                   instance-value pattern-value comparison-type))))
      `(case comparison-type
         ;; Ordered by likely frequency/effect:
         (fixnum ,(do-type "&"))
         (single-float ,(do-type "$"))
         (number)                       ; no dispatch on number
         (pseudo-probability ,(do-type "%"))
         (short-float ,(do-type "$&"))
         (double-float ,(do-type "$$"))
         (long-float ,(do-type "$$$"))))))

;;; ---------------------------------------------------------------------------

(macrolet
    ((generate-match-< (name number-test op &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,op instance-value pattern-value))
                    (t (,op instance-value (interval-start pattern-value)))))
             ;; interval instance-value and point pattern-value:
             ((,number-test pattern-value)
              (,op (interval-end instance-value) pattern-value))
             ;; interval instance-value and pattern-value:
             (t (,op (interval-end instance-value) 
                     (interval-start pattern-value))))))))
  (generate-match-< match-< numberp < :dispatching)
  (generate-match-< match-<= numberp <= :dispatching)
  (generate-match-< match-ends numberp = :dispatching)
  ;; declared fixnums:
  (generate-match-< match-<& fixnump <&)
  (generate-match-< match-<=& fixnump <=&)
  (generate-match-< match-ends& fixnump =&)
  ;; declared short-floats:  
  (generate-match-< match-<$& short-float-p <$&)
  (generate-match-< match-<=$& short-float-p <=$&)
  (generate-match-< match-ends$& short-float-p =$&)
  ;; declared single-floats:  
  (generate-match-< match-<$ single-float-p <$)
  (generate-match-< match-<=$ single-float-p <=$)
  (generate-match-< match-ends$ single-float-p =$)
  ;; declared double-floats:
  (generate-match-< match-<$$ double-float-p <$$)
  (generate-match-< match-<=$$ double-float-p <=$$)
  (generate-match-< match-ends$$ double-float-p =$$)
  ;; declared long-floats:
  (generate-match-< match-<$$$ long-float-p <$$$)
  (generate-match-< match-<=$$$ long-float-p <=$$$)
  (generate-match-< match-ends$$$ long-float-p =$$$)
  ;; declared pseudo-probabilities:
  (generate-match-< match-<% pseudo-probability-p <%)
  (generate-match-< match-<=% pseudo-probability-p <=%)
  (generate-match-< match-ends% pseudo-probability-p =%))

;;; ---------------------------------------------------------------------------

(macrolet 
    ((generate-match-> (name number-test op &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,op instance-value pattern-value))
                    (t (,op instance-value (interval-end pattern-value)))))
             ;; interval instance-value and point pattern-value:
             ((,number-test pattern-value)
              (,op (interval-start instance-value) pattern-value))
             ;; interval instance-value and pattern-value:
             (t (,op (interval-start instance-value) 
                     (interval-end pattern-value))))))))
  (generate-match-> match-> numberp > :dispatching)
  (generate-match-> match->= numberp >= :dispatching)
  (generate-match-> match-starts numberp = :dispatching)
  ;; declared fixnums:
  (generate-match-> match->& fixnump >&)
  (generate-match-> match->=& fixnump >=&)
  (generate-match-> match-starts& fixnump =&)
  ;; declared short-floats:
  (generate-match-> match->$& short-float-p >$&)
  (generate-match-> match->=$& short-float-p >=$&)
  (generate-match-> match-starts$& short-float-p =$&)
  ;; declared single-floats:
  (generate-match-> match->$ single-float-p >$)
  (generate-match-> match->=$ single-float-p >=$)
  (generate-match-> match-starts$ single-float-p =$)
  ;; declared double-floats:
  (generate-match-> match->$$ double-float-p >$$)
  (generate-match-> match->=$$ double-float-p >=$$)
  (generate-match-> match-starts$$ double-float-p =$$)
  ;; declared long-floats:
  (generate-match-> match->$$$ long-float-p >$$$)
  (generate-match-> match->=$$$ long-float-p >=$$$)
  (generate-match-> match-starts$$$ long-float-p =$$$)
  ;; declared pseudo-probabilities:
  (generate-match-> match->% pseudo-probability-p >%)
  (generate-match-> match->=% pseudo-probability-p >=%)
  (generate-match-> match-starts% pseudo-probability-p =%))

;;; ---------------------------------------------------------------------------

(macrolet 
    ((generate-match-within (name number-test point-op range-op
                             &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,point-op ,range-op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,point-op instance-value pattern-value))
                    (t (multiple-value-bind (pstart pend)
                           (interval-values pattern-value)
                         (,range-op pstart instance-value pend)))))
             ;; interval instance-value point pattern-value:
             ((,number-test pattern-value)
              (multiple-value-bind (istart iend)
                  (interval-values instance-value)
                (,point-op istart pattern-value iend)))
             ;; interval instance-value and pattern-value:
             (t (multiple-value-bind (istart iend)
                    (interval-values instance-value)
                  (multiple-value-bind (pstart pend)
                      (interval-values pattern-value)
                    (,range-op pstart istart iend pend)))))))))
  (generate-match-within match-= numberp = = :dispatching)
  (generate-match-within match-within numberp = <= :dispatching)
  ;; declared fixnums:
  (generate-match-within match-=& fixnump =& =&)
  (generate-match-within match-within& fixnump =& <=&)
  ;; declared short-floats:
  (generate-match-within match-=$& short-float-p =$& =$&)
  (generate-match-within match-within$& short-float-p =$& <=$&)
  ;; declared single-floats:
  (generate-match-within match-=$ single-float-p =$ =$)
  (generate-match-within match-within$ single-float-p =$ <=$)
  ;; declared double-floats:
  (generate-match-within match-=$$ double-float-p =$$ =$$)
  (generate-match-within match-within$$ double-float-p =$$ <=$$)
  ;; declared long-floats:
  (generate-match-within match-=$$$ long-float-p =$$$ =$$$)
  (generate-match-within match-within$$$ long-float-p =$$$ <=$$$)
  ;; declared pseudo-probabilities:
  (generate-match-within match-=% pseudo-probability-p =% =%)
  (generate-match-within match-within% pseudo-probability-p =% <=%))

;;; ---------------------------------------------------------------------------

(macrolet 
    ((generate-match-covers (name number-test point-op range-op
                             &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,point-op ,range-op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,point-op instance-value pattern-value))
                    (t (multiple-value-bind (pstart pend)
                           (interval-values pattern-value)
                         (,point-op pstart instance-value pend)))))
             ;; interval instance-value point pattern-value:
             ((,number-test pattern-value)
              (multiple-value-bind (istart iend)
                  (interval-values instance-value)
                (,range-op istart pattern-value iend)))              
             ;; interval instance-value and pattern-value:
             (t (multiple-value-bind (istart iend)
                    (interval-values instance-value)
                  (multiple-value-bind (pstart pend)
                      (interval-values pattern-value)
                    (,range-op istart pstart pend iend)))))))))
  (generate-match-covers match-covers numberp = <= :dispatching)
  ;; declared fixnums:
  (generate-match-covers match-covers& fixnump =& <=&)
  ;; declared short-floats:
  (generate-match-covers match-covers$& short-float-p =$& <=$&)
  ;; declared single-floats:
  (generate-match-covers match-covers$ single-float-p =$ <=$)
  ;; declared double-floats:
  (generate-match-covers match-covers$$ double-float-p =$$ <=$$)
  ;; declared long-floats:
  (generate-match-covers match-covers$$$ long-float-p =$$$ <=$$$)
  ;; declared pseudo-probabilities:
  (generate-match-covers match-covers% pseudo-probability-p =% <=%))

;;; ---------------------------------------------------------------------------

(macrolet 
    ((generate-match-overlaps (name number-test point-op range-op sub-op
                               &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,point-op ,range-op ,sub-op))))
            ,@(when (eq number-test 'fixnump)
                `((declare (notinline ,range-op ,sub-op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,point-op instance-value pattern-value))
                    (t (multiple-value-bind (pstart pend)
                           (interval-values pattern-value)
                         (,range-op pstart instance-value pend)))))
             ;; interval instance-value point pattern-value:
             ((,number-test pattern-value) 
              (multiple-value-bind (istart iend)
                  (interval-values instance-value)
                (,range-op istart pattern-value iend)))
             ;; interval instance-value and pattern-value:
             (t (multiple-value-bind (istart iend)
                    (interval-values instance-value)
                  (multiple-value-bind (pstart pend)
                      (interval-values pattern-value)
                    ;; eliminate special cases by converting check
                    ;; to a point within an expanded interval:
                    (,range-op 
                     ;; on CLs without infinity extensions, we must
                     ;; check for overflow/underflow explicitly
                     #+infinity-not-available
                     (if (or (= iend infinity)
                             (= istart -infinity)
                             (= pstart -infinity))
                         -infinity
                         (,sub-op pstart (,sub-op iend istart))) 
                     #-infinity-not-available
                     (,sub-op pstart (,sub-op iend istart)) 
                     istart 
                     pend)))))))))
  (generate-match-overlaps match-overlaps numberp = <= - :dispatching)
  ;; declared fixnums (NOTE: we can't use -& as sub-op and <=& as range-op, 
  ;; due to potential bignum overflow):
  ;; (generate-match-overlaps match-overlaps& fixnump =& <=& -&)
  (generate-match-overlaps match-overlaps& fixnump =& <= -)
  ;; declared short-floats:
  (generate-match-overlaps match-overlaps$& short-float-p =$& <=$& -$&)
  ;; declared single-floats:
  (generate-match-overlaps match-overlaps$ single-float-p =$ <=$ -$)
  ;; declared double-floats:
  (generate-match-overlaps match-overlaps$$ double-float-p =$$ <=$$ -$$)
  ;; declared long-floats:
  (generate-match-overlaps match-overlaps$$$ long-float-p =$$$ <=$$$ -$$$)
  ;; declared pseudo-probabilities:
  (generate-match-overlaps match-overlaps% pseudo-probability-p =% <=% -%))

;;; ---------------------------------------------------------------------------
;;;    THIS ONE IS UNDER CONSTRUCTION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(macrolet 
    ((generate-match-abuts (name number-test op &optional dispatching)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            ,@(when (eq number-test 'numberp)
                `((declare (notinline ,op))))
            ,(cond
              ;; If dispatching, generate the dispatch code:
              (dispatching (generate-numeric-dispatch name))
              ;; Non-dispatching (declared type) operators:
              (t '(declare (ignore comparison-type))))
            (cond 
             ;; point instance-value:
             ((,number-test instance-value)
              (cond ((,number-test pattern-value)
                     (,op instance-value pattern-value))
                    (t (or (,op instance-value (interval-start pattern-value))
                           (,op instance-value (interval-end pattern-value))))))
             ;; interval instance-value and point pattern-value:
             ((,number-test pattern-value)
              (or (,op (interval-end instance-value) pattern-value)
                  (,op (interval-end instance-value) pattern-value)))
             ;; interval instance-value and pattern-value:
             (t (or (,op (interval-end instance-value) 
                         (interval-start pattern-value))
                    (,op (interval-start instance-value) 
                         (interval-end pattern-value)))))))))
  (generate-match-abuts match-abuts numberp = :dispatching)
  (generate-match-abuts match-abuts& fixnump =&)
  ;; declared short-floats:
  (generate-match-abuts match-abuts$& short-float-p =$&)
  ;; declared single-floats:
  (generate-match-abuts match-abuts$ single-float-p =$)
  ;; declared double-floats:
  (generate-match-abuts match-abuts$$ double-float-p =$$)
  ;; declared long-floats:
  (generate-match-abuts match-abuts$$$ long-float-p =$$$)
  ;; declared pseudo-probabilities:
  (generate-match-abuts match-abuts% pseudo-probability-p =%))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun match-is (instance-value pattern-value comparison-type)
    ;; Inhibit CMUCL and SCL optimization warnings:
    #+(or cmu scl)
    (declare (optimize (extensions:inhibit-warnings 3)))
    ;; general enumerated match operator:
    (funcall (fdefinition comparison-type) instance-value pattern-value)))
           
;;; ---------------------------------------------------------------------------

(macrolet
    ((generate-match-is (name comparison-test)
       `(with-full-optimization ()
          (defun ,name (instance-value pattern-value comparison-type)
            (declare (ignore comparison-type))
            (without-cmucl/sbcl/scl-optimization-warnings
             (,comparison-test instance-value pattern-value))))))
  (generate-match-is match-is-eq eq)
  (generate-match-is match-is-eql eql)
  (generate-match-is match-is-equal equal)
  (generate-match-is match-is-equalp equalp))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun match-true (instance-value there-is-no-pattern-value 
                     comparison-type)
    (declare (ignore comparison-type))
    (declare (ignore there-is-no-pattern-value))
    ;; Boolean match functions do not involve a pattern value:
    instance-value))
           
;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun match-false (instance-value there-is-no-pattern-value
                      comparison-type)
    (declare (ignore comparison-type))
    (declare (ignore there-is-no-pattern-value))
    ;; Boolean match functions do not involve a pattern value:
    (not instance-value)))
           
;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun match-eqv (instance-value pattern-value comparison-type)
    (declare (ignore comparison-type))
    ;; fast (not (xor instance-value pattern-value)):
    (if instance-value pattern-value (not pattern-value))))
           
;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun match-matches (instance-value pattern-value comparison-type)
    (declare (ignore comparison-type))
    ;; placeholder for general string match operator:
    (nyi instance-value pattern-value)))
           
;;; ---------------------------------------------------------------------------
;;;  Extent operators

(with-full-optimization ()
  (defun make-extent (start end)
    (cons start end)))

(defcm make-extent (start end)
  `(cons ,start ,end))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro destructure-extent ((start end) form &body body)
    `(destructuring-bind (,start . ,end)
         ,form
       ,@body)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun extent-start (extent)
    (car (the cons extent))))

(defcm extent-start (extent)
  `(car (the cons ,extent)))

(defsetf extent-start #.(first (nth-value 3 (get-setf-expansion '(car x)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun extent-end (extent)
    (cdr (the cons extent))))

(defcm extent-end (extent)
  `(cdr (the cons ,extent)))

(defsetf extent-end #.(first (nth-value 3 (get-setf-expansion '(cdr x)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun extent-< (value)
    (cons -infinity value)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun extent-> (value)
    (cons value infinity)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun extent-/= (value)
    (declare (ignore value))
    infinite-extent))

;;; ---------------------------------------------------------------------------

(defparameter *ordered-match-op-fns*
    ;; Ordered by expected declaration-type frequency:
    `((= ,#'match-=)
      (< ,#'match-< ,#'extent-<)
      (> ,#'match-> ,#'extent->)
      (<= ,#'match-<= ,#'extent-<)
      (>= ,#'match->= ,#'extent->)
      (/= ,(complement #'match-=) ,#'extent-/=)
      (within ,#'match-within)
      (covers ,#'match-covers)
      (overlaps ,#'match-overlaps)
      (starts ,#'match-starts) ; ,#'extent->)
      (ends ,#'match-ends) ; ,#'extent-<)
      (abuts ,#'match-abuts)
      ;; declared fixnums:
      (=& ,#'match-=&)
      (<& ,#'match-<& ,#'extent-<)
      (>& ,#'match->& ,#'extent->)
      (<=& ,#'match-<=& ,#'extent-<)
      (>=& ,#'match->=& ,#'extent->)
      (/=& ,(complement #'match-=&) ,#'extent-/=)
      (within& ,#'match-within&)
      (covers& ,#'match-covers&)
      (overlaps& ,#'match-overlaps&)
      (starts& ,#'match-starts&) ; ,#'extent->)
      (ends& ,#'match-ends&) ; ,#'extent-<)
      (abuts& ,#'match-abuts&)
      ;; declared single-floats:
      (=$ ,#'match-=$)
      (<$ ,#'match-<$ ,#'extent-<)
      (>$ ,#'match->$ ,#'extent->)
      (<=$ ,#'match-<=$ ,#'extent-<)
      (>=$ ,#'match->=$ ,#'extent->)
      (/=$ ,(complement #'match-=$) ,#'extent-/=)
      (within$ ,#'match-within$)
      (covers$ ,#'match-covers$)
      (overlaps$ ,#'match-overlaps$)
      (starts$ ,#'match-starts$) ; ,#'extent->)
      (ends$ ,#'match-ends$) ; ,#'extent-<)
      (abuts$ ,#'match-abuts$)
      ;; declared pseudo-probabilities:
      (=% ,#'match-=%)
      (<% ,#'match-<% ,#'extent-<)
      (>% ,#'match->% ,#'extent->)
      (<=% ,#'match-<=% ,#'extent-<)
      (>=% ,#'match->=% ,#'extent->)    
      (/=% ,(complement #'match-=%) ,#'extent-/=)
      (within% ,#'match-within%)
      (covers% ,#'match-covers%)
      (overlaps% ,#'match-overlaps%)
      (starts% ,#'match-starts%) ; ,#'extent->)
      (ends% ,#'match-ends%) ; ,#'extent-<)
      (abuts% ,#'match-abuts%)
      ;; declared short-floats 
      (=$& ,#'match-=$&)
      (<$& ,#'match-<$& ,#'extent-<)
      (>$& ,#'match->$& ,#'extent->)
      (<=$& ,#'match-<=$& ,#'extent-<)
      (>=$& ,#'match->=$& ,#'extent->)
      (/=$& ,(complement #'match-=$&) ,#'extent-/=)
      (within$& ,#'match-within$&)
      (covers$& ,#'match-covers$&)
      (overlaps$& ,#'match-overlaps$&)
      (starts$& ,#'match-starts$&) ; ,#'extent->)
      (ends$& ,#'match-ends$&) ; ,#'extent-<)
      (abuts$& ,#'match-abuts$&)
      ;; declared double-floats:
      (=$$ ,#'match-=$$)
      (<$$ ,#'match-<$$ ,#'extent-<)
      (>$$ ,#'match->$$ ,#'extent->)
      (<=$$ ,#'match-<=$$ ,#'extent-<)
      (>=$$ ,#'match->=$$ ,#'extent->)    
      (/=$$ ,(complement #'match-=$$) ,#'extent-/=)
      (within$$ ,#'match-within$$)
      (covers$$ ,#'match-covers$$)
      (overlaps$$ ,#'match-overlaps$$)
      (starts$$ ,#'match-starts$$) ; ,#'extent->)
      (ends$$ ,#'match-ends$$) ; ,#'extent-<)
      (abuts$$ ,#'match-abuts$$)
      ;; declared long-floats:
      (=$$$ ,#'match-=$$$)
      (<$$$ ,#'match-<$$$ ,#'extent-<)
      (>$$$ ,#'match->$$$ ,#'extent->)
      (<=$$$ ,#'match-<=$$$ ,#'extent-<)
      (>=$$$ ,#'match->=$$$ ,#'extent->)    
      (/=$$$ ,(complement #'match-=$$$) ,#'extent-/=)
      (within$$$ ,#'match-within$$$)
      (covers$$$ ,#'match-covers$$$)
      (overlaps$$$ ,#'match-overlaps$$$)
      (starts$$$ ,#'match-starts$$$) ; ,#'extent->)
      (ends$$$ ,#'match-ends$$$) ; ,#'extent-<)
      (abuts$$$ ,#'match-abuts$$$)))

;;; ---------------------------------------------------------------------------

(defparameter *enumerated-match-op-fns*
    ;; TODO: finish moving from having to specify the match type in the pattern
    ;;       operator to the generic IS enumerated operator:
    `((is ,#'match-is)
      ;; Strong-type matches:
      (is-eq ,#'match-is-eq)
      (is-eql ,#'match-is-eql)
      (is-equal ,#'match-is-equal)
      (is-equalp ,#'match-is-equalp)
      ;; Deprecated strong-type matches:
      (eq ,#'match-is-eq)
      (eql ,#'match-is-eql)
      (equal ,#'match-is-equal)
      (equalp ,#'match-is-equalp)))

;;; ---------------------------------------------------------------------------

(defparameter *string-match-op-fns*
    `((matches ,#'match-matches)))

;;; ---------------------------------------------------------------------------

(defparameter *boolean-match-op-fns*
    `((true ,#'match-true)
      (false ,#'match-false)
      (eqv ,#'match-eqv)))

;;; ===========================================================================
;;;  Pattern Matching

(defun match-instance-to-pattern-element (instance pattern-element into-cons
                                          verbose)
  ;;; Returns true if `instance' matches `pattern-element'
  (destructuring-bind (op-fn dimensions &optional values &rest options)
      pattern-element
    (declare (type function op-fn))
    (declare (ignore options))
    (declare (dynamic-extent options))
    ;; Do an extended "every-like" iteration (supporting atoms,
    ;; lists, dotted-lists, or sequences with appropriate length
    ;; checks based on the structure of dimensions).  Just using:
    ;;
    ;;       (every #'(lambda (dimension value)
    ;;         (funcall op-fn
    ;;                  (instance-dimension-value instance dimension)
    ;;                    value))
    ;;         dimensions
    ;;         values)
    ;;
    ;; comes close, but we require a bit more flexibility.
    (flet ((match-it (dimension-name pattern-value)
             (multiple-value-bind (dimension-value dimension-value-type
                                   comparison-type
                                   composite-type ordering-dimension-name)
                 (internal-instance-dimension-value 
                  instance dimension-name into-cons)
               (declare (ignore dimension-value-type ordering-dimension-name))
               (let ((result 
                      (cond 
                       ;; unbound value:
                       ((eq dimension-value unbound-value-indicator)
                        nil)
                       ;; set-composite dimension value:
                       ((eq composite-type ':set)
                        (flet ((fn (component-dimension-value)
                                 (funcall op-fn 
                                          component-dimension-value
                                          pattern-value
                                          comparison-type)))
                          (declare (dynamic-extent #'fn))
                          (some #'fn dimension-value)))
                       ;; sequence-composite dimension value:
                       ((eq composite-type ':sequence)
                        (nyi))
                       ;; series-composite dimension value:
                       ((eq composite-type ':series)
                        (nyi))
                       ;; incomposite dimension value:
                       (t (funcall op-fn
                                   dimension-value
                                   pattern-value
                                   comparison-type)))))
                 (cond 
                  ;; success on this dimension:
                  (result 't)
                  ;; failure
                  (t (when verbose
                       (find-verbose-dimension-failure 
                        instance dimension-name op-fn
                        pattern-value dimension-value))
                     ;; return failure:
                     nil))))))
      (cond 
       ;; atomic dimension requires a single value:
       ((symbolp dimensions)
        (match-it dimensions values))
       ;; list dimensions with non-list values:
       ((vectorp values)
        (let ((index 0)
              (dimension-list dimensions))
          (declare (type list dimension-list))
          (while (consp dimension-list)
            (unless (match-it (pop dimension-list) (elt values index))
              ;; failed on this dimension:
              (return-from match-instance-to-pattern-element nil))
            (incf& index)))
        ;; success:
        't)
       ;; list/dotted-list dimension values:
       (t (let ((dimension-list dimensions))
            (declare (type list dimension-list))
            (while (consp dimension-list)
              (unless (match-it
                       (pop dimension-list)
                       ;; handle pure and dotted-list values:
                       (cond ((consp values) (pop values))
                             ((null values)
                              (error "Too few pattern values supplied."))
                             (t (prog1 values (setf values nil)))))
                ;; failed on this dimension:
                (return-from match-instance-to-pattern-element nil))))
          ;; success:
          't)))))
                        
;;; ---------------------------------------------------------------------------

(defun match-instance-to-pattern (instance pattern into-cons verbose)
  ;;; Returns true if `instance' matches `pattern'
  (when verbose (find-verbose-begin-match instance))  
  (let ((result
         (cond 
          ((eq pattern 't) 't)
          (t (labels 
                 ((match-instance-to-simpler-pattern (p)
                    (case (car p)
                      (not 
                       (not (match-instance-to-simpler-pattern (second p))))
                      (and 
                       (every #'match-instance-to-simpler-pattern (cdr p)))
                      (or 
                       (some #'match-instance-to-simpler-pattern (cdr p)))
                      (otherwise
                       (match-instance-to-pattern-element
                        instance p into-cons verbose)))))
               (match-instance-to-simpler-pattern pattern))))))
    (when verbose
      (if result
          (find-verbose-match-success instance)
          (find-verbose-match-failure instance)))
    result))

;;; ===========================================================================
;;;  Pattern Optimization

(defstruct (optimized-pattern
            (:conc-name #.(dotted-conc-name 'optimized-pattern))
            (:copier nil))
  (disjunctive-dimensional-extents nil)
  (pattern nil))

;;; ---------------------------------------------------------------------------

(defun convert-to-dnf (pattern)
  ;;; A simple disjunctive-normal-form converter
  (let ((original-pattern pattern))
    (labels
        ((equivalent-clause-p (clause-1 clause-2)
           (cond
            ((or (atom clause-1) (atom clause-2))
             (equal clause-1 clause-2))
            ((and (eq (first clause-1) 'not)
                  (eq (first clause-2) 'not))
             (equal (second clause-1) (second clause-2)))
            ((and (eq (first clause-1) 'or)
                  (eq (first clause-2) 'or))
             (set-equal (rest clause-1) (rest clause-2)))
            ((and (eq (first clause-1) 'and)
                  (eq (first clause-2) 'and))
             (set-equal (rest clause-1) (rest clause-2)))))
         (transform (pattern)
           (cond 
            ((consp pattern)
             (case (car pattern)
               ;; Handle NOT clause:
               (not 
                (cond 
                 ;; (not) is an error:
                 ((null (cdr pattern))
                  (error "Empty ~s clause encountered in pattern: ~s"
                         'not original-pattern))
                 ;; (not X Y) is an error:
                 ((cddr pattern)
                  (error "~s clause ~s contains more than 1 subclause in ~
                         pattern: ~s"
                         'not pattern original-pattern))
                 ;; (not (not X) => X: 
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'not))
                  (transform (second (second pattern))))
                 ;; (not (and X .. Y)) => (or (not X) .. (not Y)): 
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'and))
                  `(or ,.(mapcar #'(lambda (p) (transform `(not ,p)))
                                 (cdr (second pattern)))))
                 ;; (not (or X .. Y)) => (and (not X) .. (not Y)): 
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'or))
                  `(and ,.(mapcar #'(lambda (p) (transform `(not ,p)))
                                  (cdr (second pattern)))))
                 ;; simple (but not "square"...!) not:
                 (t `(not ,(transform (second pattern))))))

               ;; Handle OR clause:
               (or 
                (cond 
                 ;; (or) is an error:
                 ((null (cdr pattern))
                  (error "Empty ~s clause encountered in pattern: ~s"
                         'or original-pattern))              
                 ;; (or X) => X:
                 ((null (cddr pattern))
                  (transform (second pattern)))
                 ;; (or (or A .. B) C .. D) => (or A .. B C .. D):
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'or))
                  (transform `(or ,@(cdr (second pattern))
                                  ,@(cddr pattern))))
                 ;; (or .. (or ..) ..) => (or (or ..) ..):
                 ((let* ((p (cdr pattern))
                         (pos (position-if 
                               #'(lambda (element)
                                   (and (consp element)
                                        (eq (car element) 'or)))
                               (the list p))))
                    (when pos
                      (transform `(or ,(nth pos p)
                                      ,.(subseq p 0 pos)
                                      ,.(subseq p (1+& pos)))))))
                 ;; no additional transforms for (or ...), but delete
                 ;; duplicate equivalent or clauses:
                 (t `(or ,.(delete-duplicates 
                            (mapcar #'transform (cdr pattern))
                            :test #'equivalent-clause-p)))))

               ;; Handle AND clause:
               (and        
                (cond 
                 ;; (and) is an error:
                 ((null (cdr pattern))
                  (error "Empty ~s clause encountered in pattern: ~s"
                         'and original-pattern))              
                 ;; (and X) => X:
                 ((null (cddr pattern))
                  (transform (second pattern)))            
                 ;; (and (and A .. B) C .. D) => (and A .. B C .. D):
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'and))
                  (transform `(and ,@(cdr (second pattern))
                                   ,@(cddr pattern))))
                 ;; (and (or A .. B) C .. D) => 
                 ;;    (or (and A C .. D) .. (and B C .. D)):
                 ((and (consp (second pattern))
                       (eq (car (second pattern)) 'or))
                  (transform 
                   `(or ,.(mapcar #'(lambda (p)
                                      `(and ,p ,@(cddr pattern)))
                                  (cdr (second pattern))))))
                 ;; (and .. (and ..) ..) => (and (and ..) ..)
                 ;; (and .. (or ..) ..) => (and (or ..) ..):
                 ((let* ((p (cdr pattern))
                         (pos (flet ((fn (element)
                                       (and (consp element)
                                            (or (eq (car element) 'and)
                                                (eq (car element) 'or)))))
                                (declare (dynamic-extent #'fn))
                                (position-if #'fn (the list p)))))
                    (when pos
                      (transform `(and ,(nth pos p)
                                       ,.(subseq p 0 pos)
                                       ,.(subseq p (1+& pos)))))))
                 ;; no additional transforms for (and ...), but delete
                 ;; duplicate equivalent and clauses:
                 (t `(and ,.(delete-duplicates 
                             (mapcar #'transform (cdr pattern))
                             :test #'equivalent-clause-p)))))

               ;; Handle a simple cons pattern element:
               (otherwise pattern)))

            ;; Handle a simple atomic pattern element:
            (t pattern))))
      ;; Iteratively transform pattern until no further transformations
      ;; apply:
      (loop
        (let ((maybe-dnf (transform pattern)))
          (when (equal maybe-dnf pattern)
            (return pattern))
          ;; try again:
          (setf pattern maybe-dnf))))))

;;; ---------------------------------------------------------------------------

(defun infeasible-pattern (extents)
  (when (and *warn-about-unusual-requests*
             (not (memq ':infeasible extents)))
    (if (eq *%%pattern%%* *%%pattern-clause%%*)
        (warn "Pattern ~w ~_can not be satisfied." 
              *%%pattern%%*)
        (warn "The clause ~w ~_in pattern ~w ~_can not be satisfied." 
              *%%pattern-clause%%* *%%pattern%%*))))

;;; ---------------------------------------------------------------------------

(defun merge-boolean-extent (new-extent extents)
  ;;; Merges a disjunctive ordered `new-extent' with `extents'
  ;;;
  ;;; This function is here for symmetry and possible use in the future.  The
  ;;; current pattern-optimization strategy will never call it, as each or is
  ;;; processed using separate extents.
  (if (memq new-extent extents) 
      extents
      (cons new-extent extents)))

;;; ---------------------------------------------------------------------------

(defun intersect-boolean-extent (new-extent extents)
  ;;; Intersects a conjunctive boolean `new-extent' with `extents'
  (cond 
   ((null extents) (list new-extent))
   ((memq new-extent extents) (list new-extent))
   (t (infeasible-pattern extents)
      (list ':infeasible))))

;;; ---------------------------------------------------------------------------

(defun merge-ordered-extent (new-extent extents)
  ;;; Merges a disjunctive ordered `new-extent' with `extents'
  ;;;
  ;;; This function is called by determine-storage-regions. 
  ;;;
  ;;; It is not currently called by add-dimension-extent, because the current
  ;;; pattern-optimization strategy will never call it, as each or is
  ;;; processed using separate extents.
  (if (eq new-extent ':infeasible) 
      extents
      (destructure-extent (new-start new-end)
          new-extent
        (cond 
         ((null extents) (list new-extent))
         (t (destructure-extent (existing-start existing-end)
                (first extents)
              (cond 
               ;; `new-extent' ends before the first extent:
               ((< new-end existing-start)
                (cons new-extent extents))
               ;; `new-extent' overlaps the first extent:
               ((<= new-start existing-end)
                ;; Extend first extent earlier:
                (when (< new-start existing-start)
                  (setf (extent-start (first extents)) new-start))
                ;; extend first extent later (possibly merging):
                (let ((updated-start (extent-start (first extents))))
                  (while (> new-end existing-end)
                    ;; extension without merge:
                    (when (or (null (rest extents)) ; no more extents?
                              (< new-end (extent-start (second extents))))
                      (setf (car extents) (make-extent updated-start new-end))
                      (setf (extent-end (first extents)) new-end)
                      (return))
                    (pop extents)))
                ;; existing extents are now correct:
                extents)
               ;; `new-extent' follows the first extent:
               (t (cons (first extents) 
                        (merge-ordered-extent
                         new-extent (rest extents)))))))))))
  
;;; ---------------------------------------------------------------------------

(defun intersect-ordered-extent (new-extent extents)
  ;;; Intersects a conjunctive ordered `new-extent' with `extents'
  (destructure-extent (new-start new-end)
      new-extent
    (cond 
     ((equal extents ':infeasible) extents)
     (t (let ((new-extents nil))
          (dolist (extent extents)
            (destructure-extent (existing-start existing-end)
                extent
              (unless (or (< new-end existing-start)
                          (> new-start existing-end))
                ;; `new-extent' overlaps the  extent:
                (when (> new-start existing-start)
                  (setf (extent-start extent) new-start))
                ;; end extent earlier:
                (when (< new-end existing-end)
                  (setf (extent-end extent) new-end))
                ;; first extent is now correct:
                (push extent new-extents))))
          (cond (new-extents
                 (nreverse new-extents))
                (t (infeasible-pattern extents)
                   (list ':infeasible))))))))

;;; ---------------------------------------------------------------------------

(defun add-dimension-extent (dimension dimension-type value anding negated)
  ;;; The current pattern-optimization strategy will never call the
  ;;; extent-merging functions, as each or is processed using separate
  ;;; extents.  They are here for symmetry and possible use in the future.
  (case dimension-type
    (:ordered
     (let ((extent-acons (assq dimension *%%pattern-extents%%*))
           (new-extents (if (numberp value)
                            (if negated 
                                ;; (not <point>) requires looking everywhere!
                                `(,infinite-extent)
                                `(,(make-extent value value)))
                            (multiple-value-bind (start end)
                                (interval-values value)
                              (if negated 
                                  ;; Separate dimension handling can't address
                                  ;; multiple-dimension negation (a "hole" in
                                  ;; a space.  The following only scans the
                                  ;; lower-right and upper-left quadrants
                                  ;; bounding the "hole".
                                  #+wrong
                                  `(,(make-extent -infinity start) 
                                    ,(make-extent end infinity))
                                  ;; To fix this issue, we need to consider
                                  ;; each disjunct in all pattern dimensions
                                  ;; (not individually.  For now, we'll look
                                  ;; everywhere!
                                  #-wrong
                                  `(,infinite-extent)
                                  `(,(make-extent start end)))))))
       (cond 
        ;; update existing extent for `dimension':
        (extent-acons
         (dolist (new-extent new-extents)
           (setf (cddr extent-acons) 
                 (funcall (if anding 
                              #'intersect-ordered-extent 
                              #'merge-ordered-extent)
                          new-extent 
                          (cddr extent-acons)))))
        ;; new extent:
        (t (setf *%%pattern-extents%%*
             ;; dimension-name order is only for human readers, but adds
             ;; little time penalty:
             (nsorted-insert 
              (cons dimension (cons dimension-type new-extents))
              *%%pattern-extents%%*
              #'string< #'car))))))
    (:enumerated
     ;; record that we have an enumerated dimension search:
     (pushnew-acons dimension (cons dimension-type value) 
                    *%%pattern-extents%%*))
    (:boolean
     (let ((extent-acons (assq dimension *%%pattern-extents%%*))
           (new-extent (if negated 
                           (if value 'false 'true)
                           (if value 'true 'false))))
       (cond 
        ;; update existing extent for `dimension':
        (extent-acons
         (setf (cddr extent-acons) 
               (funcall (if anding 
                            #'intersect-boolean-extent 
                            #'merge-boolean-extent)
                        new-extent 
                        (cddr extent-acons))))
        ;; new extent:
        (t (setf *%%pattern-extents%%*
             (acons dimension 
                    (cons dimension-type (list new-extent))
                    *%%pattern-extents%%*))))))))

;;; ---------------------------------------------------------------------------

(defun lookup-pattern-operator-function (operator pattern-element)
  ;;; Returns 3 values:
  ;;;  1. the pattern-operator function of `operator' 
  ;;;  2. the dimension-value type of `operator'
  ;;;  3. an extent-generator function for an :ordered pattern operator
  (let ((ordered-op-spec
         (cdr (assq operator *ordered-match-op-fns*))))
    (if ordered-op-spec
        (destructuring-bind (ordered-op-fn 
                             &optional extent-generator-fn)
            ordered-op-spec
          (values ordered-op-fn ':ordered extent-generator-fn))
        (let ((enumerated-op-fn 
               (second (assq operator *enumerated-match-op-fns*))))
          (if enumerated-op-fn
              (values enumerated-op-fn ':enumerated)
              (let ((boolean-op-fn 
                     (second (assq operator *boolean-match-op-fns*))))
                (if boolean-op-fn
                    (values boolean-op-fn ':boolean)
                    (error "Illegal pattern operator ~s in pattern element ~s."
                           operator pattern-element))))))))

;;; ---------------------------------------------------------------------------

(defun optimize-pattern-element (pattern-element anding negated)
  (destructuring-bind (operator dimensions 
                       &optional (values nil values-supplied)
                       &rest options)
      pattern-element
    (multiple-value-bind (op-fn dimension-type extent-generator-fn)
        (lookup-pattern-operator-function operator pattern-element)
      ;; Do an extended "every-like" iteration (supporting atoms,
      ;; lists, dotted-lists, or sequences with appropriate length
      ;; checks based on the structure of dimensions).  Just using:
      ;;
      ;;       (every #'(lambda (dimension value)
      ;;         (funcall op-fn
      ;;                  (instance-dimension-value instance dimension)
      ;;                    value))
      ;;         dimensions
      ;;         values)
      ;;
      ;; comes close, but we need a bit more flexibility and meaningful
      ;; error checking.
      (flet ((too-many-values-error ()
               (error "Too many dimension values were supplied for pattern ~
                       dimensions ~s: ~s"
                      dimensions
                      values))
             (too-few-values-error ()
               (error "Not enough dimension values were supplied for pattern ~
                       dimensions ~s: ~s"
                      dimensions
                      values))
             (dotted-pattern-error ()
               (error "Pattern dimensions ~s is a dotted list." dimensions)))
        (cond
         ;; boolean unary-patterns don't have a value:
         ((and (eq dimension-type ':boolean)
               (or (eq operator 'true) (eq operator 'false)))
          (when values-supplied
            (push values options))
          (setf values (if (eq operator 'true) 't nil)))
         ;; for all other dimensions, check that values were supplied: 
         (t (unless values-supplied
              (error "No dimension values were supplied for pattern ~
                      dimensions ~s"
                     dimensions))))
        (dolist (option options)
          ;; none implemented yet!
          (error "Unsupported pattern option: ~s" option))      
        (cond 
         ;; atomic dimension requires a single value
         ((symbolp dimensions)
          (add-dimension-extent 
           dimensions dimension-type 
           (if extent-generator-fn
               (funcall (the function extent-generator-fn) values)
               values)
           anding negated))
         ;; list dimensions with non-list values:
         ((vectorp values)
          (let ((index 0)
                (dims dimensions))
            (while (consp dims)
              ;; length test is fast on vectors!
              (when (<& (1-& (length values)) index)
                (too-few-values-error))
              (let ((value (elt values index)))
                (add-dimension-extent 
                 (pop dims) dimension-type 
                 (if extent-generator-fn
                     (funcall (the function extent-generator-fn) value)
                     value)
                 anding negated))
              (incf& index))
            ;; dotted dimensions are not allowed!
            (when dims (dotted-pattern-error))             
            ;; Too many dimension values provided:
            (unless (=& (length values) index)
              (too-many-values-error))))
         ;; list/dotted-list dimension values:
         (t (let ((dims dimensions)
                  (vals values))
              (while (consp dims)
                (let ((value (cond ((consp vals) (pop vals))
                                   ((null vals) (too-few-values-error))
                                   (t (prog1 vals (setf vals nil))))))
                  (add-dimension-extent
                   (pop dims)
                   dimension-type
                   (if extent-generator-fn
                       (funcall (the function extent-generator-fn) value)
                       value)
                   anding 
                   negated)))
              ;; dotted dimensions are not allowed!
              (when dims (dotted-pattern-error))             
              ;; Too many dimension values provided:
              (when vals (too-many-values-error))))))
      (cons op-fn (cdr pattern-element)))))

;;; ---------------------------------------------------------------------------

(defun optimize-pattern (pattern)
  ;;; Returns an optimized-pattern object (including disjunctive dimensional
  ;;; extents) for `pattern'
  (cond
   ((eq pattern 't)
    (make-optimized-pattern :disjunctive-dimensional-extents 't 
                            :pattern 't))
   (t (let ((dimensional-extents nil)
            ;; For error/warning messages:
            (*%%pattern%%* pattern)
            (*%%pattern-clause%%* pattern))
        (labels 
            ((optimize-simpler-pattern (p anding negated)
               (when (atom p)
                 (error "Illegal pattern ~s." pattern))
               (case (car p)
                 (not
                  `(not ,(optimize-simpler-pattern (sole-element (cdr p)) 
                                                   anding (not negated))))
                 (or
                  `(or ,.(mapcar 
                          #'(lambda (p)
                              (let* ((*%%pattern-extents%%* nil)
                                     ;; For error/warning messages:
                                     (*%%pattern-clause%%* p)
                                     (opt-p (optimize-simpler-pattern 
                                             p nil negated)))
                                (push *%%pattern-extents%%*
                                      dimensional-extents)
                                opt-p))
                          (cdr p))))
                 (and
                  (let* ((optimized-conjuncts 
                          (mapcar 
                           #'(lambda (p)
                               (optimize-simpler-pattern 
                                p 't negated))
                           (cdr p))))
                    ;; integrate any remaining (and ...) extents:
                    #+is-this-needed-any-longer
                    (dolist (new-extent-acons *%%pattern-extents%%*)
                      (destructuring-bind (dimension &rest new-extents)
                          new-extent-acons
                        (let ((extent-acons 
                               (assq dimension dimensional-extents)))
                          (cond
                           (extent-acons
                            (dolist (new-extent new-extents)
                              (setf (cdr extent-acons) 
                                    (merge-ordered-extent 
                                     new-extent (cdr extent-acons)))))
                           (t (push new-extent-acons 
                                    dimensional-extents)))))) 
                    ;; done:
                    `(and ,.optimized-conjuncts)))
                 (otherwise 
                  (optimize-pattern-element p anding negated)))))
          (let* ((dnf-pattern (convert-to-dnf pattern))
                 (*%%pattern-extents%%* nil)
                 (optimized-pattern 
                  (optimize-simpler-pattern dnf-pattern nil nil)))
            (when *%%pattern-extents%%*
              (push *%%pattern-extents%%* dimensional-extents))
            (make-optimized-pattern 
             :disjunctive-dimensional-extents dimensional-extents
             :pattern optimized-pattern)))))))

;;; ---------------------------------------------------------------------------

(defun incompatible-pattern/space-dimensions (dimension-name
                                              pattern-dimension-type
                                              space-dimension-type)
  (error "~@<Incompatible dimensions named ~s.  ~
          ~_Pattern dimension type: ~s ~
          ~_Space dimension type: ~s~:>"
         dimension-name
         pattern-dimension-type
         space-dimension-type))

;;; ---------------------------------------------------------------------------

(defun check-pattern/space-dimension-compatibility 
    (disjunctive-dimensional-extents space-instance)
  (unless (eq disjunctive-dimensional-extents 't)
    (dolist (dimensional-extents disjunctive-dimensional-extents)
      (dolist (dimensional-extent dimensional-extents)
        (destructuring-bind (extent-dimension-name
                             dimension-type . new-extents)
            dimensional-extent
          (declare (ignore new-extents))
          (let ((space-dimension
                 (assq extent-dimension-name (dimensions-of space-instance))))
            (when space-dimension
              (destructuring-bind (space-dimension-name 
                                   (space-dimension-type
                                    comparison-type))
                  space-dimension
                (declare (ignore space-dimension-name
                                 comparison-type))
                (unless (equal dimension-type space-dimension-type)
                  (incompatible-pattern/space-dimensions
                   extent-dimension-name 
                   dimension-type 
                   space-dimension-type))))))))))

;;; ---------------------------------------------------------------------------

(defun mark-based-retrieval (fn unit-classes-spec space-instances pattern 
                             filter-before filter-after verbose
                             invoking-fn-name)
  ;;; This retrieval approach uses instance marking to determine selected
  ;;; instances.  It requires a full marking sweep of candidate instances, so
  ;;; it works best with tight instance mappings on tight dimensional ranges.
  ;;; Performance is highly dependent on the speed of mark-slot access (level
  ;;; of CLOS optimization).
  (let* ((find-stats *find-stats*)
         (run-time (if find-stats (get-internal-run-time) 0))
         (optimized-pattern (optimize-pattern pattern))
         (disjunctive-dimensional-extents
          (optimized-pattern.disjunctive-dimensional-extents
           optimized-pattern))
         (storage-objects
          (if (eq pattern ':all)
              (storage-objects-for-mapping
               unit-classes-spec 
               space-instances
               invoking-fn-name)
              (storage-objects-for-retrieval
               unit-classes-spec 
               space-instances
               (optimized-pattern.disjunctive-dimensional-extents 
                optimized-pattern)
               pattern
               invoking-fn-name)))
         (unit-class-check-fn (determine-unit-class-check unit-classes-spec))
         (into-cons (cons nil nil)))
    (when verbose 
      (find-verbose-preamble pattern optimized-pattern
                             storage-objects space-instances))
    (when find-stats
      (incf (find-stats.number-of-finds find-stats))
      (incf (find-stats.number-using-marking find-stats)))
    (flet ((fn (space-instance)
             (check-pattern/space-dimension-compatibility 
              disjunctive-dimensional-extents space-instance)))
      (declare (dynamic-extent #'fn))
      (map-space-instances #'fn space-instances invoking-fn-name))
    (with-blackboard-repository-locked ()
      ;; set all flags:
      (dolist (storage storage-objects)
        (set-all-mbr-instance-marks storage disjunctive-dimensional-extents))
      ;; filter-before & pattern & filter-after & funcall `fn':
      (macrolet
          ((generate-check-fn (filter-before? filter-after?)
             `(flet 
                  ((fn (instance)
                     (when find-stats 
                       (incf (find-stats.instances-touched find-stats)))
                     (when (and (funcall (the function unit-class-check-fn)
                                         instance)
                                (progn
                                  (when find-stats 
                                    (incf (find-stats.instances-considered 
                                           find-stats)))
                                  't)
                                ,@(when filter-before?
                                    '((funcall (the function filter-before)
                                               instance)))
                                (match-instance-to-pattern 
                                 instance
                                 (optimized-pattern.pattern optimized-pattern)
                                 into-cons
                                 verbose)
                                ,@(when filter-after?
                                    '((funcall (the function filter-after)
                                               instance))))
                       ;; the instance is accepted:
                       (when find-stats 
                         (incf (find-stats.instances-accepted find-stats)))
                       (funcall (the function fn) instance))
                     ;; we have either accepted or rejected this instance:
                     (clear-mbr-instance-mark instance)))
                (declare (dynamic-extent #'fn))
                (dolist (storage storage-objects)
                  (map-marked-instances-on-storage
                   #'fn storage disjunctive-dimensional-extents verbose)))))
        (if filter-before
            (if filter-after
                (generate-check-fn t t)
                (generate-check-fn t nil))
            (if filter-after 
                (generate-check-fn nil t)
                (generate-check-fn nil nil))))
      (when find-stats
        (incf (find-stats.run-time find-stats) 
              (- (get-internal-run-time) run-time))))))

;;; ---------------------------------------------------------------------------

(defun hash-based-retrieval (fn unit-classes-spec space-instances pattern 
                             filter-before filter-after verbose
                             invoking-fn-name)
  ;;; This retrieval approach uses a hash table to determine selected
  ;;; instances.  It works best with large sweeps with limited
  ;;; selected/rejected instances.
  (let* ((find-stats *find-stats*)
         (run-time (if find-stats (get-internal-run-time) 0))
         (optimized-pattern (optimize-pattern pattern))
         (disjunctive-dimensional-extents
          (optimized-pattern.disjunctive-dimensional-extents 
           optimized-pattern))
         (storage-objects
          (if (eq pattern ':all)
              (storage-objects-for-mapping
               unit-classes-spec 
               space-instances
               invoking-fn-name)
              (storage-objects-for-retrieval
               unit-classes-spec
               space-instances
               (optimized-pattern.disjunctive-dimensional-extents
                optimized-pattern)
               pattern
               invoking-fn-name)))
         (unit-class-check-fn (determine-unit-class-check unit-classes-spec))
         (processed-instance-set (make-eset))
         (into-cons (cons nil nil)))
    (when verbose 
      (find-verbose-preamble pattern optimized-pattern 
                             storage-objects space-instances))
    (when find-stats
      (incf (find-stats.number-of-finds find-stats)))
    (flet ((fn (space-instance)
             (check-pattern/space-dimension-compatibility 
              disjunctive-dimensional-extents space-instance)))
      (declare (dynamic-extent #'fn))
      (map-space-instances #'fn space-instances invoking-fn-name))
    ;; filter-before & pattern & filter-after & funcall `fn':
    (macrolet
        ((generate-check-fn (filter-before? filter-after?)
           `(flet 
                ((fn (instance)
                   (unless (in-eset instance processed-instance-set)
                     (when find-stats 
                       (incf (find-stats.instances-touched find-stats)))
                     (when (and (funcall (the function unit-class-check-fn) 
                                         instance)
                                (progn 
                                  (when find-stats 
                                    (incf (find-stats.instances-considered 
                                           find-stats)))
                                  't)
                                ,@(when filter-before?
                                    '((funcall (the function filter-before) 
                                               instance)))
                                (match-instance-to-pattern 
                                 instance
                                 (optimized-pattern.pattern optimized-pattern)
                                 into-cons
                                 verbose)
                                ,@(when filter-after?
                                    '((funcall (the function filter-after) 
                                               instance))))
                       ;; the instance is accepted:
                       (when find-stats 
                         (incf (find-stats.instances-accepted find-stats)))
                       (funcall (the function fn) instance))
                     ;; we have either accepted or rejected this instance:
                     (add-to-eset instance processed-instance-set))))
              (declare (dynamic-extent #'fn))
              (with-blackboard-repository-locked ()
                (dolist (storage storage-objects)
                  (map-all-instances-on-storage 
                   #'fn storage disjunctive-dimensional-extents verbose))))))
      (if filter-before
          (if filter-after
              (generate-check-fn t t)
              (generate-check-fn t nil))
          (if filter-after 
              (generate-check-fn nil t)
              (generate-check-fn nil nil))))
    (when find-stats
      (incf (find-stats.run-time find-stats) 
            (- (get-internal-run-time) run-time)))))

;;; ===========================================================================
;;;  Instance Mapping
;;;
;;;   <pattern> :== t | <subpattern>
;;;   <subpattern> :== <pattern-element> |
;;;                    (and <subpattern>*) |
;;;                    (or <subpattern>*) |
;;;                    (not <subpattern>)
;;;   <pattern-element> :== (<op> <dims> <values> <option>*)

(defun mark-based-sweep (fn unit-classes-spec space-instances 
                         filter-before filter-after
                         verbose invoking-fn-name)  
  ;;; This internal mark-based function is optimized for mapping all unit
  ;;; instances on `space-instances' (one call per unit instance is
  ;;; guaranteed, even if it resides on multiple space instances).
  (let ((storage-objects
         (storage-objects-for-mapping unit-classes-spec space-instances
                                      invoking-fn-name))
        (unit-class-check-fn (determine-unit-class-check unit-classes-spec)))
    (with-blackboard-repository-locked ()
      ;; set all flags:
      (dolist (storage storage-objects)
        (set-all-mbr-instance-marks storage 't))
      ;; sweep:
      (macrolet
          ((generate-check-fn (filter-before? filter-after?)
             `(flet 
                  ((fn (instance)
                     (clear-mbr-instance-mark instance)
                     (when (and (funcall (the function unit-class-check-fn)
                                         instance)
                                ,@(when filter-before?
                                    '((funcall (the function filter-before) 
                                               instance)))
                                ;; there is no pattern here...
                                ,@(when filter-after?
                                    '((funcall (the function filter-after) 
                                               instance))))
                       (funcall (the function fn) instance))))
                (declare (dynamic-extent #'fn))
                (dolist (storage storage-objects)
                  (map-marked-instances-on-storage #'fn storage 't verbose)))))
        (if filter-before
            (if filter-after
                (generate-check-fn t t)
                (generate-check-fn t nil))
            (if filter-after 
                (generate-check-fn nil t)
                (generate-check-fn nil nil)))))))

;;; ---------------------------------------------------------------------------

(defun hash-based-sweep (fn unit-classes-spec space-instances 
                         filter-before filter-after
                         verbose invoking-fn-name)  
  ;;; This internal hash-based function is optimized for mapping all unit
  ;;; instances on `space-instances' (one call per unit instance is
  ;;; guaranteed, even if it resides on multiple space instances).
  (let ((storage-objects
         (storage-objects-for-mapping unit-classes-spec space-instances
                                      invoking-fn-name))
        (unit-class-check-fn (determine-unit-class-check unit-classes-spec))
        (processed-instance-set (make-eset)))
    (with-blackboard-repository-locked ()
      (macrolet
          ((generate-check-fn (filter-before? filter-after?)
             `(flet
                  ((fn (instance)
                     (unless (in-eset instance processed-instance-set)
                       (when (and (funcall (the function unit-class-check-fn) 
                                           instance)
                                  ,@(when filter-before?
                                      '((funcall (the function filter-before)
                                                 instance)))
                                  ;; there is no pattern here...
                                  ,@(when filter-after?
                                      '((funcall (the function filter-after)
                                                 instance))))
                         (funcall (the function fn) instance))
                       ;; we have either accepted or rejected this instance:
                       (add-to-eset instance processed-instance-set))))
                (declare (dynamic-extent #'fn))
                (dolist (storage storage-objects)
                  (map-all-instances-on-storage #'fn storage 't verbose)))))
        (if filter-before
            (if filter-after
                (generate-check-fn t t)
                (generate-check-fn t nil))
            (if filter-after 
                (generate-check-fn nil t)
                (generate-check-fn nil nil)))))))

;;; ---------------------------------------------------------------------------

(defun map-selected-instances-on-space-instances (fn unit-classes-spec 
                                                  space-instances pattern
                                                  filter-before filter-after 
                                                  use-marking verbose
                                                  invoking-fn-name)
  (funcall (if use-marking
               #'mark-based-retrieval
               #'hash-based-retrieval)
           fn unit-classes-spec space-instances pattern 
           filter-before filter-after verbose
           invoking-fn-name))

;;; ---------------------------------------------------------------------------

(defun map-instances-on-space-instances (fn unit-classes-spec space-instances 
                                         &key (pattern ':all)
                                              filter-before filter-after
                                              (use-marking *use-marking*)
                                              (verbose *find-verbose*))
  ;;; Note that checking for deleted instances is not done here, the
  ;;; check must be added to `fn', if desired.
  (when verbose (find-verbose-operation 'map-instances-on-space-instances))
  (let ((fn (coerce fn 'function))
        (filter-before (when filter-before
                         (coerce filter-before 'function)))
        (filter-after (when filter-after 
                        (coerce filter-after 'function))))
    (if (eq pattern ':all)
        ;; full mapping (pass-thru 't `space-instances' value):
        (funcall (if use-marking
                     #'mark-based-sweep
                     #'hash-based-sweep)
                 fn unit-classes-spec space-instances 
                 filter-before filter-after verbose 
                 'map-instances-on-space-instances)
        ;; pattern-selected mapping:
        (map-selected-instances-on-space-instances
         fn unit-classes-spec 
         (if (eq space-instances 't)
             (find-space-instances '(*))
             (ensure-list space-instances))
         pattern filter-before filter-after use-marking verbose
         'map-instances-on-space-instances))))

;;; ---------------------------------------------------------------------------

(defmacro do-instances-on-space-instances ((var unit-classes-spec 
                                            space-instances 
                                            &key (pattern ':all)
                                                 filter-before filter-after
                                                 (use-marking *use-marking*)
                                                 (verbose *find-verbose*))
                                           &body body)
   ;;; Do-xxx variant of map-instances-on-space-instances.
  `(block nil
     (flet ((fn (,var) ,@body))
       (declare (dynamic-extent #'fn))
       (map-instances-on-space-instances 
        #'fn
        ,unit-classes-spec
        ,space-instances
        ,@(when pattern `(:pattern ,pattern))
        ,@(when filter-before `(:filter-before ,filter-before))
        ,@(when filter-after `(:filter-after ,filter-after))
        ,@(when use-marking `(:use-marking ,use-marking))
        ,@(when verbose `(:verbose ,verbose))))))

;;; ===========================================================================
;;;   Find Instances
;;;
;;;   <pattern> :== t | <subpattern>
;;;   <subpattern> :== <pattern-element> |
;;;                    (and <subpattern>*) |
;;;                    (or <subpattern>*) |
;;;                    (not <subpattern>)
;;;   <pattern-element> :== (<op> <dims> <values> <option>*)

(defun find-instances (unit-classes-spec space-instances pattern
                       &key filter-before filter-after
                            (use-marking *use-marking*)
                            (verbose *find-verbose*))
  (when verbose (find-verbose-operation 'find-instances))
  (let* ((result nil)
         (filter-before (when filter-before (coerce filter-before 'function)))
         (filter-after (when filter-after (coerce filter-after 'function))))
    (flet ((fn (instance) 
             (push instance result)))
      (declare (dynamic-extent #'fn))
      (cond 
       ;; full space-instance sweep:
       ((eq pattern ':all)
        (funcall (if use-marking 
                     #'mark-based-sweep
                     #'hash-based-sweep)
                 #'fn unit-classes-spec space-instances 
                 filter-before filter-after verbose
                 'find-instances))
       ;; pattern-based retrieval:
       (t (funcall (if use-marking 
                       #'mark-based-retrieval
                       #'hash-based-retrieval)
                   #'fn
                   unit-classes-spec 
                   (if (eq space-instances 't)
                       (find-space-instances '(*))
                       (ensure-list space-instances))
                   pattern
                   filter-before filter-after verbose
                   'find-instances))))
    result))

;;; ===========================================================================
;;;   Filter Instances
;;;
;;;   <pattern> :== t | <subpattern>
;;;   <subpattern> :== <pattern-element> |
;;;                    (and <subpattern>*) |
;;;                    (or <subpattern>*) |
;;;                    (not <subpattern>)
;;;   <pattern-element> :== (<op> <dims> <values> <option>*)
;;;
;;;  Filter-instances will test/return multiple copies of a unit instance if
;;;  it appears multiple times in `instances'

(defun filter-instances (instances pattern
                         &key filter-before filter-after 
                              (verbose *find-verbose*))
  (when verbose (find-verbose-operation 'filter-instances))
  (let ((optimized-pattern (optimize-pattern pattern))
        (filter-before (when filter-before (coerce filter-before 'function)))
        (filter-after (when filter-after (coerce filter-after 'function)))
        (into-cons (cons nil nil)))
    (flet ((fn-bare (instance)
             (when (match-instance-to-pattern 
                    instance
                    (optimized-pattern.pattern optimized-pattern)
                    into-cons
                    verbose)
               ;; instance is accepted:
               (list instance)))
           (fn-before-only (instance)
             (when (and (funcall (the function filter-before) instance)
                        (match-instance-to-pattern 
                         instance
                         (optimized-pattern.pattern optimized-pattern)
                         into-cons
                         verbose))
               ;; instance is accepted:
               (list instance)))
           (fn-after-only (instance)
             (when (and (match-instance-to-pattern 
                         instance
                         (optimized-pattern.pattern optimized-pattern)
                         into-cons
                         verbose)
                        (funcall (the function filter-after) instance))
               ;; instance is accepted:
               (list instance)))
           (fn-both (instance)
             (when (and (funcall (the function filter-before) instance)
                        (match-instance-to-pattern 
                         instance
                         (optimized-pattern.pattern optimized-pattern)
                         into-cons
                         verbose)
                        (funcall (the function filter-after) instance))
               ;; instance is accepted:
               (list instance))))
      (declare (dynamic-extent #'fn-bare #'fn-before-only
                               #'fn-after-only #'fn-both))
      (let ((fn (if filter-before
                    (if filter-after #'fn-both #'fn-before-only)
                    (if filter-after #'fn-after-only #'fn-bare))))
        (mapcan fn instances)))))

;;; ===========================================================================
;;;   With-Find-Stats
;;;

(defun report-find-stats (&key (reset nil))
  (let ((stats *find-stats*))
    (cond ((null stats)
           (format *trace-output* 
                   "~&;; No find/map statistics are available.~%"))
          (t (let ((seconds (/ (find-stats.run-time stats)
                               #.(float internal-time-units-per-second)))
                   (finds (find-stats.number-of-finds stats))
                   (marking (find-stats.number-using-marking stats)))
               (format *trace-output* 
                       "~&;; Find/Map Statistics:~
                        ~%;; ~9d find/map operation~:p ~
                        (~s using marking, ~s using hashing)~
                        ~%;; ~9d bucket~:p scanned~
                        ~%;; ~9d instance~:p touched~
                        ~%;; ~9d instance~:p considered~
                        ~%;; ~9d instance~:p accepted~
                        ~%;; ~9,2f seconds (~,2,2f msec/operation)~%"
                       finds
                       marking
                       (- finds marking)
                       (find-stats.bucket-count stats)
                       (find-stats.instances-touched stats)
                       (find-stats.instances-considered stats)
                       (find-stats.instances-accepted stats)
                       seconds
                       (if (plusp finds)
                           (/ seconds finds)
                           0.0)))
             (when reset (setf *find-stats* (make-find-stats))))))
  (values))

;;; ---------------------------------------------------------------------------

(defun init-find-stats (initialize)
  (if initialize 
      (make-find-stats)
      (or *find-stats*
          (progn
            (warn "No find-stats being recorded, :initialize 't assumed.")
            (make-find-stats)))))  

;;; ---------------------------------------------------------------------------

(defmacro with-find-stats ((&key (initialize 't) (report 't)) &body body)
  `(let ((*find-stats* (init-find-stats ,initialize)))
     (multiple-value-prog1 
         (progn ,@body)
       (when ,report (report-find-stats)))))

;;; ---------------------------------------------------------------------------

(defmacro without-find-stats (&body body)
  `(let ((*find-stats* nil))
     ,@body))  

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
