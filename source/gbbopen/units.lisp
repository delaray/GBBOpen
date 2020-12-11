;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/units.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Dec 20 11:26:11 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen Unit-Class Functions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;   [Description of MOP implementation-specific porting details needed...]
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-02 File created.  (Corkill)
;;;  09-15-02 Moved extended class functions into ../tools/define-class.lisp.
;;;           (Corkill)
;;;  01-21-04 Added CLASS-INSTANCES-COUNT.  (Corkill)
;;;  03-11-04 Added inheritance of initial-space-instances and
;;;           dimensional-values (no inheritance prevention yet).  (Corkill)
;;;  03-16-04 Changed dimension-value-type :label to :element.  (Corkill)
;;;  04-20-04 MOP imports separated into mop-interface.lisp.  (Corkill)
;;;  05-27-04 Added UNIT-CLASS-DIMENSIONS.  (Corkill)
;;;  06-08-04 Added PARSE-UNIT-CLASSES-SPECIFIER.  (Corkill)
;;;  08-05-04 Removed hacks supporting CMUCL-18e non-compliances.  (Corkill)
;;;  08-16-04 Added check for accidentally quoted :singular values 
;;;           in link-slot options.  (Corkill)
;;;  09-01-04 Added check on finalization that a unit-class inherits from
;;;           standard-unit-instance.  (Corkill)
;;;  09-27-04 Removed ALL-UNIT-CLASS-NAMES.  (Corkill)
;;;  12-21-04 Remove instance-name-comparison-test GF mechanism.  (Corkill)
;;;  06-01-05 Support two-slot :interval dimensional-values.  (Corkill)
;;;  06-08-05 Added CLISP support. (sds)
;;;  06-13-05 Moved GBBopen unit metaclass definitions to 
;;;           unit-metaclasses.lisp. (Corkill)
;;;  12-20-12 Tightened CLISP eq-losage workaround symbol-name test used in 
;;;           FIXUP-FUNCTION-VALUE-PART2. (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-instances-count
            class-instances-summary     ; not yet documented
            define-unit-class
            describe-unit-class
            effective-link-definition
            effective-nonlink-slot-definition 
	    ensure-unit-class		; not yet implemented/documented
            find-effective-slot-definition-by-name ; not documented (yet...)
            find-unit-class             ; not documented (at least yet...)
            gbbopen-effective-slot-definition
            reset-unit-class)))

;;; ---------------------------------------------------------------------------

(defparameter *dimension-value-types*
    '(:point :interval :mixed :element :boolean))

(defparameter *ordered-comparison-types*
    '(number fixnum short-float single-float double-float long-float
      pseudo-probability))

(defparameter *enumerated-comparison-types*
    '(eq eql equal equalp))

(defparameter *boolean-comparison-types*
    '(t))

;;; ===========================================================================
;;;   Define-unit-class

(defmethod validate-class-option ((metaclass standard-class) option)
  #+ecl (declare (ignore metaclass))
  (or (memq (car option) *standard-define-class-options*)
      (memq (car option) '(:default-initargs 
			   :documentation
			   :metaclass
			   #+(or ecl-no-longer lispworks)
			   :optimize-slot-access))))

 ;;; ---------------------------------------------------------------------------

(defun compute-inherited-unit-class-values (unit-class)
  ;;; Determines directly inherited unit-class values from super-unit-classes
  ;;; of `unit-class' and then propagates them down to all sub-unit-classes
  
  ;; Begin with direct values:
  (let ((initial-space-instances
         (standard-unit-class.initial-space-instances unit-class))
        (dimensional-values
         (standard-unit-class.dimensional-values unit-class))
        (retain
         (standard-unit-class.retain unit-class)))
    (declare (type list dimensional-values))
    (unless initial-space-instances
      ;; Find closest initial-space-instances from supers:
      (dolist (super (class-direct-superclasses unit-class))
        (when (typep super 'standard-unit-class)
          (let ((inherited-value 
                 (standard-unit-class.effective-initial-space-instances
                  super)))
            (when inherited-value
              (setf initial-space-instances inherited-value)
              (return))))))
    ;; Add dimensional-values from supers:
    (dolist (super (class-direct-superclasses unit-class))
      (when (typep super 'standard-unit-class)
        (dolist (cdv (standard-unit-class.effective-dimensional-values super))
          (pushnew cdv dimensional-values :test #'eq :key #'cdv.dimension-name))))
    ;; Stash the effective values:
    (setf (standard-unit-class.effective-initial-space-instances unit-class)
          initial-space-instances)
    (setf (standard-unit-class.effective-dimensional-values unit-class)
          dimensional-values)
    ;; Compute and stash effective dimension-value-source slots:
    (let ((dv-source-slots nil))
      (dolist (cdv dimensional-values)
        (when cdv
          (let ((dimension-name (cdv.dimension-name cdv))
                (slot-name-or-fn (cdv.slot-name-or-fn cdv))
                (slot-name (cdv.slot-name cdv)))
            (flet ((add-dimension (slot-name dimension-name)
                     (let ((acons (assq slot-name dv-source-slots)))
                       (if acons
                           (pushnew dimension-name (cdr acons))
                           (push-acons slot-name 
                                       (list dimension-name)
                                       dv-source-slots)))))
            (when (symbolp slot-name-or-fn) ; check against effective-slot
                                            ; names?
              (add-dimension slot-name-or-fn dimension-name))
            (when slot-name
              (add-dimension slot-name dimension-name))))))
      (setf (standard-unit-class.effective-dv-source-slots unit-class)
            dv-source-slots))
    ;; Cache unit-class-dimensions:
    (setf (standard-unit-class.unit-class-dimensions unit-class)
          (flet ((fn (cdv)
                   `(;; dimension name:
                     ,(cdv.dimension-name cdv)
                     (;; dimension type:
                      ,(case (cdv.dimension-value-type cdv)
                         (:boolean ':boolean)
                         (:element ':enumerated)
                         ((:point :interval :mixed) ':ordered))
                      ;; comparison type:
                      ,(cdv.comparison-type cdv)))))
            (declare (dynamic-extent #'fn))
            (mapcar #'fn dimensional-values)))
    ;; Propogate retain attribute from supers:
    (unless retain
      (dolist (super (class-direct-superclasses unit-class))
        (when (and (typep super 'standard-unit-class)
                   (eq (standard-unit-class.retain super) ':propagate))
          (setf (standard-unit-class.retain unit-class) ':propagate)
          (return))))
    ;; Propagate to subs (naive, for now...):
    (dolist (sub (class-direct-subclasses unit-class))
      (compute-inherited-unit-class-values sub))))

;;; ---------------------------------------------------------------------------

(defvar *%%fixup-function-objects%%*)

(defun fixup-function-value-part1 (value)
  ;;; Part 1 of the function-fixup scheme.  Adds functions and possible
  ;;; closures to *%%fixup-function-objects%%* and replaces `value' with a
  ;;; gensym key into *%%fixup-function-objects%%*
  (cond 
   ;; We'll allow symbols as if they were quoted symbols:
   ((symbolp value) value)
   ;; Quoted symbols imply (funcall 'symbol ...):
   ((and (consp value) 
         (eq (first value) 'quote)
         (symbolp (second value)))
    (second value))
   ;; Functions & possible closures:
   ((and (consp value) 
         (eq (first value) 'function))
    (let ((fixup-symbol (gensym "F")))
      (push `(,fixup-symbol ,value)
            *%%fixup-function-objects%%*)
      fixup-symbol))
   ;; Pass thru anything else:
   (t value)))

;;; ---------------------------------------------------------------------------
;;;   Dimensional-value class option syntax
;;;
;;; <dimensional-value> :== <incomposite-dimensional-value> | 
;;;                         <composite-dimensional-value>
;;; <incomposite-dimensional-value>
;;;             :== (<dim-name> <dimension-value-spec> <dimension-value-place>)
;;; <composite-dimensional-value>
;;;             :== (<dim-name> <dimension-value-spec> 
;;;                  <composite-type> <dimension-value-place>)
;;; <composite-type> :== :set | :series |
;;;                      :ascending-series <ordering-dimension-name> |
;;;                      :descending-series <ordering-dimension-name>
;;; <dimension-value-spec> :== <dimension-value-type> |
;;;                            (<ordered-dimension-value-type> 
;;;                             [<ordered-comparison-type>]) |
;;;                            (<enumerated-dimension-value-type> 
;;;                             [<enumerated-comparison-type>]) |
;;;                            (<boolean-dimension-value-type>
;;;                             [<boolean-comparison-type>]) 
;;; <dimension-value-type> :==  <ordered-dimension-value-type> |
;;;                             <enumerated-dimension-value-type> |
;;;                             <boolean-dimension-value-type>
;;; <ordered-dimension-value-type> :== :point | :interval | :mixed
;;; <enumerated-dimension-value-type> :== :element
;;; <boolean-dimension-value-type> :== :boolean
;;; <ordered-comparison-type> :== number | fixnum | 
;;;                               short-float | single-float |
;;;                               double-float | long-float |
;;;                               pseudo-probability
;;; <enumerated-comparison-type> :== eq | eql | equal | equalp
;;; <boolean-comparison-type> :== t
;;; <dimension-value-place> :== {<slot-name> [<slot-name>]} |
;;;                             {<function> [<slot-name>]}
;;;  note that 2 slot-names are only allowed for :interval dimension values
;;; ---------------------------------------------------------------------------

(defun comparison-type-error (unit-class-name dimension-name 
                              dimension-value-type comparison-type 
                              allowed-comparison-types)
  (error "The comparison-type ~s specified for the ~s dimension ~a of ~
          unit-class ~s is not~{~#[~^~; ~s~^~; ~s, or ~s~^~] ~s,~}."
         comparison-type
         dimension-value-type
         dimension-name
         unit-class-name
         allowed-comparison-types))

;;; ---------------------------------------------------------------------------

(defun determine-dimension-value-comparison-type (unit-class-name 
                                                  dimension-name dv-type-spec)
  ;;; Returns the specified or implicit comparison-type for a
  ;;; dimension-value-type-spec or nil if the specified dimension-value-type
  ;;; is invalid.
  (destructuring-bind (dv-type &optional comparison-type)
      (ensure-list dv-type-spec)
    (case dv-type
      ((:point :interval :mixed)
       (cond 
        ;; specified, check its validity:
        (comparison-type
         (unless (memq comparison-type *ordered-comparison-types*)
           (comparison-type-error unit-class-name 
                                  dimension-name ':ordered comparison-type 
                                  *ordered-comparison-types*)))
        ;; use the ordered-dimension default:
        (t (setf comparison-type 'number)))
       comparison-type)
      (:element
       (cond
        ;; specified, check its validity:
        (comparison-type
         (unless (memq comparison-type *enumerated-comparison-types*)
           (comparison-type-error unit-class-name 
                                  dimension-name ':enumerated comparison-type 
                                  *enumerated-comparison-types*)))
        ;; use the enumerated-dimension default:
        ((setf comparison-type 'eql)))
       comparison-type)
      (:boolean
       (cond
        ;; specified, check its validity:
        (comparison-type
         (unless (memq comparison-type *boolean-comparison-types*)
           (comparison-type-error unit-class-name
                                  dimension-name ':boolean comparison-type 
                                  *boolean-comparison-types*)))
        ;; use the boolean-dimension default:
        (t (setf comparison-type 't)))
       comparison-type))))

;;; ---------------------------------------------------------------------------

(defun unfinished-composite-type-warning (composite-type)
  (warn "Support for ~s composite dimensional values is not completed."
        composite-type))

;;; ---------------------------------------------------------------------------

(defun parse-dimensional-value-spec (unit-class-name dv-spec)
  ;;; Handles syntax differences among the various composite types.
  ;;; (Also warns about use of unfinished composite types)
  (flet ((extract-dimension-value-type (dimension-value-type)
           (if (consp dimension-value-type)
               (first dimension-value-type)
               dimension-value-type)))
    (case (third dv-spec)               ; potential composite-type keyword
      ;; A set-composite or a sequence-composite dimensional value spec:
      ((:set :sequence)
       (destructuring-bind (dimension-name dimension-value-type
                            composite-type slot-name-or-fn 
                            &optional slot-name)
           dv-spec
         (when (eq composite-type ':sequence)
           (unfinished-composite-type-warning composite-type))
         (values dimension-name
                 (extract-dimension-value-type dimension-value-type)
                 (determine-dimension-value-comparison-type 
                  unit-class-name dimension-name dimension-value-type)
                 composite-type nil 
                 slot-name-or-fn slot-name)))
      ;; A series-composite dimensional value spec:
      ((:ascending-series :descending-series)
       (unfinished-composite-type-warning (third dv-spec))
       (destructuring-bind (dimension-name dimension-value-type
                            composite-type ordering-dimension-name
                            slot-name-or-fn &optional slot-name)
           dv-spec
         (values dimension-name
                 (extract-dimension-value-type dimension-value-type)
                 (determine-dimension-value-comparison-type 
                  unit-class-name dimension-name dimension-value-type)
                 composite-type ordering-dimension-name 
                 slot-name-or-fn slot-name)))    
      ;; An incomposite dimensional value spec:
      (otherwise 
       (destructuring-bind (dimension-name #+IGNORE-FOR-NOW composite-value-type
                            dimension-value-type slot-name-or-fn 
                            &optional slot-name)
           dv-spec
         (values dimension-name 
                 (extract-dimension-value-type dimension-value-type)
                 (determine-dimension-value-comparison-type 
                  unit-class-name dimension-name dimension-value-type)
                 nil nil 
                 slot-name-or-fn slot-name))))))
 
;;; ---------------------------------------------------------------------------

(defun declared-function-designator (fnd)
  ;; Declares `fnd' is a function, unless it is a quoted symbol:
  (cond 
   ;; quoted symbol:
   ((eq 'quote (first fnd)) fnd)
   ;; anything else should be a function:
   (t `(the function ,fnd))))

;;; ---------------------------------------------------------------------------

(defun build-cannonical-dimensional-value-spec (unit-class-name dv-spec)
  ;;; Returns a cannonical dimension-value (cdv) of `dv-spec'; note that the
  ;;; cdv does not contain sufficient accessible information to recreate the
  ;;; exact original `dv-spec'.
  (multiple-value-bind (dimension-name 
                        dimension-value-type comparison-type
                        composite-type ordering-dimension-name
                        slot-name-or-fn slot-name)
      (parse-dimensional-value-spec unit-class-name dv-spec)
    ;; A nil comparison-type indicates that an illegal dimension-value-type
    ;; was specified:
    (unless comparison-type
      (error "Dimension-value-type ~s specified for ~
              dimension ~s of ~s is not one of ~
              ~{~#[~;~s~;~s and ~s~:;~@{~s~#[~;, or ~:;, ~]~}~]~}."
             dimension-value-type
             dimension-name
             unit-class-name
	     *dimension-value-types*))
    ;; Slot-name must name a real slot:
    (when slot-name
      (unless (symbolp slot-name)
        (error "Invalid slot name ~s specified for ~
                       dimension ~s of ~s." 
               slot-name
               dimension-name
               unit-class-name)))
    ;; Build the dimensional-value lookup function, called on a unit instance
    ;; to return the dimensional value for the `dv-spec' dimension:
    (let* ((unfixed-dv-lookup-function
            `(function
              (lambda (instance &optional into-cons)
                (declare (type ,unit-class-name instance)
                         (ignorable into-cons))
                ,(cond 
                  ;; an :interval dimension-value-type with two slot names:
                  ((and (eq dimension-value-type ':interval)
                        (symbolp slot-name-or-fn)
                        slot-name)
                   `(if (and (slot-boundp instance ',slot-name-or-fn)
                             (slot-boundp instance ',slot-name))
                        (let ((interval-start 
                               (slot-value instance ',slot-name-or-fn))
                              (interval-end
                               (slot-value instance ',slot-name)))
                          (cond 
                           (into-cons
                            (setf (car into-cons) interval-start)
                            (setf (cdr into-cons) interval-end)
                            into-cons)
			 (t (cons interval-start interval-end))))
                        unbound-value-indicator))
                  ;; only a slot name:
                  ((symbolp slot-name-or-fn)
                   `(if (slot-boundp instance ',slot-name-or-fn)
			(slot-value instance ',slot-name-or-fn)
			unbound-value-indicator))
                  ;; a function with a slot name:
                  (slot-name
                   `(if (slot-boundp instance ',slot-name)
			(funcall ,(declared-function-designator
                                   slot-name-or-fn)
				 (slot-value instance ',slot-name))
			unbound-value-indicator))
                  ;; just a function
                  (t `(funcall ,(declared-function-designator 
                                 slot-name-or-fn)
			       instance))))))
           (dv-lookup-function 
            (fixup-function-value-part1 unfixed-dv-lookup-function)))
      (make-cdv
       :dimension-name dimension-name 
       :dimension-value-type dimension-value-type 
       :comparison-type comparison-type
       :value-fn dv-lookup-function
       :composite-type composite-type 
       :ordering-dimension-name ordering-dimension-name
       :slot-name-or-fn slot-name-or-fn 
       :slot-name slot-name))))

;;; ---------------------------------------------------------------------------

#+clisp
(defun %same-symbol-names (a b)
  ;; Test used in the workaround in FIXUP-FUNCTION-OBJECTS-PART2 for CLISP
  ;; losing eq-ness of the uninterned fixup-symbols when loading the compiled
  ;; file into the compiling image (still observed in CLISP 2.45).
  (and (symbolp a)
       (symbolp b)
       (string= (symbol-name a) (symbol-name b))))

;;; ---------------------------------------------------------------------------

(defun fixup-function-objects-part2 (unit-class fixup-symbols)
  ;;; Part 2 of the function-fixup scheme.  Restores function initvals for
  ;;; :sort-function and :sort-key from *%%fixup-function-objects%%* based on
  ;;; the gensym'ed fixup-symbols
  (declare (type list fixup-symbols))
  (flet ((do-fn (dslotd)
           ;; fixup sort-function slot
           (let ((fn (direct-link-definition.sort-function dslotd)))
             (when (and fn (memq fn fixup-symbols))
               (setf (direct-link-definition.sort-function dslotd)
                     (symbol-value fn))))
           ;; fixup sort-key slot
           (let ((fn (direct-link-definition.sort-key dslotd)))
             (when (and fn (memq fn fixup-symbols))
               (setf (direct-link-definition.sort-key dslotd)
                     (symbol-value fn))))))
    (declare (dynamic-extent #'do-fn))
    (map-direct-link-slots #'do-fn unit-class))
  ;; fixup canonicalized :dimensional-values option:  
  (dolist (cdv (standard-unit-class.dimensional-values unit-class))
    (let ((maybe-fn (cdv.value-fn cdv)))
      (when (or (memq maybe-fn fixup-symbols)
                ;; NOTE: CLISP looses eq-ness of the uninterned fixup-symbols
                ;; when loading the compiled file into the compiling image
                ;; (still observed in CLISP 2.45).  As a work-around until
                ;; this is tracked down, we look up the symbol in
                ;; fixup-symbols by comparing the symbol-name (if the eq check
                ;; fails) and then using the symbol from fixup-symbols:
                #+clisp
                (setf maybe-fn (find maybe-fn fixup-symbols 
                                     :test #'%same-symbol-names)))
        (setf (cdv.value-fn cdv)
              (symbol-value maybe-fn)))))
  ;; fixup :initial-space-instances option
  (let* ((initial-space-instances-spec
          (standard-unit-class.initial-space-instances unit-class))
         (maybe-fn (first initial-space-instances-spec)))
    (when (and (symbolp maybe-fn)
               (or (memq maybe-fn fixup-symbols)
                   ;; NOTE: CLISP looses eq-ness of the uninterned
                   ;; fixup-symbols when loading the compiled file into the
                   ;; compiling image (still observed in CLISP 2.45).  As a
                   ;; work-around until this is tracked down, we look up the
                   ;; symbol in fixup-symbols by comparing the symbol-name (if
                   ;; the eq check fails) and then using the symbol from
                   ;; fixup-symbols:
                   #+clisp
                   (setf maybe-fn (find maybe-fn fixup-symbols 
                                        :test #'%same-symbol-names))))
      (setf (standard-unit-class.initial-space-instances unit-class)
            (symbol-value maybe-fn)))))
  
;;; ---------------------------------------------------------------------------

(defun fixup-function-initargs (initargs)
  ;;; Part 1b of the function-fixup scheme.  Adds function initvals for
  ;;; :sort-function and :sort-key to *%%fixup-function-objects%%* and replaces
  ;;; them with gensyms
  (flet ((fixup-initarg (slot-def indicator)
           (let ((initval (getf (cdr slot-def) indicator)))
             (when initval 
               (setf (getf (cdr slot-def) indicator)
                     (fixup-function-value-part1 initval))))))
    (dolist (slot-def initargs)
      (fixup-initarg slot-def ':sort-function)
      (fixup-initarg slot-def ':sort-key))
    initargs))

;;; ---------------------------------------------------------------------------

(defun fixup-function-option-values (unit-class-name metaclass options)
  ;;; Part 1a of the function-fixup scheme.  
  ;;;
  ;;; Adds functional option values for :initial-space-instances to
  ;;; *%%fixup-function-objects%%* and replaces them with gensyms.
  ;;;
  ;;; Also adds functional option-values for :dimensional-values, as part of
  ;;; syntax checking and cannonicalization.
  (let ((dimensional-values-seen nil)
	(initial-space-instances-seen nil))
    (dolist (option options)
      (case (car option)
	(:dimensional-values 
	 (setf dimensional-values-seen 't)
	 (setf (cdr option)
               ;; allow (:dimensional-values nil) to mean none:
	       (if (equal (cdr option) '(nil))
		   nil
                   (flet ((fn (dv-spec)
                            (build-cannonical-dimensional-value-spec
                             unit-class-name dv-spec)))
                     (declare (dynamic-extent #'fn))
                     (mapcar #'fn (cdr option))))))
	(:initial-space-instances
	 (setf initial-space-instances-seen 't)
         (let ((initial-space-instances-option-values (cdr option)))
           (declare (type list initial-space-instances-option-values))
           ;; check that the initial-space-instances specification contains
           ;; only a single function object!
           (when (and (list-length>1 initial-space-instances-option-values)
                      (flet ((fn (elt)
                               (eq (car elt) 'function)))
                        (declare (dynamic-extent #'fn))
                        (some #'fn initial-space-instances-option-values)))
             (error "The option: ~s in the definition of unit-class ~s ~
                     contains more than a single function object."
                    option
                    unit-class-name))
	 (setf (cdr option)
	       ;; allow (:initial-space-instances nil) to mean none:
	       (if (equal initial-space-instances-option-values '(nil))
		   nil
		   (mapcar #'fixup-function-value-part1 
			   initial-space-instances-option-values)))))
	;; Check that the class option is valid (not mis-named or misspelled);
	;; this is a heavy-handed approach that doesn't pass through unknown
	;; options without using the validate-class-option API.
	(otherwise 
	 (unless (validate-class-option metaclass option)
	   (error "~s is an invalid option in the definition of unit-class ~s."
		  option unit-class-name)))))
    ;; We want to clear these in reinitialized class objects, if they
    ;; weren't explicitly specified:
    (unless dimensional-values-seen 
      (push '(:dimensional-values) options))
    (unless initial-space-instances-seen
      (push '(:initial-space-instances) options)))
  options)

;;; ---------------------------------------------------------------------------

(defun finish-unit-class-loading (unit-class fixup-symbols)
  (fixup-function-objects-part2 unit-class fixup-symbols)
  (let ((class-name (class-name unit-class)))
    ;; Clear space-instance storage caches (can't use do-space-instances yet):
    (when (fboundp 'map-space-instances) ;; don't run when loading GBBopen!
      (flet ((fn (space-instance)
               (delete-space-instance-caches space-instance class-name)))
        (declare (dynamic-extent #'fn))
        (map-space-instances #'fn '(*)))))
  ;; Clear memoized dlslotd caches:
  (clear-memoized-dlslotd-caches unit-class)
  (compute-inherited-unit-class-values unit-class))

;;; ---------------------------------------------------------------------------

#+under-construction
(defun ensure-unit-class (event-class-name direct-superclass-names
			  &key direct-default-initargs direct-slots
			       direct-superclasses name metaclass 
			  &allow-other-keys)
  (nyi))

;;; ---------------------------------------------------------------------------

(defmacro define-unit-class (unit-class-name direct-superclass-names
                             direct-slots &rest options &environment env)
  ;; () value for direct-superclass-names is (standard-unit-instance):
  (when (and (null direct-superclass-names)
             (not (eq unit-class-name 'standard-unit-instance)))
    (setf direct-superclass-names '(standard-unit-instance)))
  (unless (flet ((fn (element)
                   (and (symbolp element) (not (keywordp element)))))
            (declare (dynamic-extent #'fn))
            (every #'fn direct-superclass-names))
    (error "Illegal direct-superclasses syntax in define-unit-class: ~s."
	   direct-superclass-names))
  ;; To support function objects and closures in :initial-space-instances and
  ;; in :sort-function and :sort-key for links we create a let binding with
  ;; gensym'ed symbols and the function objects, replacing the function
  ;; objects in the defclass form with the gensyms.  Then, at load time, we
  ;; replace the gensyms in the generated class object with the compiled
  ;; function objects from the let binding.
  (let ((*%%fixup-function-objects%%* nil))
    (multiple-value-bind (clos-direct-slots clos-class-options exports)
        (parse-define-class 
         unit-class-name direct-slots 
         (if (member :metaclass (the list options)
                     :test #'eq
                     :key #'first)
             options
             (cons '(:metaclass standard-unit-class) options))
         *standard-define-class-options*
         env)
      ;; We must tell ECL and Lispworks to use accessor methods that call
      ;; slot-value-using-class:
      #+(or ecl-no-longer lispworks)
      (setf clos-class-options 
        (cons '(:optimize-slot-access nil) clos-class-options))
      `(#-clisp progn
	;; CLISP requires let (rather than progn) to work around CLISP's
	;; non-eq uninterned symbols bug (still observed in CLISP 2.45); even
	;; so, more eq-lossage handling is required in
	;; fixup-function-objects-part2:
	#+clisp let #+clisp ()
	#+sbcl
	(eval-when (:compile-toplevel :load-toplevel :execute)
	  (sb-pcl::preinform-compiler-about-class-type ',unit-class-name))
	,@(when exports
	    `((eval-when (:compile-toplevel :load-toplevel :execute)
		(export ',exports))))
	(defclass ,unit-class-name ,direct-superclass-names
	  ,(fixup-function-initargs clos-direct-slots)
	  ,@(fixup-function-option-values 
	     unit-class-name
	     (class-prototype
	      (ensure-finalized-class
	       (find-class (second (assq :metaclass clos-class-options)))))
	     clos-class-options))
	;; Some CLs have problems using the class object returned by
	;; defclass (e.g., CMUCL and SBCL).  So, we'll always use
	;; find-class to get it back...
	,(with-gensyms (unit-class)
	   (let ((fixup-symbols (mapcar #'first *%%fixup-function-objects%%*)))
	     `(let ((,unit-class (find-class ',unit-class-name))
		    ,.*%%fixup-function-objects%%*)
		(declare (special ,@fixup-symbols))
		(finish-unit-class-loading ,unit-class ',fixup-symbols)
		,unit-class)))))))
  
;;; ===========================================================================
;;;   Unit-class mappers, utilities, etc.

(defun find-unit-class (unit-class-name)
  ;;; A unit-class-only version of find-class.  Signals an error if
  ;;; `unit-class-name' does not specify a unit class:
  (if (typep unit-class-name 'standard-unit-class)
      unit-class-name
      (let ((unit-class (find-class unit-class-name nil)))
        (unless (typep unit-class 'standard-unit-class)
          (error "~s does not name a unit class." unit-class-name))
        unit-class)))

;;; ---------------------------------------------------------------------------
;;;  Unit-class-in-specifier-p

(defun unit-class-in-specifier-p (unit-class specifier)
  ;; Returns true if `unit-class' is specified in the extended unit-classes
  ;; specifier list `specifier'. 
  (flet ((in-simple-specifier-p  (specifier)
	   (let ((plus-subclasses? nil))
	     ;; Convert extended-unit-class-specifier to simple and flag:
	     (when (consp specifier)
	       (destructuring-bind (unit-class-name subclass-indicator)
		   specifier
		 (setf specifier unit-class-name)
		 (ecase subclass-indicator
		   ((:plus-subclasses +) (setf plus-subclasses? 't))
		   ((:no-subclasses =)))))
	     (if (typep specifier 'standard-unit-class)
		 ;; we have a unit-class object
		 (if plus-subclasses?
		     (subtypep (class-name unit-class) (class-name specifier))
		     (eq (class-name unit-class) (class-name specifier)))
		 ;; anything else we assume is a unit-class-name:
		 (if plus-subclasses?
		     (subtypep (class-name unit-class) specifier)
		     (eq (class-name unit-class) specifier))))))
    (declare (dynamic-extent #'in-simple-specifier-p))
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq specifier 't) (typep unit-class 'standard-unit-class))
     ;; could be a single, extended unit-class specification or a list of
     ;; specifications:
     ((consp specifier)
      (if (and (list-length-2-p specifier)
	       (let ((possible-subclass-indicator (second specifier)))
		 (memq possible-subclass-indicator
                       '(:plus-subclasses + :no-subclasses =))))
          ;; simply a single, extended unit-class specification:
	  (in-simple-specifier-p specifier)
	  ;; a list of specifications:
	  (some #'in-simple-specifier-p specifier)))
     ;; a simple specifier:
     (t (in-simple-specifier-p specifier)))))

;;; ---------------------------------------------------------------------------

(defun map-unit-classes (fn class)
  ;;; Applies `fn' to `class' and all its subclasses.  The order is undefined,
  ;;; but `fn' will not be applied more than once to a class.
  (with-full-optimization ()
    (let ((classes-seen nil)
          (fn (if (functionp fn) fn (fdefinition fn))))
      (declare (function fn) (type list classes-seen))
      (labels ((doit (class)
                 (when (and (typep class 'standard-unit-class)
                            (not (memq class classes-seen)))
                   (push class classes-seen)
                   (funcall fn class 't)
                   (dolist (class (class-direct-subclasses class))
                     (doit class)))))
        (doit class)))))

;;; ---------------------------------------------------------------------------

(defun map-extended-unit-classes (fn unit-class-name)
  ;;; Handles mapping for functions that accept an extended unit-class-name.
  ;;; The order is undefined, but `fn' will not be applied more than once to a
  ;;; class.
  (with-full-optimization ()
    (let ((fn (if (functionp fn) fn (fdefinition fn))))
      (multiple-value-bind (unit-class plus-subclasses)
          (parse-unit-class-specifier unit-class-name)
        (if plus-subclasses
            (map-unit-classes fn unit-class)
            (funcall fn unit-class nil))))))

;;; ---------------------------------------------------------------------------

(defun map-extended-unit-classes-sorted (fn unit-class-name)
  ;;; Handles mapping for functions that accept an extended unit-class-name.
  ;;; The `fn' will applied once to each class in lexical order.
  (with-full-optimization ()
    (let* ((classes nil)
           (fn (if (functionp fn) fn (fdefinition fn))))
      (declare (function fn))
      (flet ((accumulation-fn (&rest class-spec)
               (push class-spec classes)))
        (declare (dynamic-extent #'accumulation-fn))
        (multiple-value-bind (unit-class plus-subclasses)
            (parse-unit-class-specifier unit-class-name)
          (if plus-subclasses
              (map-unit-classes #'accumulation-fn unit-class)
              (funcall #'accumulation-fn unit-class nil)))
        (setf classes
              (flet ((fn (class-spec1 class-spec2)
                       (string<
                        ;; SBCL (last checked 1.0.50) does not infer symbol
                        ;; result from CLASS-NAME
                        (the symbol (class-name (first class-spec1)))
                        (the symbol (class-name (first class-spec2))))))
                (declare (dynamic-extent #'fn))
                (sort classes #'fn)))
        (dolist (class-spec classes)
          (apply fn class-spec))))))

;;; ---------------------------------------------------------------------------

(defun map-unit-classes-specifier (fn unit-classes-specifier)
  ;;; Handles mapping for functions that accept a unit-classes-specifier
  (with-full-optimization ()
    (let ((fn (if (functionp fn) fn (fdefinition fn))))
      (declare (function fn))
      (dolist (specifier (parse-unit-classes-specifier unit-classes-specifier))
        (destructuring-bind (unit-class . plus-subclasses)
            specifier
          (if plus-subclasses
              (map-unit-classes fn unit-class)
              (funcall fn unit-class nil)))))))

;;; ---------------------------------------------------------------------------

(defun find-effective-slot-definition-by-name (class slot-name)
  ;; (car (member ...)) with :test & :key often optimizes better than (find
  ;; ...):
  (car (member slot-name (class-slots class)
               :key #'slot-definition-name
               :test #'eq)))

(defcm find-effective-slot-definition-by-name (class slot-name)
  (let ((class-var '#:class))
    `(let ((,class-var (class-slots ,class)))
       ;; (car (member ...)) with :test & :key often optimizes
       ;; better than (find ...):
       (car (member ,slot-name ,class-var
                    :key #'slot-definition-name 
                    :test #'eq)))))

;;; ---------------------------------------------------------------------------

(defun map-unit-class-slots (fn unit-class slot-names)
  ;;; Applies `fn' to the slots of `unit-class' specified by `slot-names'.
  ;;; `Slot-names' can be t (all slots), a single slot-name, or a list of
  ;;; slot-names. 
  (declare (function fn))
  (let ((class-slots (class-slots unit-class))) 
    (declare (type list class-slots))
    (if (eq slot-names 't)
        ;; all slots:
        (mapc fn class-slots)
        ;; one or more specific slot names:
        (let ((slot-names (ensure-list slot-names)))
          (dolist (slot-name slot-names)
            (let ((slot 
                   ;; (car (member ...)) with :test & :key often optimizes
                   ;; better than (find ...):
                   (car (member slot-name class-slots 
                                :key #'slot-definition-name 
                                :test #'eq))))
              (if slot 
                  (funcall fn slot)
                  (error "Slot ~s does not exist in ~s."
                         slot-name 
                         unit-class))))))))

;;; ---------------------------------------------------------------------------

(defun resize-instance-hash-table (unit-class new-size)
  ;;; Resize the instance-hash-table of `unit-class' to `new-size' if supported
  ;;; by the CL implementation 
  (resize-hash-table (standard-unit-class.instance-hash-table unit-class)
                     new-size))

;;; ---------------------------------------------------------------------------

(defmethod class-instances-count ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (class-instances-count '(standard-unit-instance :plus-subclasses))
      (class-instances-count (find-unit-class unit-class-name))))

(defmethod class-instances-count ((unit-class-spec cons))
  (let ((total 0))
    (flet ((fn (unit-class plus-subclasses)
             (declare (ignore plus-subclasses))
             (incf total (class-instances-count unit-class))))
      (declare (dynamic-extent #'fn))
      (map-extended-unit-classes #'fn unit-class-spec))
    ;; return the computed total:
    total))
    
(defmethod class-instances-count ((unit-class standard-unit-class))
  ;;; Returns the number of instances (in the instance-hash-table) of
  ;;; `unit-class-name-or-class'
  (hash-table-count (standard-unit-class.instance-hash-table unit-class)))
    
;;; ---------------------------------------------------------------------------

(defmethod class-instances-summary ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (class-instances-summary '(standard-unit-instance :plus-subclasses))
      (class-instances-summary (find-unit-class unit-class-name))))

(defmethod class-instances-summary ((unit-class-spec cons))
  (flet ((fn (unit-class plus-subclasses)
           (declare (ignore plus-subclasses))
           (class-instances-summary unit-class)))
    (declare (dynamic-extent #'fn))
    (map-extended-unit-classes-sorted #'fn unit-class-spec)))
    
(defmethod class-instances-summary ((unit-class standard-unit-class))
  ;;; Prints a descriptive summary of `unit-class-name-or-class'
  (let ((class-name (class-name unit-class)))
    (if (standard-unit-class.abstract unit-class)
        (format t "~&~s:~%~2tAbstract~%" class-name)
        (format t "~&~s:~%~2tInstances: ~d~%~2tCounter: ~d~%"
                class-name
                (class-instances-count unit-class)
                (standard-unit-class.instance-name-counter unit-class)))))

;;; ---------------------------------------------------------------------------

(defmethod reset-unit-class ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (reset-unit-class '(standard-unit-instance :plus-subclasses))
      (reset-unit-class (find-unit-class unit-class-name))))
    
(defmethod reset-unit-class ((unit-class-spec cons))
  (flet ((fn (unit-class plus-subclasses)
           (declare (ignore plus-subclasses))
           (reset-unit-class unit-class)))
    (declare (dynamic-extent #'fn))
    (map-extended-unit-classes #'fn unit-class-spec)))
    
(defmethod reset-unit-class ((unit-class standard-unit-class))
  ;;; Resets the instance count of a unit-class-name-or-class and creates a new
  ;;; instance hash table, but only if the class is non-abstract
  ;;; and has no existing instances
  (when (and (class-finalized-p unit-class)
             (not (or (standard-unit-class.abstract unit-class)
                      (standard-unit-class.use-global-instance-name-counter
                       unit-class))))
    (let ((count (class-instances-count unit-class)))
      (cond
       ((zerop count)
        (setf (standard-unit-class.instance-name-counter unit-class)
              (initial-class-instance-number (class-prototype unit-class)))
        (make-instance-name-hash-table unit-class))
       (t (warn "Unit class ~s has ~s instance~:p; not reset"
                (class-name unit-class)
                count))))))

;;; ===========================================================================
;;;   Unit-class describing

(defmethod describe-unit-class ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (describe-unit-class '(standard-unit-instance :plus-subclasses))
      (describe-unit-class (find-unit-class unit-class-name))))

(defmethod describe-unit-class ((unit-class-spec cons))
  (flet ((fn (unit-class plus-subclasses)
           (declare (ignore plus-subclasses))
           (describe-unit-class unit-class)))
    (declare (dynamic-extent #'fn))
    (map-extended-unit-classes #'fn unit-class-spec)))
    
(defmethod describe-unit-class ((unit-class standard-unit-class))
  (ensure-finalized-class unit-class)
  (format t "~&~@(~s~) ~s~:[~; (abstract)~]" 
          (type-of unit-class)
          unit-class
          (standard-unit-class.abstract unit-class))
  (let* ((supers (class-direct-superclasses unit-class))
         (subs (class-direct-subclasses unit-class))
         (hidden-slot-names
          (hidden-nonlink-slot-names (class-prototype unit-class)))
         (direct-slots
          (sort 
           (flet ((fn (slot)
                    (memq (slot-definition-name slot)
                          hidden-slot-names)))
             (declare (dynamic-extent #'fn))
             (delete-if #'fn (copy-list (class-direct-slots unit-class))))
           #'string<
           :key #'slot-definition-name))
         (effective-slots
          (sort
           (flet ((fn (slot)
                    (memq (slot-definition-name slot)
                          hidden-slot-names)))
             (declare (dynamic-extent #'fn))
             (delete-if #'fn (copy-list (class-slots unit-class))))
           #'string<
           :key #'slot-definition-name)))
    (flet ((name&abstract (class)
             (values (class-name class)
                     (and (typep class 'standard-unit-class)
                          (standard-unit-class.abstract class)))))
      (declare (dynamic-extent #'name&abstract))
      (format t "~&~2tDirect superclasses:")
      (format-column 4 supers "~s~:[~; (abstract)~]" #'name&abstract)
      (format t "~&~2tDirect subclasses:")
      (format-column 4 subs "~s~:[~; (abstract)~]" #'name&abstract))
    ;; Direct nonlink slots:
    (format t "~&~2tDirect nonlink slots:~@[ None~%~]"
            (flet ((fn (slot) 
                     (not (typep slot 'direct-link-definition))))
              (declare (dynamic-extent #'fn))
              (notany #'fn direct-slots)))
    (dolist (slot direct-slots)
      (unless (typep slot 'direct-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Direct link slots:
    (format t "~&~2tDirect link slots:~@[ None~%~]"
            (flet ((fn (slot) 
                     (typep slot 'direct-link-definition)))
              (declare (dynamic-extent #'fn))
              (notany #'fn direct-slots)))
    (dolist (slot direct-slots)
      (when (typep slot 'direct-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Effective nonlink slots:
    (format t "~&~2tEffective nonlink slots:")
    (dolist (slot effective-slots)
      (unless (typep slot 'effective-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Effective link slots:
    (format t "~&~2tEffective link slots:~@[ None~%~]"
            (flet ((fn (slot) 
                     (typep slot 'effective-link-definition)))
              (declare (dynamic-extent #'fn))
            (notany #'fn effective-slots)))
    (dolist (slot effective-slots)
      (when (typep slot 'effective-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Dimensional values:
    (let ((cdvs 
           (sort
            (copy-list (standard-unit-class.dimensional-values 
                        unit-class))
            #'string<
            :key #'cdv.dimension-name)))
      (format t "~&~2tDimensional values:~@[ None~%~]"
              (null cdvs))
      (dolist (cdv cdvs)
        (describe-dimensional-value cdv)))
    ;; Effective dimensional values:
    (let ((ecdvs 
           (sort
            (copy-list (standard-unit-class.effective-dimensional-values 
                        unit-class))
            #'string<
            :key #'cdv.dimension-name)))
      (format t "~&~2tEffective dimensional values:~@[ None~%~]"
              (null ecdvs))
      (dolist (cdv ecdvs)
        (describe-dimensional-value cdv)))
    ;; Initial space instances:
    (let ((space-instances 
           (ensure-list
            (standard-unit-class.initial-space-instances
             unit-class))))
      (format t "~&~2tInitial space instances:~@[ None~%~]"
              (null space-instances))
      (dolist (space-instance space-instances)
        (describe-initial-space-instance unit-class space-instance)))
    ;; Effective initial space instances:
    (let ((space-instances 
           (ensure-list
            (standard-unit-class.effective-initial-space-instances
             unit-class))))
      (format t "~&~2tEffective initial space instances:~@[ None~%~]"
              (null space-instances))
      (dolist (space-instance space-instances)
        (describe-initial-space-instance unit-class space-instance)))
    ;; Retain:
    (format t "~&~2tRetain: ~s~%" (standard-unit-class.retain unit-class)))
  (values))
  
;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot standard-slot-definition))
  (declare (ignore unit-class))
  (format t "~&~4t~s" (slot-definition-name slot))
  (let ((tab-column 6))
    (maybe-format-labeled-entry 
     tab-column (slot-definition-allocation slot) ':allocation)
    (maybe-format-labeled-entry 
     tab-column (documentation slot t) ':documentation)
    (maybe-format-labeled-entry
     tab-column (slot-definition-initargs slot) ':initargs)
    (maybe-format-labeled-entry 
     tab-column (slot-definition-initfunction slot) ':initform
     (slot-definition-initform slot))))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class 
                               (slot standard-direct-slot-definition))
  (declare (ignore unit-class))
  (call-next-method)
  (let ((tab-column 6))
    (maybe-format-labeled-entry 
     tab-column (slot-definition-readers slot) ':readers)
    (maybe-format-labeled-entry 
     tab-column (slot-definition-writers slot) ':writers)))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot direct-link-definition))
  (call-next-method)
  (format t "~&~6t~s ~s ~:[<-- *** Inconsistent ***~;~]" 
          ':link 
          (direct-link-definition.inverse-link slot)
          (%check-link-definition unit-class slot nil nil))
  (when (direct-link-definition.singular slot)
    (format t "~&~6t~s ~s" ':singular 't)))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class
                               (slot standard-effective-slot-definition))
  (declare (ignore unit-class #+ecl slot))
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot effective-link-definition))
  (declare (ignore unit-class #+ecl slot))
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defun describe-dimensional-value (cdv)
  (format t "~&~4t~s (~s ~s)~@[ ~s~@[ ~s~]~]~%" 
          (cdv.dimension-name cdv)
          (cdv.dimension-value-type cdv)
          (cdv.comparison-type cdv)
          (cdv.composite-type cdv)
          (cdv.ordering-dimension-name cdv)))

;;; ---------------------------------------------------------------------------

(defun describe-initial-space-instance (unit-class space-instance-or-fn)
  (declare (ignore unit-class))
  (cond 
   ((functionp space-instance-or-fn)
    (format t "~&~4t")
    (print-pretty-function-object space-instance-or-fn)
    (terpri))
   (t (format t "~&~4t~s~%" space-instance-or-fn))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
