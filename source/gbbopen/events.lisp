;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/events.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Feb 24 18:11:40 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            Event Functions
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
;;;  05-04-03 File created.  (Corkill)
;;;  03-17-04 Added WITH-EVENTS-DISABLED/ENABLED.  (Corkill)
;;;  06-22-04 Initial instance-event functions/signaling.  (Corkill)
;;;  07-07-04 Added EVFN-PRINTV.  (Corkill)
;;;  07-15-04 Core space-instance-event functions/signaling.  (Corkill)
;;;  07-22-04 Support control-shell trigger removal.  (Corkill)
;;;  07-23-04 Event printing.  (Corkill)
;;;  06-13-05 Moved GBBopen event metaclass definitions to 
;;;           event-metaclasses.lisp. (Corkill)
;;;  02-22-11 Added INSTANCE-ADDED-TO-SPACE-INSTANCE-EVENT and
;;;           INSTANCE-REMOVED-FROM-SPACE-INSTANCE-EVENT.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(add-event-function
	    define-event-class 
	    describe-all-event-functions ; not yet documented
	    describe-event-function	; not yet documented
	    describe-event-printing
	    disable-event-printing
	    enable-event-printing
	    ensure-event-class		; not yet implemented/documented
	    event-printer
	    evfn-printv			; not documented
	    find-event-class		; not documented
	    remove-all-event-functions
	    remove-event-function
	    resume-event-printing
	    signal-event
            signal-event-using-class    ; not documented
	    suspend-event-printing
	    with-events-disabled
	    with-events-enabled)))

;;; ---------------------------------------------------------------------------

(defvar *%%events-enabled%%* 't)
(defvar *event-print-stream* *trace-output*)
;; Indicates when add/remove-event-functions are being performed on a new
;; space instance:
(defvar *%%doing-path-event-functions%%* nil)
;; Indicates when an instance is being initialized:
(defvar *%%doing-initialize-instance%%* nil)

;;; ---------------------------------------------------------------------------
;;;  Trivial streamer class (used by :streaming module)

(define-class %trivial-streamer% (standard-gbbopen-instance)
  ())
  
;;; ===========================================================================
;;;   Event disable/enable macros

(defmacro with-events-disabled ((&key) &body body)
  ;;; Disables event signaling during execution of `body'
  `(let ((*%%events-enabled%%* nil))
     ,@body))

;;; ---------------------------------------------------------------------------

(defmacro with-events-enabled ((&key) &body body)
  ;;; Enables event signaling during execution of `body'
  `(let ((*%%events-enabled%%* 't))
     ,@body))

;;; ===========================================================================
;;;   EVFN -- Event-function descriptor
;;;
;;;  A dotted pair (function . attributes), where attributes contains the
;;;  event-function priority value (bits 0-7) and the following flags:
;;;     propagate-event-classes 
;;;     propagate-unit-classes
;;;     all-slots-p
;;;     permanent

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype evfn-priority ()
    '(integer -127 127)))

(defun evfn.function (evfn)
  (car evfn))

(defcm evfn.function (evfn)
  `(car (the cons ,evfn)))

;;; ---------------------------------------------------------------------------
;;; Priority accessor:

(defun evfn.priority (evfn)
  (-& (ldb (byte 8 0) (cdr evfn)) 127))

(defcm evfn.priority (evfn)
  `(-& (ldb (byte 8 0) (cdr (the cons ,evfn))) 127))

(defun (setf evfn.priority) (nv evfn)
  (setf (ldb (byte 8 0) (cdr (the cons evfn))) (+& nv 127))
  nv)

;;; ---------------------------------------------------------------------------
;;; The flag accessors:

(macrolet 
    ((do-flag (flag index)
       `(progn
          (defun ,flag (evfn)
            (declare (cons evfn))
            (flag-set-p (cdr evfn) ,index))

          (defcm ,flag (evfn)
            `(flag-set-p (cdr (the cons ,evfn)) ,,index))
          
          (defun (setf ,flag) (nv evfn)
            (declare (cons evfn))
            (setf (cdr evfn)
                  (if nv
                      (set-flag (cdr evfn) ,index)
                      (clear-flag (cdr evfn) ,index)))
            nv))))

  (do-flag evfn.propagate-event-classes 8)
  (do-flag evfn.propagate-unit-classes 9)
  (do-flag evfn.all-slots-p 10)
  (do-flag evfn.permanent 11))

;;; ---------------------------------------------------------------------------

(defun make-evfn (function propagate-event-classes propagate-unit-classes
                  all-slots-p permanent priority)
  (check-type priority evfn-priority)
  (let ((flags (+& priority 127)))
    ;; Flags start after the priority field:
    (when propagate-event-classes (setf flags (set-flag flags 8)))
    (when propagate-unit-classes (setf flags (set-flag flags 9)))
    (when all-slots-p (setf flags (set-flag flags 10)))
    (when permanent (setf flags (set-flag flags 11)))
    (cons function flags)))

;;; ===========================================================================
;;;   EVSTREAMER -- Event-streamer descriptor
;;;
;;;  A dotted pair (streamer . flags), where flags are:
;;;     propagate-event-classes 
;;;     propagate-unit-classes
;;;     all-slots-p

(defun evstreamer.streamer (evstreamer)
  (car evstreamer))

(defcm evstreamer.streamer (evstreamer)
  `(car (the cons ,evstreamer)))

;;; ---------------------------------------------------------------------------
;;; The flag accessors:

(macrolet 
    ((do-flag (flag index)
       `(progn
          (defun ,flag (evstreamer)
            (declare (cons evstreamer))
            (flag-set-p (cdr evstreamer) ,index))

          (defcm ,flag (evstreamer)
            `(flag-set-p (cdr (the cons ,evstreamer)) ,,index))
          
          (defun (setf ,flag) (nv evstreamer)
            (declare (cons evstreamer))
            (setf (cdr evstreamer)
                  (if nv
                      (set-flag (cdr evstreamer) ,index)
                      (clear-flag (cdr evstreamer) ,index)))
            nv))))

  (do-flag evstreamer.propagate-event-classes 8)
  (do-flag evstreamer.propagate-unit-classes 9)
  (do-flag evstreamer.all-slots-p 10))

;;; ---------------------------------------------------------------------------

(defun make-evstreamer (streamer propagate-event-classes
                        propagate-unit-classes all-slots-p)
  (let ((flags 0))
    ;; Use the same flag indexes as MAKE-EVFN:
    (when propagate-event-classes (setf flags (set-flag flags 8)))
    (when propagate-unit-classes (setf flags (set-flag flags 9)))
    (when all-slots-p (setf flags (set-flag flags 10)))
    (cons streamer flags)))

;;; ===========================================================================
;;;   Event metaclass validation

;;; ---------------------------------------------------------------------------
;;;                       Allowed Event Superclass Metaclasses
;;;                |----------------------------------------------------
;;;     Event      |   non-   |          |  space-  | nonlink- | link- |
;;;   Metaclass    | instance | instance | instance |   slot   | slot  |
;;; --------------------------------------------------------------------
;;; non-instance   |     X    |          |          |          |       |
;;; --------------------------------------------------------------------
;;; instance       |     X    |     X    |          |          |       |
;;; --------------------------------------------------------------------
;;; space-instance |     X    |     X    |     X    |          |       |
;;; --------------------------------------------------------------------
;;; nonlink-slot   |     X    |     X    |          |     X    |       |
;;; --------------------------------------------------------------------
;;; link-slot      |     X    |     X    |          |          |   X   |
;;; --------------------------------------------------------------------

(defmethod validate-event-metaclass ((event-metaclass-name
                                      (eql 'non-instance-event-class))
                                     super-event-metaclasses)
  #+ecl (declare (ignore event-metaclass-name))
  (flet ((fn (super-metaclass)
           (subtypep super-metaclass 'non-instance-event-class)))
    (declare (dynamic-extent #'fn))
    (every #'fn super-event-metaclasses)))

;;; ---------------------------------------------------------------------------

(defmethod validate-event-metaclass ((event-metaclass-name
                                      (eql 'instance-event-class))
                                     super-event-metaclasses)
  #+ecl (declare (ignore event-metaclass-name))
  (flet ((fn (super-metaclass)
           (or (subtypep super-metaclass 'non-instance-event-class)
               (subtypep super-metaclass 'instance-event-class))))
    (declare (dynamic-extent #'fn))
    (every #'fn super-event-metaclasses)))

;;; ---------------------------------------------------------------------------

(defmethod validate-event-metaclass ((event-metaclass-name
                                      (eql 'space-instance-event-class))
                                     super-event-metaclasses)
  #+ecl (declare (ignore event-metaclass-name))
  (flet ((fn (super-metaclass)
           (or (subtypep super-metaclass 'non-instance-event-class)
               (subtypep super-metaclass 'instance-event-class)
               (subtypep super-metaclass 'space-instance-event-class))))
    (declare (dynamic-extent #'fn))
    (every #'fn super-event-metaclasses)))

;;; ---------------------------------------------------------------------------

(defmethod validate-event-metaclass ((event-metaclass-name
                                      (eql 'nonlink-slot-event-class))
                                     super-event-metaclasses)
  #+ecl (declare (ignore event-metaclass-name))
  (flet ((fn (super-metaclass)
           (or (subtypep super-metaclass 'non-instance-event-class)
               (subtypep super-metaclass 'instance-event-class)
               (subtypep super-metaclass 'nonlink-slot-event-class))))
    (declare (dynamic-extent #'fn))
    (every #'fn super-event-metaclasses)))

;;; ---------------------------------------------------------------------------

(defmethod validate-event-metaclass ((event-metaclass-name
                                      (eql 'link-slot-event-class))
                                     super-event-metaclasses)
  #+ecl (declare (ignore event-metaclass-name))
  (flet ((fn (super-metaclass)
           (or (subtypep super-metaclass 'non-instance-event-class)
               (subtypep super-metaclass 'instance-event-class)
               (subtypep super-metaclass 'link-slot-event-class))))
    (declare (dynamic-extent #'fn))
    (every #'fn super-event-metaclasses)))

;;; ---------------------------------------------------------------------------

(defun compute-inherited-event-class-values (event-class)
  ;;; Determines directly inherited event-class values from the
  ;;; super-event-classes of `event-class' and then propagates them down to
  ;;; all sub-event-classes
  (let ((event-metaclass-name
         (standard-event-class.event-metaclass event-class))
        (superclasses (class-direct-superclasses event-class))
        (super-event-classes nil)
        (super-event-metaclasses nil))
    (dolist (super superclasses)
      (when (typep super 'standard-event-class)
        (push super super-event-classes)
        (pushnew (type-of super) super-event-metaclasses :test #'eq)))
    ;; Compute the event-metaclass-name if not specified directly:
    (unless event-metaclass-name
      (unless super-event-metaclasses
        (error "Unable to inherit a valid :metaclass for event-class ~s ~
                from super-classes ~s." 
               (class-name event-class)
               (mapcar #'class-name (class-direct-superclasses event-class))))
      (unless (list-length-1-p super-event-metaclasses)
        (error "Unable to inherit a unique :metaclass for event-class ~s.~@
                The superevents have the following event metaclasses: ~s.~@
                You must specify the :metaclass for this event-class ~
                definition."
               (class-name event-class)
               super-event-metaclasses))
      (setf event-metaclass-name (car super-event-metaclasses)))
    ;; Check that `event-metaclass-name' is consistent with its supers:
    (unless (validate-event-metaclass event-metaclass-name
                                      super-event-metaclasses)    
      (error "The :metaclass ~s for event-class ~s is incompatible with ~
            its superclasses ~s."
             event-metaclass-name 
             (class-name event-class)
             (mapcar #'class-name superclasses)))
    ;; Update the metaclass, if necessary; changing the metaclass is not
    ;; legal under the AMOP, so such a change will fail on many CLs:
    (unless (typep event-class event-metaclass-name)
      (warn "Changing the :metaclass of ~s from ~s to ~s.~@
             You should specify the :metaclass for this event-class ~
             definition."
	    (class-name event-class)
	    (type-of event-class) 
	    event-metaclass-name)
      (change-class event-class event-metaclass-name))
    ;; Propagate event functions and printing:
    (propagate-event-evfns/printing event-class super-event-classes)
    ;; Propagate to subs (naive, for now...):
    (dolist (sub (class-direct-subclasses event-class))
      (compute-inherited-event-class-values sub))))

;;; ---------------------------------------------------------------------------

#+under-construction
(defun ensure-event-class (event-class-name direct-superclass-names
			   &key direct-default-initargs direct-slots
				direct-superclasses name metaclass 
			   &allow-other-keys)
  (nyi))

;;; ===========================================================================
;;;   Define-event-class

(defmacro define-event-class (event-class-name direct-superclass-names
                              direct-slots &rest options &environment env)
  ;; () value for direct-superclass-names is (standard-event-instance):
  (when (and (null direct-superclass-names)
             (not (eq event-class-name 'standard-event-instance)))
    (setf direct-superclass-names '(standard-event-instance)))
  (multiple-value-bind (clos-direct-slots clos-class-options exports)
      (parse-define-class 
       event-class-name direct-slots 
       (let ((metaclass (second (assq :metaclass options)))
	     (event-metaclass (second (assq :event-metaclass options))))
	 (cond 
	  ;; both metaclass forms were specified, they must be eq:
	  ((and metaclass event-metaclass)
	   (unless (eq metaclass event-metaclass)
	     (error "Incompatible ~s and ~s were specified for ~s."
		    ':metaclass
		    ':event-metaclass
		    event-class-name))
	   options)
	  (metaclass
	   (cons `(:event-metaclass ,metaclass) options))
	  (t (cons `(:metaclass 
		     ,(or event-metaclass
			  ;; use a super-event-class's metaclass, if any
			  ;; can be determined:
                          (flet ((fn (class-name)
                                   (let ((class (find-class class-name nil)))
                                     (when (typep class 'standard-event-class)
                                       (standard-event-class.event-metaclass 
                                        class)))))
                            (declare (dynamic-extent #'fn))
                            (some #'fn direct-superclass-names))
			  'non-instance-event-class))
		   options))))
       *standard-define-class-options*
       env)
    `(progn
       ,@(when exports
	   `((eval-when (:compile-toplevel :load-toplevel :execute)
	       (export ',exports))))
       (defclass ,event-class-name ,direct-superclass-names 
	 ,clos-direct-slots
	 ,@clos-class-options) 
       ;; Some CLs have problems using the class object returned by
       ;; defclass (e.g., CMUCL and SBCL).  So, we'll always use
       ;; find-class to get it back...
       ,(with-gensyms (event-class)
	  `(let ((,event-class (find-class ',event-class-name)))
	     (finish-event-class-loading ,event-class)
	     ,event-class)))))

;;; ===========================================================================
;;;   Event-Function and Printing Propagation

(defmethod propagate-event-evfns/printing
    ((event-class non-instance-event-class) super-event-classes)
  #+ecl (declare (ignore event-class))
  (dolist (super-event-class super-event-classes)
    (typecase super-event-class
      #+not-yet
      (non-instance-event-class 
       (let ((evfn-blk (non-instance-event-class.evfn-blk super-event-class)))
	 (printv evfn-blk))))))

;;; ---------------------------------------------------------------------------

(defmethod propagate-event-evfns/printing 
    ((event-class instance-event-class) super-event-classes)
  (declare #+ecl (ignore event-class)
	   (ignore super-event-classes)))

;;; ---------------------------------------------------------------------------

(defmethod propagate-event-evfns/printing 
    ((event-class space-instance-event-class) super-event-classes)
  (declare #+ecl (ignore event-class)
	   (ignore super-event-classes)))

;;; ---------------------------------------------------------------------------

(defmethod propagate-event-evfns/printing 
    ((event-class nonlink-slot-event-class) super-event-classes)
  (declare #+ecl (ignore event-class)
	   (ignore super-event-classes)))

;;; ---------------------------------------------------------------------------

(defmethod propagate-event-evfns/printing 
    ((event-class link-slot-event-class) super-event-classes)
  (declare #+ecl (ignore event-class)
	   (ignore super-event-classes)))

;;; ---------------------------------------------------------------------------

(defun finish-event-class-loading (event-class)
  (compute-inherited-event-class-values event-class))

;;; ===========================================================================
;;;   Standard-Event-Instance
;;;
;;;   Standard-event-instance is defined here rather than in
;;;   system-events.lisp to support mappers, etc., below.

(with-generate-accessors-format (:prefix)

  (define-event-class standard-event-instance (standard-gbbopen-instance)
    (;; used in control shells to hold triggering context information:
     (ks-triggers :initform nil))
    (:abstract t)
    (:metaclass non-instance-event-class)
    (:generate-accessors t)
    (:export-class-name t)
    (:export-accessors t)
    (:system-event t))

  ;; Close with-generate-accessors-format:
  )

;;; ===========================================================================
;;;   Event-Class Mappers, Utilities, etc.

(defun evfn-printv (event-class &rest args)
  ;;; Useful event-function for debugging
  (declare (dynamic-extent args))
  (format *trace-output* "~&;; EVFN => ~s~{~^~%;;~11t~s ~s~}~%" 
	  (class-name event-class)
	  args))

;;; ---------------------------------------------------------------------------

(defun find-event-class (event-class-name)
  ;;; An event-class-only version of find-class.  Signals an error if
  ;;; `event-class-name' does not specify an event class:
  (if (typep event-class-name 'standard-event-class)
      event-class-name
      (let ((event-class (find-class event-class-name nil)))
        (unless (typep event-class 'standard-event-class)
          (error "~s does not name an event class." event-class-name))
        event-class)))

;;; ---------------------------------------------------------------------------

(defun parse-event-class-specifier (event-class-spec)
  (with-full-optimization ()
    (cond
     ;; 't is shorthand for '(standard-event-instance :plus-subevents):
     ((eq event-class-spec 't) 
      (values (load-time-value (find-class 'standard-event-instance)) 't))
     ;; extended event-class specification:
     ((consp event-class-spec)
      (destructuring-bind (event-class-name subclass-indicator)
          event-class-spec
        (let ((event-class (find-event-class event-class-name)))
          (values event-class 
                  (ecase subclass-indicator
                    ((:plus-subevents +) 't)
                    ((:no-subevents =) nil))))))
     ;; anything else we assume is a event-class-name or event-class:
     (t (values (find-event-class event-class-spec) nil)))))

;;; ---------------------------------------------------------------------------

(defun map-event-classes (fn &optional 
                            (class (load-time-value (find-class 'standard-event-instance))))
  ;;; Applies `fn' to `class' and all its subclasses.  The order is undefined,
  ;;; but `fn' will not be applied more than once to a class.
  (with-full-optimization ()
    (let ((classes-seen nil))
      (labels ((doit (class)
                 (when (and (typep class 'standard-event-class)
                            (not (memq class classes-seen)))
                   (push class classes-seen)
                   (funcall (if (functionp fn) fn (fdefinition fn)) class 't)
                   (dolist (class (class-direct-subclasses class))
                     (doit class)))))
        (doit class)))))

;;; ---------------------------------------------------------------------------

(defun map-extended-event-classes (fn event-class-name)
  ;;; Handles mapping for functions that accept an extended event-class-name
  (with-full-optimization ()
    (multiple-value-bind (event-class plus-subevents)
        (parse-event-class-specifier event-class-name)
      (if plus-subevents
          (map-event-classes fn event-class)
          (funcall (if (functionp fn) fn (fdefinition fn)) event-class nil)))))

;;; ---------------------------------------------------------------------------

(defun parse-event-function-args (args &rest additional-arg-specs)
  ;;; Parses the optional/keyword argument syntax used by event-function and 
  ;;; event-printing manipulators:
  ;;;
  ;;;  [unit-classes-spec]
  ;;;  &key :slot-names | :slot-name 
  ;;;       :paths | :path
  ;;;       [additional keyword args]
  ;;;
  ;;; Returns: unit-classes-spec slot-names paths [additional-arg-value*]
  ;;;
  ;;; Additional keyword arg values are returned after the standard
  ;;; args in the order specified in the additional-arg-specs parameter.
  
  (let ((supplied-additional-args nil)
        (additional-arg-values nil)
        ;; default values:
        (unit-classes-spec t)
        (slot-names t)
        (paths '(*)))
    (declare (type list supplied-additional-args))
    (when args
      ;; a unit-classes-spec was supplied:
      (unless (keywordp (first args))
        (setf unit-classes-spec (pop args)))
      ;; keyword processing:
      (let ((key-slot-names unbound-value-indicator)
            (key-paths unbound-value-indicator))
        (do ((ptr args (cddr ptr)))
            ((null ptr))
          (unless (consp (cdr ptr))
            (error "~s list is not even." '&key))
          (case (first ptr)
            ((:slot-name :slot-names)
             (if (eq key-slot-names unbound-value-indicator)
                 (setf key-slot-names (second ptr))
                 (error "Multiple ~s or ~s keyword arguments were specified."
                        ':slot-name ':slot-names)))
            ((:path :paths)
             (if (eq key-paths unbound-value-indicator)
                 (setf key-paths (second ptr))
                 (error "Multiple ~s or ~s keyword arguments were specified."
                        ':path ':paths)))
            (otherwise
             (cond ((member (first ptr) additional-arg-specs
                            :test #'eq :key #'car)
                    (push `(,(first ptr) ,(second ptr)) 
                          supplied-additional-args))
                   (t (error "Illegal keyword argument: ~s" (first ptr)))))))
        (unless (eq unbound-value-indicator key-slot-names)
          (setf slot-names key-slot-names))
        (unless (eq unbound-value-indicator key-paths)
          (setf paths key-paths)))
      (setf supplied-additional-args (nreverse supplied-additional-args)))
    (setf additional-arg-values 
          (flet ((fn (arg-spec)
                   (let ((supplied-value-pair
                          ;; (car (member ...)) with :test & :key often
                          ;; optimizes better than (find ...):
                          (car (member (first arg-spec)
                                       supplied-additional-args
                                       :test #'eq :key #'car))))
                     (if (null supplied-value-pair) 
                         (second arg-spec)
                         (second supplied-value-pair)))))
            (declare (dynamic-extent #'fn))
            (mapcar #'fn additional-arg-specs)))
    (apply #'values unit-classes-spec slot-names paths additional-arg-values)))

;;; ---------------------------------------------------------------------------

(defun instance-event-functions-nyi ()
  (error "Instance event functions are not yet available in GBBopen"))

;;; ===========================================================================
;;;   Add event function
;;;
;;; The add/remove-event-function machinery also provides (undocumented)
;;; services for controling event printing and to control-shell triggering
;;; when the supplied `fn' is nil.  When the `fn' is a streamer, mirroring
;;; support is performed.
;;;
;;; TODO: Still need to deal with propagation to post-add event/unit class
;;;       creation

(defun evfn-adder (evfn-blk fn event-class plus-subevents
		   plus-subclasses all-slots-p permanent priority)
  (when fn
    (let ((evfns 
           (flet ((do-fn (evfn)
                    (and (eq fn (evfn.function evfn))
                         (or permanent
                             (not (evfn.permanent evfn))
                             (error "Permanent event-function ~s cannot ~
                                        be redefined for event-class ~s."
                                    fn
                                    (class-name event-class))))))
             (declare (dynamic-extent #'do-fn))
             (delete-if #'do-fn (evfn-blk.evfns evfn-blk)))))
      (setf (evfn-blk.evfns evfn-blk)
            (nsorted-insert
             (make-evfn fn plus-subevents plus-subclasses all-slots-p
                        permanent priority)
             evfns
             #'>
             #'evfn.priority)))))

;;; ---------------------------------------------------------------------------

(defun streamer-adder (evfn-blk streamer plus-subevents plus-subclasses
                       all-slots-p)
  (setf (evfn-blk.evstreamers evfn-blk)
        (cons 
         (make-evstreamer streamer plus-subevents plus-subclasses all-slots-p)
         (delete streamer (evfn-blk.evstreamers evfn-blk) 
                 :key #'evstreamer.streamer))))

;;; ---------------------------------------------------------------------------

(defmethod addto-evfn-using-class (fn (event-class non-instance-event-class)
                                   plus-subevents
                                   unit-class/instance plus-subclasses
                                   slot-names paths permanent priority
                                   printing streamer
                                   evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore unit-class/instance paths))
  (let ((evfn-blk (non-instance-event-class.evfn-blk event-class)))
    (cond 
     ;; non-nil `fn':
     (fn (evfn-adder evfn-blk fn event-class 
                     plus-subevents plus-subclasses 
                     (eq slot-names 't) permanent priority))
     ;; nil `fn' with `printing':
     (printing
      (set-evfn-printing-flags evfn-blk printing 
                               plus-subevents plus-subclasses))
     ;; nil `fn' with `streamer':
     (streamer
      (streamer-adder evfn-blk streamer
                      plus-subevents plus-subclasses 
                      (eq slot-names 't)))
     ;; nil `fn' with `evfn-blk-fn':
     (evfn-blk-fn (apply (the function (symbol-function evfn-blk-fn))
                         evfn-blk plus-subevents plus-subclasses
                         evfn-blk-fn-args)))))

;;; ---------------------------------------------------------------------------

(defmethod addto-evfn-using-class (fn (event-class instance-event-class) 
                                   plus-subevents 
                                   unit-class/instance plus-subclasses
                                   slot-names paths permanent priority 
                                   printing streamer
                                   evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore paths))
  (flet ((add-it (unit-class)
           (let* ((evfn-blks (standard-unit-class.evfn-blks unit-class))
                  (evfn-blk 
                   (or (cdr (assq event-class evfn-blks))
                       (let ((new-evfn-blk
                              (make-evfn-blk :event-class event-class
                                             :unit-class unit-class)))
                         (push-acons event-class new-evfn-blk
                                     (standard-unit-class.evfn-blks
                                      unit-class))
                         new-evfn-blk))))
             (cond 
              ;; non-nil `fn':
              (fn (evfn-adder evfn-blk fn event-class 
                              plus-subevents plus-subclasses
                              (eq slot-names 't) permanent priority))
              ;; nil `fn' with `printing':
              (printing
               (set-evfn-printing-flags evfn-blk printing
                                        plus-subevents plus-subclasses))
              ;; nil `fn' with `streamer':
              (streamer
               (streamer-adder evfn-blk streamer 
                               plus-subevents plus-subclasses 
                               (eq slot-names 't)))
              ;; nil `fn' with `evfn-blk-fn':
              (evfn-blk-fn
               (apply (the function (symbol-function evfn-blk-fn))
                      evfn-blk plus-subevents plus-subclasses 
                      evfn-blk-fn-args))))))
    (if plus-subclasses
        (flet ((fn (unit-class plus-subclasses)
                 (declare (ignore plus-subclasses))
                 (add-it unit-class)))
          (declare (dynamic-extent #'fn))
          (map-unit-classes #'fn unit-class/instance))
        (if (typep unit-class/instance '(or standard-unit-instance cons))
            (instance-event-functions-nyi)
            (add-it unit-class/instance)))))
  
;;; ---------------------------------------------------------------------------

(defmethod addto-evfn-using-class (fn (event-class space-instance-event-class)
                                   plus-subevents 
                                   unit-class/instance plus-subclasses
                                   slot-names paths permanent priority 
                                   printing streamer
                                   evfn-blk-fn evfn-blk-fn-args)
  (flet ((do-space-instance (space-instance)
           (flet
               ((add-it (unit-class)
                  (let* ((evfn-blks 
                          (get-et
                           unit-class
                           (standard-space-instance.%%evfn-unit-ht%%
                            space-instance)))
                         (evfn-blk 
                          (or (cdr (assq event-class evfn-blks))
                              (let ((new-evfn-blk
                                     (make-evfn-blk :event-class event-class
                                                    :unit-class unit-class)))
                                (push-acons 
                                 event-class new-evfn-blk
                                 (get-et
                                  unit-class
                                  (standard-space-instance.%%evfn-unit-ht%%
                                   space-instance)))
                                new-evfn-blk))))
                    (cond
                     ;; non-nil `fn':
                     (fn (evfn-adder evfn-blk fn event-class
                                     plus-subevents plus-subclasses
                                     (eq slot-names 't) permanent priority))
                     ;; nil `fn' with `printing':
                     (printing
                      (set-evfn-printing-flags 
                       evfn-blk printing
                       plus-subevents plus-subclasses))
                     ;; nil `fn' with `streamer':
                     (streamer
                      (streamer-adder evfn-blk streamer
                                      plus-subevents plus-subclasses 
                                      (eq slot-names 't)))
                     ;; nil `fn' with `evfn-blk-fn':
                     (evfn-blk-fn
                      (apply (the function (symbol-function evfn-blk-fn))
                             evfn-blk plus-subevents plus-subclasses
                             evfn-blk-fn-args))))))
             (if plus-subclasses
                 (flet ((fn (unit-class plus-subclasses)
                          (declare (ignore plus-subclasses))
                          (add-it unit-class)))
                   (declare (dynamic-extent #'fn))
                   (map-unit-classes #'fn unit-class/instance))
                 (add-it unit-class/instance)))))
    (declare (dynamic-extent #'do-space-instance))
    ;; save information for newly created space instances:
    (cond ((typep paths 'standard-space-instance)
           (do-space-instance paths))
          (t (unless *%%doing-path-event-functions%%*
               (let ((addto-form
                      (list paths 'addto-evfn-using-class
                            fn event-class
                            plus-subevents 
                            unit-class/instance plus-subclasses
                            slot-names paths permanent priority
                            printing streamer
                            evfn-blk-fn evfn-blk-fn-args)))
                 #+this-does-not-work-correctly
                 (pushnew addto-form
                          (space-instance-event-class.path-event-functions
                           event-class)
                          :test #'equal)
                 #-so-we-stick-with-this
                 (push addto-form
                       (space-instance-event-class.path-event-functions
                        event-class))))
             (map-space-instances #'do-space-instance paths)))))

;;; ---------------------------------------------------------------------------

(defun slot-evfn-adder (fn event-class                  
                        plus-subevents 
                        unit-class/instance plus-subclasses
                        slot-names paths permanent priority
                        printing streamer
                        evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore paths))
  (flet
      ((add-it (unit-class)
         (ensure-finalized-class unit-class)
         (flet ((do-fn (slot)
                  (when (typep slot 'gbbopen-effective-slot-definition)                                       
                    (let* ((evfn-blks
                            (gbbopen-effective-slot-definition.evfn-blks slot))
                           (evfn-blk 
                            (or (cdr (assq event-class evfn-blks))
                                (let ((new-evfn-blk
                                       (make-evfn-blk :event-class event-class
                                                      :unit-class unit-class
                                                      :slot-or-space-qualifier
                                                      slot)))
                                  (push-acons 
                                   event-class new-evfn-blk
                                   (gbbopen-effective-slot-definition.evfn-blks
                                    slot))
                                  new-evfn-blk))))
                      (cond
                       ;; non-nil `fn':
                       (fn (evfn-adder evfn-blk fn event-class 
                                       plus-subevents plus-subclasses 
                                       (eq slot-names 't) permanent priority))
                       ;; nil `fn' with `printing':
                       (printing
                        (set-evfn-printing-flags 
                         evfn-blk printing
                         plus-subevents plus-subclasses))
                       ;; nil `fn' with `streamer':
                       (streamer
                        (let ((slot-name (slot-definition-name slot))
                              (class-prototype (class-prototype unit-class)))
                          (unless (or (memq slot-name
                                            (hidden-nonlink-slot-names
                                             class-prototype))
                                      (memq slot-name
                                            (omitted-slots-for-saving/sending 
                                             class-prototype)))
                            (streamer-adder evfn-blk streamer 
                                            plus-subevents plus-subclasses 
                                            (eq slot-names 't)))))
                       ;; nil `fn' with `evfn-blk-fn':
                       (evfn-blk-fn
                        (apply (the function (symbol-function evfn-blk-fn))
                               evfn-blk plus-subevents plus-subclasses
                               evfn-blk-fn-args)))))))
           (declare (dynamic-extent #'do-fn))
         (map-unit-class-slots #'do-fn unit-class slot-names))))
    (if plus-subclasses
        (flet ((fn (unit-class plus-subclasses)
                 (declare (ignore plus-subclasses))
                 (add-it unit-class)))
          (declare (dynamic-extent #'fn))
          (map-unit-classes #'fn unit-class/instance))
        (add-it unit-class/instance))))
  
;;; ---------------------------------------------------------------------------

(defmethod addto-evfn-using-class (fn (event-class nonlink-slot-event-class) 
                                   plus-subevents 
                                   unit-class/instance plus-subclasses
                                   slot-names paths permanent priority 
                                   printing streamer
                                   evfn-blk-fn evfn-blk-fn-args)
  (slot-evfn-adder fn event-class 
                   plus-subevents 
                   unit-class/instance plus-subclasses
                   slot-names paths permanent priority
                   printing streamer
                   evfn-blk-fn evfn-blk-fn-args))

;;; ---------------------------------------------------------------------------

(defmethod addto-evfn-using-class (fn (event-class link-slot-event-class)
                                   plus-subevents 
                                   unit-class/instance plus-subclasses
                                   slot-names paths permanent priority 
                                   printing streamer
                                   evfn-blk-fn evfn-blk-fn-args)
  (slot-evfn-adder fn event-class 
                   plus-subevents 
                   unit-class/instance plus-subclasses
                   slot-names paths permanent priority 
                   printing streamer
                   evfn-blk-fn evfn-blk-fn-args))

;;; ---------------------------------------------------------------------------

(defun add-event-function (fn &optional (event-classes-spec 't)
                           &rest args &aux streamer)
  ;;; Adds `fn' to the specified event signature(s).
  ;;; Also provides (undocumented) services for enabling/resuming event
  ;;; printing or adding KS triggers when the supplied `fn' is nil.
  (declare (dynamic-extent args))
  (unless (or (symbolp fn) (functionp fn) (typep fn '%trivial-streamer%))
    (error "~s is not a function." fn))
  (multiple-value-bind (unit-class-spec slot-names paths permanent priority
			printing evfn-blk-fn evfn-blk-fn-args)
      (parse-event-function-args args 
				 '(:permanent nil) '(:priority 0)
				 ;; used by enable/resume event printing:
				 '(:printing nil)
				 ;; used by control shells:
				 '(:evfn-blk-fn nil) '(:evfn-blk-fn-args nil))
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (when (typep fn '%trivial-streamer%)
        (setf streamer fn 
              fn nil))
      (flet ((do-fn (event-class plus-subevents) 
               (addto-evfn-using-class 
                fn event-class plus-subevents unit-class/instance plus-subclasses
                slot-names paths permanent priority printing streamer
                evfn-blk-fn evfn-blk-fn-args)))
        (declare (dynamic-extent #'do-fn))
        (map-extended-event-classes #'do-fn event-classes-spec))))
  fn)

;;; ---------------------------------------------------------------------------
;;;   Remove event function
;;;
;;; The add/remove-event-function machinery also provides (undocumented)
;;; services for controling event printing and to a control shell when the
;;; supplied `fn' is nil.

(defun evfn-remover (evfn-blk fn event-class permanent)
  (when evfn-blk
    (let ((evfns 
           (flet ((do-fn (evfn)
                    (and (or (eq fn (evfn.function evfn))
                             ;; remove-all:
                             (eq fn 't))
                         (or permanent
                             (not (evfn.permanent evfn))
                             (unless (eq fn 't)
                               (error "Permanent event-function ~s cannot be ~
                                       removed for event-class ~s."
                                      fn
                                      (class-name event-class)))))))
             (declare (dynamic-extent #'do-fn))
             (delete-if #'do-fn (evfn-blk.evfns evfn-blk)))))
      (setf (evfn-blk.evfns evfn-blk) evfns))))

;;; ---------------------------------------------------------------------------

(defun streamer-remover (evfn-blk streamer)
  (when evfn-blk
    (setf (evfn-blk.evstreamers evfn-blk) 
          (delete streamer (evfn-blk.evstreamers evfn-blk) 
                  :key #'evstreamer.streamer))))

;;; ---------------------------------------------------------------------------

(defmethod rmfrom-evfn-using-class (fn (event-class non-instance-event-class)
                                    plus-subevents 
                                    unit-class plus-subclasses
                                    slot-names paths permanent 
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore plus-subevents unit-class plus-subclasses
                   slot-names paths))
  (let ((evfn-blk (non-instance-event-class.evfn-blk event-class)))
    (when evfn-blk
      (cond
       ;; non-nil `fn':
       (fn (evfn-remover evfn-blk fn event-class permanent))
       ;; nil `fn' with `printing':
       (printing
        (clear-evfn-printing-flags evfn-blk printing))
       ;; nil `fn' with `streamer':
       (streamer (streamer-remover evfn-blk streamer))
       ;; nil `fn' with `evfn-blk-fn':
       (evfn-blk-fn
        (apply (the function (symbol-function evfn-blk-fn))
               evfn-blk evfn-blk-fn-args))))))

;;; ---------------------------------------------------------------------------

(defmethod rmfrom-evfn-using-class (fn (event-class instance-event-class)
                                    plus-subevents 
                                    unit-class plus-subclasses
                                    slot-names paths permanent 
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore plus-subevents slot-names paths))
  (flet ((remove-it (unit-class)
           (let* ((evfn-blks (standard-unit-class.evfn-blks unit-class))
                  (evfn-blk (cdr (assq event-class evfn-blks))))
             (when evfn-blk
               (cond
                ;; non-nil `fn':
                (fn (evfn-remover evfn-blk fn event-class permanent))
                ;; nil `fn' with `printing':
                (printing
                 (clear-evfn-printing-flags evfn-blk printing))
                ;; nil `fn' with `streamer':
                (streamer (streamer-remover evfn-blk streamer))
                ;; nil `fn' with `evfn-blk-fn':
                (evfn-blk-fn
                 (apply (the function (symbol-function evfn-blk-fn))
                        evfn-blk evfn-blk-fn-args)))))))
    (if plus-subclasses
        (flet ((fn (unit-class plus-subclasses)
                 (declare (ignore plus-subclasses))
                 (remove-it unit-class)))
          (declare (dynamic-extent #'fn))
          (map-unit-classes #'fn unit-class))
        (remove-it unit-class))))

;;; ---------------------------------------------------------------------------

(defmethod rmfrom-evfn-using-class (fn (event-class space-instance-event-class)
                                    plus-subevents 
                                    unit-class plus-subclasses
                                    slot-names paths permanent
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore slot-names plus-subevents))
  (flet ((do-space-instance (space-instance)
           (flet ((remove-it (unit-class)
                    (let* ((evfn-blks 
                            (get-et
                             unit-class
                             (standard-space-instance.%%evfn-unit-ht%%
                              space-instance)))
                           (evfn-blk (cdr (assq event-class evfn-blks))))
                      (when evfn-blk
                        (cond
                         ;; non-nil `fn':
                         (fn 
                          (evfn-remover evfn-blk fn event-class permanent))
                         ;; nil `fn' with `printing':
                         (printing
                          (clear-evfn-printing-flags evfn-blk printing))
                         ;; nil `fn' with `streamer':
                         (streamer
                          (streamer-remover evfn-blk streamer))
                         ;; nil `fn' with `evfn-blk-fn':
                         (evfn-blk-fn
                          (apply (the function (symbol-function evfn-blk-fn))
                                 evfn-blk evfn-blk-fn-args)))))))
             (if plus-subclasses
                 (flet ((fn (unit-class plus-subclasses)
                          (declare (ignore plus-subclasses))
                          (remove-it unit-class)))
                   (declare (dynamic-extent #'fn))
                   (map-unit-classes #'fn unit-class))
                 (remove-it unit-class)))))
    (cond ((typep paths 'standard-space-instance)
           (do-space-instance paths))
          (t (map-space-instances #'do-space-instance paths)
             ;; Delete existing entries when the removal makes them
             ;; superflous:
             (setf (space-instance-event-class.path-event-functions 
                    event-class)
                   (flet ((do-fn (entry)
                            (destructuring-bind (path-pattern add/remove-fn 
                                                 entry-fn entry-event-class 
                                                 &rest entry-args)
                                entry
                              (declare (ignore add/remove-fn entry-event-class))
                              (and (or (and (eq fn 't) entry-fn)
                                       (and fn (eq fn entry-fn))
                                       ;; for ks-trigger processing:
                                       (and (not fn)
                                            ;; see if the evfn-blk-fn-args match:
                                            (equal (tenth entry-args) evfn-blk-fn-args)))
                                   (subsumed-path-pattern-p 
                                    path-pattern paths)))))
                     (declare (dynamic-extent #'do-fn))
                     (delete-if 
                      #'do-fn 
                      (space-instance-event-class.path-event-functions event-class))))
             ;; Include this operation, just to be safe:
             #+should-never-be-needed
             (when (and (not *%%doing-path-event-functions%%*)
                        (space-instance-event-class.path-event-functions
                         event-class))
               (push (list paths 'rmfrom-evfn-using-class
                           fn event-class plus-subevents 
                           unit-class plus-subclasses
                           slot-names paths permanent 
                           printing streamer
                           evfn-blk-fn evfn-blk-fn-args)
                     (space-instance-event-class.path-event-functions
                      event-class)))))))

;;; ---------------------------------------------------------------------------

(defun slot-evfn-remover (fn event-class plus-subevents
                          unit-class plus-subclasses
                          slot-names paths permanent 
                          printing streamer
                          evfn-blk-fn evfn-blk-fn-args)
  (declare (ignore plus-subevents paths))
  (flet
      ((remove-it (unit-class)
         ;; nothing to remove on non-finalized classes:
         (when (class-finalized-p unit-class)
           (flet ((do-fn (slot)
                    (when (typep slot 'gbbopen-effective-slot-definition)
                      (let* ((evfn-blks
                              (gbbopen-effective-slot-definition.evfn-blks slot))
                             (evfn-blk 
                              (cdr (assq event-class evfn-blks))))
                        (when evfn-blk
                          (cond
                           ;; non-nil `fn':
                           (fn
                            (evfn-remover evfn-blk fn event-class permanent))
                           ;; nil `fn' with `printing':
                           (printing
                            (clear-evfn-printing-flags evfn-blk printing))
                           ;; nil `fn' with `streamer':
                           (streamer
                            (streamer-remover evfn-blk streamer))
                           ;; nil `fn' with `evfn-blk-fn':
                           (evfn-blk-fn
                            (apply (the function (symbol-function evfn-blk-fn))
                                   evfn-blk evfn-blk-fn-args))))))))
             (declare (dynamic-extent #'do-fn))
             (map-unit-class-slots #'do-fn unit-class slot-names)))))
    (if plus-subclasses
        (flet ((fn (unit-class plus-subclasses)
                 (declare (ignore plus-subclasses))
                 (remove-it unit-class)))
          (declare (dynamic-extent #'fn))
          (map-unit-classes #'fn unit-class))
        (remove-it unit-class))))
  
;;; ---------------------------------------------------------------------------

(defmethod rmfrom-evfn-using-class (fn (event-class nonlink-slot-event-class) 
                                    plus-subevents 
                                    unit-class plus-subclasses
                                    slot-names paths permanent 
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args)
  (slot-evfn-remover fn event-class plus-subevents
                     unit-class plus-subclasses
                     slot-names paths permanent 
                     printing streamer
                     evfn-blk-fn evfn-blk-fn-args))

;;; ---------------------------------------------------------------------------

(defmethod rmfrom-evfn-using-class (fn (event-class link-slot-event-class) 
                                    plus-subevents 
                                    unit-class plus-subclasses
                                    slot-names paths permanent 
                                    printing streamer
                                    evfn-blk-fn evfn-blk-fn-args)
  (slot-evfn-remover fn event-class plus-subevents
                     unit-class plus-subclasses
                     slot-names paths permanent 
                     printing streamer
                     evfn-blk-fn evfn-blk-fn-args))

;;; ---------------------------------------------------------------------------

(defun remove-event-function (fn &optional (event-classes-spec 't)
                              &rest args &aux streamer)
  ;;; Removes `fn' from the specified event signature(s).
  ;;; Also provides (undocumented) services for disabling/suspending event
  ;;; printing or removing KS triggers when the supplied `fn' is nil.
  (declare (dynamic-extent args))
  (unless (or (symbolp fn) (functionp fn) (typep fn '%trivial-streamer%))
    (error "~s is not a function." fn))
  (multiple-value-bind (unit-class-spec slot-names paths permanent 
			printing evfn-blk-fn evfn-blk-fn-args)
      (parse-event-function-args args 
                                 '(:permanent nil)
				 ;; used by enable/resume event printing:
				 '(:printing nil)
				 ;; used by control shells:
				 '(:evfn-blk-fn nil) '(:evfn-blk-fn-args nil))
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (when (typep fn '%trivial-streamer%)
        (setf streamer fn 
              fn nil))
      (flet ((do-fn (event-class plus-subevents) 
               (rmfrom-evfn-using-class 
                fn event-class plus-subevents unit-class/instance plus-subclasses
                slot-names paths permanent printing streamer
                evfn-blk-fn evfn-blk-fn-args)))
        (declare (dynamic-extent #'do-fn))
        (map-extended-event-classes #'do-fn event-classes-spec))))
  fn)

;;; ---------------------------------------------------------------------------

(defun remove-all-event-functions (&optional (event-classes-spec 't)
                                   &rest args)
  (declare (dynamic-extent args))
  (apply #'remove-event-function 't event-classes-spec args))

;;; ===========================================================================
;;;   Event printing

(defun enable-event-printing (&optional (event-classes-spec 't)
			      &rest args)
  (declare (dynamic-extent args))
  (apply #'add-event-function nil event-classes-spec 
	 (append args '(:printing #.printing-enabled-mask))))

;;; ---------------------------------------------------------------------------

(defun disable-event-printing (&optional (event-classes-spec 't)
			       &rest args)
  (declare (dynamic-extent args))
  (apply #'remove-event-function nil event-classes-spec 
	 (append args '(:printing #.printing-enabled-mask))))

;;; ---------------------------------------------------------------------------

(defun resume-event-printing (&optional (event-classes-spec 't)
			      &rest args)
  (declare (dynamic-extent args))
  (apply #'remove-event-function nil event-classes-spec 
	 (append args '(:printing #.printing-suspended-mask))))

;;; ---------------------------------------------------------------------------

(defun suspend-event-printing (&optional (event-classes-spec 't)
			       &rest args)
  (declare (dynamic-extent args))
  (apply #'add-event-function nil event-classes-spec 
	 (append args '(:printing #.printing-suspended-mask))))

;;; ===========================================================================
;;;   Describe event function

;;; Internal special bindings used to control class-name printing:
(defvar *%%event-class-name%%*)
(defvar *%%unit-class-name%%*)
(defvar *%%slot-or-space-instance-name%%*)

;;; ---------------------------------------------------------------------------

(defun show-evfn-describer-headers ()
  (when *%%event-class-name%%*
    (format t "~2&~s" *%%event-class-name%%*)
    (setf *%%event-class-name%%* nil))
  (when *%%unit-class-name%%*
    (format t "~&~2t~s" *%%unit-class-name%%*)
    (setf *%%unit-class-name%%* nil))
  (when *%%slot-or-space-instance-name%%*
    (format t "~&~4t~s" *%%slot-or-space-instance-name%%*)
    (setf *%%slot-or-space-instance-name%%* nil)))

;;; ---------------------------------------------------------------------------

(defmethod evfn-describer (evfn-blk fn)
  (declare (type evfn-blk evfn-blk))
  (case fn
    ;; called by describe-event-printing:
    (describe-event-printing
     (when (evfn-blk.event-printing-enabled evfn-blk)
       (show-evfn-describer-headers)
       (when (evfn-blk.event-printing-suspended evfn-blk) 
	 (princ " [suspended]"))
       (terpri)))
    ;; called by describe-mirroring:
    (describe-mirroring
     (let ((evstreamers (evfn-blk.evstreamers evfn-blk)))
       (when evstreamers
         (show-evfn-describer-headers)
	 (format t "~&~4t~s~%" (mapcar #'evstreamer.streamer evstreamers)))))
    ;; called by describe-event-functions/describe-all-event-functions:
    (otherwise
     (dolist (evfn (evfn-blk.evfns evfn-blk))
       (when (or (eq fn (evfn.function evfn)) 
		 ;; describe-all:
		 (eq fn 't))
	 (show-evfn-describer-headers)
	 (format t "~&~4t~4d~:[ ~;P~] ~s~%"
		 (evfn.priority evfn)
		 (evfn.permanent evfn)
		 (evfn.function evfn)))))))

;;; ---------------------------------------------------------------------------

(defmethod ds-evfn-using-class (fn (event-class non-instance-event-class)
				&rest args)
  (declare (dynamic-extent args))
  (destructuring-bind (plus-subevents unit-class plus-subclasses
                       slot-names paths)
      args
    (declare (ignore plus-subevents unit-class plus-subclasses
                     slot-names paths))
    (let ((evfn-blk (non-instance-event-class.evfn-blk event-class))
	  (*%%event-class-name%%* (class-name event-class))
	  (*%%unit-class-name%%* nil)
	  (*%%slot-or-space-instance-name%%* nil))
      (when evfn-blk
	(evfn-describer evfn-blk fn)))))

;;; ---------------------------------------------------------------------------

(defmethod ds-evfn-using-class (fn (event-class instance-event-class)
				&rest args)
  (destructuring-bind (plus-subevents unit-class plus-subclasses
                       slot-names paths)
      args
    (declare (ignore plus-subevents slot-names paths))
    (flet ((describe-it (unit-class)
	     (let* ((evfn-blks (standard-unit-class.evfn-blks unit-class))
		    (evfn-blk (cdr (assq event-class evfn-blks)))
		    (*%%unit-class-name%%* (class-name unit-class)))
	       (when evfn-blk
		 (evfn-describer evfn-blk fn)))))
      (let ((*%%event-class-name%%* (class-name event-class))
	    (*%%slot-or-space-instance-name%%* nil))
	(if plus-subclasses
            (flet ((fn (unit-class plus-subclasses)
                     (declare (ignore plus-subclasses))
                     (describe-it unit-class)))
              (declare (dynamic-extent #'fn))
              (map-unit-classes #'fn unit-class))
            (describe-it unit-class))))))

;;; ---------------------------------------------------------------------------

(defmethod ds-evfn-using-class (fn (event-class space-instance-event-class)
				&rest args)
  (destructuring-bind (plus-subevents unit-class plus-subclasses
                       slot-names paths)
      args
    (declare (ignore plus-subevents slot-names))
    (flet
	((do-space-instance (space-instance)
	   (flet
	       ((describe-it (unit-class)
		  (let* ((evfn-blks 
			  (get-et
			   unit-class
			   (standard-space-instance.%%evfn-unit-ht%%
			    space-instance)))
			 (evfn-blk (cdr (assq event-class evfn-blks)))
			 (*%%unit-class-name%%* (class-name unit-class)))
		    (when evfn-blk
		      (evfn-describer evfn-blk fn)))))
	     (let ((*%%slot-or-space-instance-name%%* 
		    (instance-name-of space-instance)))
	       (if plus-subclasses
                   (flet ((fn (unit-class plus-subclasses)
                            (declare (ignore plus-subclasses))
                            (describe-it unit-class)))
                     (declare (dynamic-extent #'fn))
                     (map-unit-classes #'fn unit-class))
		   (describe-it unit-class))))))
      (let ((*%%event-class-name%%* (class-name event-class)))
	(cond ((typep paths 'standard-space-instance)
	       (do-space-instance paths))
	      (t (map-space-instances #'do-space-instance paths)))))))

;;; ---------------------------------------------------------------------------

(defun slot-evfn-describer (fn event-class args)
  (destructuring-bind (plus-subevents unit-class plus-subclasses
                       slot-names paths)
      args
    (declare (ignore plus-subevents paths))
    (flet
	((describe-it (unit-class)
	   ;; nothing to describe on non-finalized classes:
	   (when (class-finalized-p unit-class)
	     (let ((*%%unit-class-name%%* (class-name unit-class)))
               (flet ((do-fn (slot)
                        (when (typep slot 'gbbopen-effective-slot-definition)
                          (let* ((evfn-blks
                                  (gbbopen-effective-slot-definition.evfn-blks 
                                   slot))
                                 (evfn-blk 
                                  (cdr (assq event-class evfn-blks))))
                            (when evfn-blk
                              (let ((*%%slot-or-space-instance-name%%*
                                     (slot-definition-name slot)))
                                (evfn-describer evfn-blk fn)))))))
                 (declare (dynamic-extent #'do-fn))
                 (map-unit-class-slots 
                  #'do-fn unit-class slot-names))))))
      (let ((*%%event-class-name%%* (class-name event-class)))
	(if plus-subclasses
            (flet ((fn (unit-class plus-subclasses)
                     (declare (ignore plus-subclasses))
                     (describe-it unit-class)))
              (declare (dynamic-extent #'fn))
              (map-unit-classes #'fn unit-class))
	    (describe-it unit-class))))))
  
;;; ---------------------------------------------------------------------------

(defmethod ds-evfn-using-class (fn (event-class nonlink-slot-event-class)
				&rest args)
  (declare (dynamic-extent args))
  (slot-evfn-describer fn event-class args))

;;; ---------------------------------------------------------------------------

(defmethod ds-evfn-using-class (fn (event-class link-slot-event-class)
				&rest args)
  (declare (dynamic-extent args))
  (slot-evfn-describer fn event-class args))

;;; ---------------------------------------------------------------------------

(defun describe-event-function (fn &optional (event-classes-spec 't)
                                &rest args)
  ;;; Prints event signatures for 'fn' given the specified event signature(s).
  (declare (dynamic-extent args))
  (unless (or (symbolp fn) (functionp fn))
    (error "~s is not a function." fn))
  (multiple-value-bind (unit-class-spec slot-names paths)
      (parse-event-function-args args)
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (flet ((do-fn (event-class plus-subevents) 
               (ds-evfn-using-class fn event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths)))
        (declare (dynamic-extent #'do-fn))
        (map-extended-event-classes #'do-fn event-classes-spec))))
  (values))

;;; ---------------------------------------------------------------------------

(defun describe-all-event-functions (&optional (event-classes-spec 't)
                                     &rest args)
  (declare (dynamic-extent args))
  (apply #'describe-event-function 't event-classes-spec args))

;;; ---------------------------------------------------------------------------

(defun describe-event-printing (&optional (event-classes-spec 't)
				&rest args)
  ;;; Prints event printing settings for the specified event signature(s).
  (declare (dynamic-extent args))
  (multiple-value-bind (unit-class-spec slot-names paths)
      (parse-event-function-args args)
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (flet ((fn (event-class plus-subevents) 
               (ds-evfn-using-class 'describe-event-printing
                                    event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths)))
        (declare (dynamic-extent #'fn))
        (map-extended-event-classes #'fn event-classes-spec))))
  (values))

;;; ===========================================================================
;;;   Signal Event

(defmethod do-evfns (evfn-blk event-class args)
  ;;; This is a generic function to support an :after method in
  ;;; agenda-shell.lisp:
  (dolist (evfn (evfn-blk.evfns evfn-blk))
    (apply 
     ;; the evfn.function can be a function or a symbol:
     (coerce (evfn.function evfn) 'function)
     event-class
     args)))

;;; ---------------------------------------------------------------------------

(defun do-event-printing-and-evfns (evfn-blk event-class args)
  ;; Event printing is more intuitive when it occurs before running the
  ;; event functions:
  (when (and (do-event-printing evfn-blk)
	     *event-print-stream*)
    (let ((event-instance (class-prototype event-class)))
      (apply #'event-printer
	     event-instance *event-print-stream* args)))
  (do-evfns evfn-blk event-class args))

;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class :before ((event-class standard-event-class)
                                             &key &allow-other-keys)
  (ensure-finalized-class event-class)
  (when (standard-event-class.abstract event-class)      
    (error "Event class ~s is abstract and cannot be signalled."
           (class-name event-class)))) 
  
;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class ((event-class non-instance-event-class)
                                     &rest args)
  (declare (dynamic-extent args))
  (when *%%events-enabled%%*
    (let ((evfn-blk (non-instance-event-class.evfn-blk event-class)))
      (when evfn-blk
	(do-event-printing-and-evfns evfn-blk event-class args)))))

;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class ((event-class instance-event-class)
                                     &rest args 
				     &key instance
				     &allow-other-keys)
  (declare (dynamic-extent args))
  (when *%%events-enabled%%*
    (let* ((unit-class 
            (if (typep instance 'deleted-unit-instance)
                (original-class-of instance)
                (class-of instance)))
	   (evfn-blks (standard-unit-class.evfn-blks unit-class))
	   (evfn-blk (cdr (assq event-class evfn-blks))))
      (when evfn-blk
	(do-event-printing-and-evfns evfn-blk event-class args)
        (let ((evstreamers (evfn-blk.evstreamers evfn-blk)))
          (when evstreamers
            (apply-when-fboundp
             'do-instance-mirroring evstreamers event-class args)))))))

;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class ((event-class space-instance-event-class)
                                     &rest args 
				     &key instance space-instance
				     &allow-other-keys) 
  (declare (dynamic-extent args))
  (when *%%events-enabled%%*
    (let* ((unit-class (class-of instance))
	   (evfn-unit-ht 
	    (standard-space-instance.%%evfn-unit-ht%% space-instance))
	   (evfn-blks (get-et unit-class evfn-unit-ht))
	   (evfn-blk (cdr (assq event-class evfn-blks))))
      (when evfn-blk	
	(do-event-printing-and-evfns evfn-blk event-class args)
        (let ((evstreamers (evfn-blk.evstreamers evfn-blk)))
          (when evstreamers
            (apply-when-fboundp 
             'do-space-instance-mirroring evstreamers event-class args)))))))

;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class ((event-class nonlink-slot-event-class)
                                     &rest args 
				     &key slot
				     &allow-other-keys)
  (declare (dynamic-extent args))
  (when *%%events-enabled%%*
    (let* ((evfn-blks (gbbopen-effective-slot-definition.evfn-blks slot))
	   (evfn-blk (cdr (assq event-class evfn-blks))))
      (when evfn-blk
	(do-event-printing-and-evfns evfn-blk event-class args)
        (let ((evstreamers (evfn-blk.evstreamers evfn-blk)))
          (when evstreamers
            (apply-when-fboundp 
             'do-nonlink-slot-mirroring evstreamers event-class args)))))))

;;; ---------------------------------------------------------------------------

(defmethod signal-event-using-class ((event-class link-slot-event-class)
                                     &rest args
				     &key slot
				     &allow-other-keys)
  (declare (dynamic-extent args))
  (when *%%events-enabled%%*
    (let* ((evfn-blks (gbbopen-effective-slot-definition.evfn-blks slot))
	   (evfn-blk (cdr (assq event-class evfn-blks))))
      (when evfn-blk
	(do-event-printing-and-evfns evfn-blk event-class args)
        (let ((evstreamers (evfn-blk.evstreamers evfn-blk)))
          (when evstreamers
            (apply-when-fboundp 
             'do-link-slot-mirroring evstreamers event-class args)))))))

;;; ---------------------------------------------------------------------------

(defun signal-event (event-class-name &rest args)
  ;;; Public function.  Eventually, add a compiler macro that stashes
  ;;; the find-event-class lookup for constant event-class-names (and
  ;;; potentially beyond that to cache the evfn-blk lookup as well).
  (declare (dynamic-extent args))
  (apply #'signal-event-using-class (find-event-class event-class-name) args))

;;; ===========================================================================
;;;   Event Printing

(defmethod event-printer :before ((event standard-event-instance)
				  stream &rest args)
  (declare (ignore args))
  ;;; Prints the standard event signature line for all event printing:
  (let ((*print-case* ':upcase))
    (format stream "~&;; => ~a~%" (type-of event))))

;;; ---------------------------------------------------------------------------

(defmethod event-printer ((event standard-event-instance)
			  stream &rest args)
  ;;; Performs the standard `args' printing for all events.  Printing of
  ;;; `args' for specific events can be customized by defining more specific
  ;;; methods:
  #+ecl (declare (ignore event))
  (declare (dynamic-extent args))
  (while args
    (format stream "~&;;~6t~s ~w~%" (pop args) (pop args))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

