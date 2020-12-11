;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/instances.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 25 02:44:40 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Unit Instance Functions
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
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-18-02 File created.  (Corkill)
;;;  01-21-04 Added INSTANCE-SPACE-INSTANCES.  (Corkill)
;;;  05-07-04 Added WITH-CHANGING-DIMENSION-VALUES.  (Corkill)
;;;  01-06-05 Support :space-instances initarg values when initializing
;;;           standard-unit-instances.  (Corkill)
;;;  11-22-05 Move deletion-event signaling into an :around method.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  07-27-06 Move unit-class locking to the shared-initialize :around method.
;;;           (Corkill)
;;;  08-20-06 Added DO-INSTANCES-OF-CLASS & DO-SORTED-INSTANCES-OF-CLASS
;;;           syntactic sugar.  (Corkill)
;;;  09-04-06 Added INSTANCE-NAME-OF and SPACE-INSTANCES-OF in place of
;;;           INSTANCE-NAME and INSTANCE-SPACE-INSTANCES.  (Corkill)
;;;  09-06-06 Completed change-class support.  (Corkill)
;;;  03-09-07 Added FIND-INSTANCES-OF-CLASS (please don't abuse!).  (Corkill)
;;;  06-14-07 Removed INSTANCE-NAME and INSTANCE-SPACE-INSTANCES.  (Corkill)
;;;  07-03-07 Reworked instance-marks and locking to use only a single
;;;           mark-based-retrieval mark.  (Corkill)
;;;  04-04-08 Added FIND-ALL-INSTANCES-BY-NAME (please don't abuse!). 
;;;           (Corkill)
;;;  09-03-08 Added MAKE-INSTANCES-OF-CLASS-VECTOR (please don't abuse!). 
;;;           (Corkill)
;;;  04-15-08 Added *SKIP-DELETED-UNIT-INSTANCE-CLASS-CHANGE*.  (Corkill)
;;;  05-08-10 Added CHECK-INSTANCE-LOCATORS.  (Corkill)
;;;  01-20-11 Check for bound %%space-instances%% slot in INSTANCE-DELETED-P.
;;;           (Corkill)
;;;  01-31-11 Added optional errorp argument to FIND-INSTANCE-BY-NAME.  
;;;           (Corkill)
;;;  02-02-11 Signal INSTANCE-CREATED-EVENT in INITIALIZE-SAVED/SENT-INSTANCE 
;;;           (standard-unit-instance) method. (Corkill)
;;;  02-10-11 Added support for incomplete instances and internal 
;;;           *%%LOADING-COMPLETE-REPOSITORY%%* special variable.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*%%skip-gbbopen-shared-initialize-method-processing%%*
            gbbopen-tools::*forward-referenced-peak-count*
            gbbopen-tools::clear-flag
            gbbopen-tools::outside-reading-saved/sent-objects-block-error
            gbbopen-tools::possibly-translate-class-name
            gbbopen-tools::set-flag)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*skip-deleted-unit-instance-class-change*
            check-for-deleted-instance
            check-instance-locators
            delete-instance
            deleted-instance-class
            deleted/non-deleted-unit-instance ; class-name, not documented yet
            describe-instance
            describe-instance-slot-value
            do-instances-of-class
            do-sorted-instances-of-class
            find-instance-by-name
            find-all-instances-by-name
            find-instances-of-class
            incomplete-instance-p
            initial-class-instance-number
            instance-dimension-value
            instance-dimension-values
            instance-deleted-p
            instance-name               ; re-export
	    instance-name-of
            link-instance-of
            make-duplicate-instance
            make-duplicate-instance-changing-class
            make-instances-of-class-vector
            map-instances-of-class
            map-sorted-instances-of-class
            next-class-instance-number
            original-class              ; slot-name in a deleted-unit-instance
            root-space-instance         ; for old .bb support, delete eventually
	    space-instances-of
            standard-unit-instance
            unduplicated-slot-names     ; re-export
            with-changing-dimension-values)))

;;; ---------------------------------------------------------------------------
;;;  Bypasses the normal class change to DELETED-UNIT-INSTANCE by
;;;  DELETE-INSTANCE.  Use only for very high volume deleting and at your own
;;;  peril -- you've been warned!!

(defvar *skip-deleted-unit-instance-class-change* nil)

;; Indicates if a complete LOAD-BLACKBOARD-REPOSITORY is underway:
(defvar *%%loading-complete-repository%%* nil)

;;; ===========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-class deleted/non-deleted-unit-instance 
      (standard-gbbopen-instance)
    (instance-name)
    (:export-class-name t)
    (:export-accessors t)))

;;; ===========================================================================
;;;   Deleted Unit Instances

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-class deleted-unit-instance (deleted/non-deleted-unit-instance)
    (original-class)
    (:export-class-name t)
    (:export-accessors t)))

(defmethod print-instance-slots ((instance deleted-unit-instance) stream)
  (call-next-method)
  (format stream " ~s" (class-name (original-class-of instance)))
  (print-instance-slot-value instance 'instance-name stream))

;;; ---------------------------------------------------------------------------

(defun instance-deleted-p (instance)
  (or (typep instance 'deleted-unit-instance)
      (and (slot-boundp instance '%%space-instances%%)
           (eq (standard-unit-instance.%%space-instances%% instance) 
               ':deleted))))

(defcm instance-deleted-p (instance)
  (with-once-only-bindings (instance)
    `(or (typep ,instance 'deleted-unit-instance)
         (and (slot-boundp ,instance '%%space-instances%%)
              (eq (standard-unit-instance.%%space-instances%% ,instance) 
                  ':deleted)))))

;;; ---------------------------------------------------------------------------

(defun operation-on-deleted-instance (instance operation)
  (if operation
      (error "~s attempted with a deleted instance: ~s"
             operation instance)
      (error "Instance ~s has been deleted" instance)))

;;; ---------------------------------------------------------------------------

(defun check-for-deleted-instance (instance &optional operation)
  ;;; Generate an error if `instance' is a deleted unit instance
  (when (instance-deleted-p instance)
    (operation-on-deleted-instance instance operation)))

(defcm check-for-deleted-instance (instance &optional operation)
  (with-once-only-bindings (instance)
    `(when (instance-deleted-p ,instance) 
       (operation-on-deleted-instance ,instance ,operation))))

;;; ===========================================================================
;;;   Incomplete (Forward Referenced) Unit Instances

(defun incomplete-instance-p (instance)
  (and (slot-boundp instance '%%marks%%)
       (incomplete-instance-mark-set-p instance)))

(defcm incomplete-instance-p (instance)
  (with-once-only-bindings (instance)
    `(and (slot-boundp ,instance '%%marks%%)
          (incomplete-instance-mark-set-p ,instance))))

;;; ---------------------------------------------------------------------------

(defun operation-on-incomplete-instance (instance operation)
  (if operation
      (error "~s attempted with an incomplete instance: ~s"
             operation instance)
      (error "Instance ~s is an incomplete instance" instance)))

;;; ---------------------------------------------------------------------------

(defun check-for-incomplete-instance (instance &optional operation)
  ;;; Generate an error if `instance' is an incomplete unit instance
  (when (incomplete-instance-p instance)
    (operation-on-incomplete-instance instance operation)))

(defcm check-for-incomplete-instance (instance &optional operation)
  (with-once-only-bindings (instance)
    `(when (incomplete-instance-p ,instance) 
       (operation-on-incomplete-instance ,instance ,operation))))

;;; ===========================================================================
;;;   Unit Instances
;;;
;;;   Add %%marks%% slot to standard-unit-instance internal slot names 
;;;   (added to *internal-unit-instance-slot-names* here, but defined in
;;;    unit-metaclasses.lisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew '%%marks%% *internal-unit-instance-slot-names*))

;; compile-time evaluation required for fast typep check in 
;; parse-unit-class/instance-specifier (below):
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-unit-class standard-unit-instance
      (deleted/non-deleted-unit-instance)
    ((instance-name :accessor instance-name-of)
     (%%marks%% :initform 0 :type fixnum)
     ;; %%space-instances%% slot also indicates deleted unit instances (via
     ;; :deleted value):
     (%%space-instances%% :initarg :space-instances :initform nil))
    (:abstract t)
    (:generate-accessors-format :prefix)
    (:generate-accessors t :exclude instance-name)
    (:generate-initargs t :exclude %%marks%% %%space-instances%%)))

;;; Promise that the above slots will not be unbound or their position
;;; modified:
#+cmu-possible-optimization
(declaim (pcl::slots (slot-boundp standard-unit-instance)
                     (inline standard-unit-instance)))

;;; ---------------------------------------------------------------------------

(defmethod initial-class-instance-number ((instance standard-unit-instance))
  #+ecl (declare (ignore instance))
  0)

(defmethod initial-class-instance-number ((unit-class-name symbol))
  (initial-class-instance-number 
   (class-prototype (find-unit-class unit-class-name))))

;;; ---------------------------------------------------------------------------

(defmethod next-class-instance-number ((instance standard-unit-instance))
  ;; The blackboard repository is locked whenever this is called:
  (let ((unit-class (class-of instance)))
    (if (standard-unit-class.use-global-instance-name-counter unit-class)
        ;; Using global instance-name counter?
        (incf *global-instance-name-counter*)
        ;; Otherwise, use normal per-unit-class counter:
        (incf (standard-unit-class.instance-name-counter unit-class)))))

(defmethod next-class-instance-number ((unit-class-name symbol))
  (next-class-instance-number 
   (class-prototype (find-unit-class unit-class-name))))

;;; ---------------------------------------------------------------------------

(defun add-instance-to-space-instance-paths (instance space-instance-paths)
  (flet ((add-it (path)
           (let ((space-instance (find-space-instance-by-path path)))
             (if space-instance
                 (add-instance-to-space-instance instance space-instance)
                 (error "Space instance ~s does not exist." path)))))
    (if (and (consp space-instance-paths)
             (not (consp (car space-instance-paths))))
        ;; Handle a single (non-listed) space-instance path:
        (add-it space-instance-paths)
        (mapc #'add-it space-instance-paths))))

;;; ===========================================================================
;;;  Marks 
;;;   0 - MBR (used by mark-based-retrieval) 
;;;   1 - Incomplete unit instance

;;; ---------------------------------------------------------------------------
;;;  MBR Mark (used by mark-based-retrieval)

(with-full-optimization ()
  (defun set-mbr-instance-mark (instance)
    (let ((marks (standard-unit-instance.%%marks%% instance)))
      (declare (fixnum marks))
      (setf (standard-unit-instance.%%marks%% instance)
            (logior marks #.(ash 1 0)))))
  
  (defcm set-mbr-instance-mark (instance)
    (with-once-only-bindings (instance)
      `(let ((.marks. (standard-unit-instance.%%marks%% ,instance)))
         (declare (fixnum .marks.))
         (setf (standard-unit-instance.%%marks%% ,instance)
               (logior .marks. #.(ash 1 0))))))
  
  (defun clear-mbr-instance-mark (instance)
    (let ((marks (standard-unit-instance.%%marks%% instance)))
      (declare (fixnum marks))
      (setf (standard-unit-instance.%%marks%% instance)
            (logandc2 marks #.(ash 1 0)))))
  
  (defcm clear-mbr-instance-mark (instance)
    (with-once-only-bindings (instance)
      `(let ((.marks. (standard-unit-instance.%%marks%% ,instance)))
         (declare (fixnum .marks.))
         (setf (standard-unit-instance.%%marks%% ,instance)
               (logandc2 .marks. #.(ash 1 0))))))
  
  (defun mbr-instance-mark-set-p (instance)
    ;; The obvious fixnum logbitp form `(logbitp (& ,index) (& ,flag)) is not
    ;; optimized in some CLs, so we do it ourself as follows:
    (plusp& (logand
             (& (standard-unit-instance.%%marks%% instance))
             #.(ash 1 0))))
  
  (defcm mbr-instance-mark-set-p (instance)
    `(plusp& (logand
              (& (standard-unit-instance.%%marks%% ,instance))
              #.(ash 1 0)))))

;;; ---------------------------------------------------------------------------
;;;  Incomplete unit-instance mark

(with-full-optimization ()
  (defun set-incomplete-instance-mark
      (instance 
       &optional (marks (standard-unit-instance.%%marks%% instance)))
    (declare (fixnum marks))
    (setf (standard-unit-instance.%%marks%% instance)
          (logior marks #.(ash 1 1))))
  
  (defcm set-incomplete-instance-mark (instance &optional marks)
    (if marks
        `(setf (standard-unit-instance.%%marks%% ,instance)
               (logior ,marks #.(ash 1 1)))
        (with-once-only-bindings (instance)
          `(let ((.marks. (standard-unit-instance.%%marks%% ,instance)))
             (declare (fixnum .marks.))
             (setf (standard-unit-instance.%%marks%% ,instance)
                   (logior .marks. #.(ash 1 1)))))))
  
  (defun clear-incomplete-instance-mark (instance)
    (let ((marks (standard-unit-instance.%%marks%% instance)))
      (declare (fixnum marks))
      (setf (standard-unit-instance.%%marks%% instance)
            (logandc2 marks #.(ash 1 1)))))
  
  (defcm clear-incomplete-instance-mark (instance)
    (with-once-only-bindings (instance)
      `(let ((.marks. (standard-unit-instance.%%marks%% ,instance)))
         (declare (fixnum .marks.))
         (setf (standard-unit-instance.%%marks%% ,instance)
               (logandc2 .marks. #.(ash 1 1))))))
  
  (defun incomplete-instance-mark-set-p (instance)
    ;; The obvious fixnum logbitp form `(logbitp (& ,index) (& ,flag)) is not
    ;; optimized in some CLs, so we do it ourself as follows:
    (plusp& (logand
             (& (standard-unit-instance.%%marks%% instance))
             #.(ash 1 1))))
    
  (defcm incomplete-instance-mark-set-p (instance)
    `(plusp& (logand
              (& (standard-unit-instance.%%marks%% ,instance))
              #.(ash 1 1)))))

;;; ===========================================================================
;;;   Default LINK-INSTANCE-OF reader/writer

(defmethod link-instance-of ((link deleted/non-deleted-unit-instance))
  ;; The default value for a basic link to a unit instance is just the unit
  ;; instance:
  link)

(defmethod (setf link-instance-of) 
    (nv (link deleted/non-deleted-unit-instance))
  #+ecl (declare (ignore link))
  ;; The default writer for a basic link to a unit instance is a noop:
  nv)

;;; ---------------------------------------------------------------------------

(defmethod link-instance-of ((link standard-unit-instance))
  ;; The default value for a basic link to a unit instance is just the unit
  ;; instance:
  link)

(defmethod (setf link-instance-of) (nv (link standard-unit-instance))
  #+ecl (declare (ignore link))
  ;; The default writer for a basic link to a unit instance is a noop:
  nv)

;;; ===========================================================================
;;;  Duplicating unit instances

(defmethod unduplicated-slot-names ((instance standard-unit-instance))
  #+ecl (declare (ignore instance))
  (list* '%%marks%%
         'instance-name 
         (call-next-method)))

;;; ---------------------------------------------------------------------------

(defun complete-duplicate-unit-instance (new-instance slots 
                                         space-instances space-instances-p)
  ;;; Perform the completion work for MAKE-DUPLICATE-INSTANCE and
  ;;; MAKE-DUPLICATE-INSTANCE-CHANGING-CLASS that create new unit instances:
  (post-initialize-instance-slots new-instance nil slots)
  (with-blackboard-repository-locked ()
    (maybe-initialize-instance-name-slot
     (class-of new-instance) new-instance)
    (let ((original-space-instances
           (standard-unit-instance.%%space-instances%% new-instance)))
      ;; Clear the old %%space-instances%% value:
      (setf (standard-unit-instance.%%space-instances%% new-instance)
            nil)
      (add-instance-to-space-instance-paths 
       new-instance 
       (if space-instances-p
           space-instances
           original-space-instances))))
  (values new-instance slots))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance ((instance standard-unit-instance)
                                    unduplicated-slot-names
                                    &rest initargs
                                    &key (space-instances 
                                          nil space-instances-p)
                                    &allow-other-keys)
  (multiple-value-bind (new-instance slots)
      (apply #'call-next-method instance unduplicated-slot-names initargs)
    (complete-duplicate-unit-instance 
     new-instance slots space-instances space-instances-p)))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance-changing-class ((instance standard-object)
                                                   (new-class standard-unit-class)
                                                   unduplicated-slot-names
                                                   &rest initargs
                                                   &key (space-instances 
                                                         nil space-instances-p)
                                                   &allow-other-keys)
  (multiple-value-bind (new-instance slots)
      (apply #'call-next-method instance new-class unduplicated-slot-names 
             initargs)
    (complete-duplicate-unit-instance 
     new-instance slots space-instances space-instances-p)))

;;; ---------------------------------------------------------------------------
;;; Instance-created-event signaling is done in these :around methods to follow
;;; the activities performed by the primary and :before/:after methods.

(defmethod make-duplicate-instance :around ((instance standard-unit-instance)
                                    unduplicated-slot-names
                                    &rest initargs)
  ;; Allow setf setting of link-slot pointers:
  (multiple-value-bind (new-instance slots)
      (let ((*%%allow-setf-on-link%%* 't)
            (*%%doing-initialize-instance%%* 't))
        (apply #'call-next-method instance unduplicated-slot-names initargs))
    ;; signal the creation event:
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-class 'create-instance-event))
     :instance new-instance)
    (signal-event-using-class
     (load-time-value (find-class 'instance-created-event))
     :instance new-instance)
    (values new-instance slots)))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance-changing-class :around 
           ((instance standard-object)
            (new-class standard-unit-class)
            unduplicated-slot-names
            &rest initargs)
  (multiple-value-bind (new-instance slots)
      (let ((*%%allow-setf-on-link%%* 't)
            (*%%doing-initialize-instance%%* 't))
        (apply #'call-next-method instance new-class unduplicated-slot-names
               initargs))
    ;; signal the creation event:
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-class 'create-instance-event))
     :instance instance)
    (signal-event-using-class
     (load-time-value (find-class 'instance-created-event))
     :instance instance)
    (values new-instance slots)))

;;; ===========================================================================
;;;  Saving/sending unit instances

(defmethod omitted-slots-for-saving/sending ((instance standard-unit-instance))
  #+ecl (declare (ignore instance))
  (cons '%%marks%% (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod print-slot-for-saving/sending ((instance standard-unit-instance)
                                          (slot-name (eql '%%space-instances%%))
                                          stream)
  (let ((slot-value (slot-value instance slot-name)))
    (cond 
     ;; We have some space-instances:
     ((consp slot-value)
      (write-char #\( stream)
      ;; print the first space-instance path:
      (prin1 (instance-name-of (car slot-value)) stream)
      ;; print any remaining space-instance paths:
      (dolist (space-instance (cdr slot-value))
        ;; (write-char #\space stream) <-- we don't require a <space> character here
        (prin1 (instance-name-of space-instance) stream))
      (write-char #\) stream))
     ;; Otherwise, simply print nil:
     (t (prin1 slot-value stream)))))

;;; ---------------------------------------------------------------------------

(defmethod print-object-for-saving/sending ((instance standard-unit-instance)
                                            stream)
  (cond
   ;; Unit-instance references only:
   (*save/send-references-only*
    (format stream "#GR(~s ~s)"
            (type-of instance)
            (instance-name-of instance)))
   ;; Save/send this unit instance (but only save/send references to instances
   ;; that it points to):
   (t (let ((*save/send-references-only* 't))
        (call-next-method)))))

;;; ---------------------------------------------------------------------------

(defmethod initialize-saved/sent-instance ((instance standard-unit-instance)
                                           slots slot-values missing-slot-names
                                           &aux (*%%doing-initialize-instance%%* 't))
  (declare (ignore slots slot-values missing-slot-names))
  ;; Allow setf setting of link-slot pointers. 
  (let ((*%%allow-setf-on-link%%* 't))
    (call-next-method))
  ;; remove incompleteness mark (whether or not it was set before); the
  ;; instance must be marked complete before it is added to its space
  ;; instances:
  (clear-incomplete-instance-mark instance)
  ;; Add the instance to space instances:
  (with-blackboard-repository-locked ()
    (let ((space-instance-paths
           (standard-unit-instance.%%space-instances%% instance)))
      (when space-instance-paths
        (setf (standard-unit-instance.%%space-instances%% instance) nil)
        (add-instance-to-space-instance-paths
         instance space-instance-paths))))
  ;; Note: additional fixing of direct link-slot values is done at the end of
  ;; load-blackboard-repository to update values to reflect changed link-slot
  ;; options (arity & sorting); the same is not done automatically for sent
  ;; instances.
  (unless *%%loading-complete-repository%%*    
    (let ((*%%allow-setf-on-link%%* 't))
      (reconcile-direct-link-values instance))
    ;; do the inverse pointers all link slots:
    (let ((class (class-of instance)))
      (dolist (eslotd (class-slots class))
        (when (typep eslotd 'effective-link-definition)
          (%do-ilinks 
           (effective-link-definition.direct-slot-definition eslotd)
           instance 
           (ensure-list (slot-value-using-class class instance eslotd))
           't))))
    ;; Bump the instance counter, if needed:
    (let ((unit-class (class-of instance))
          (instance-name (instance-name-of instance)))
      (when (integerp instance-name)
        (if (standard-unit-class.use-global-instance-name-counter unit-class)
            ;; Using global instance-name counter?
            (when (> instance-name *global-instance-name-counter*)
              (setf *global-instance-name-counter* instance-name))
            ;; Otherwise, using normal per-unit-class counter:
            (when (> instance-name 
                     (standard-unit-class.instance-name-counter unit-class))
              (setf (standard-unit-class.instance-name-counter unit-class)
                    instance-name))))))
  ;; signal the creation event:
  #+OLD-EVENT-NAMES
  (signal-event-using-class
   (load-time-value (find-class 'create-instance-event))
   :instance instance)
  (signal-event-using-class
   (load-time-value (find-class 'instance-created-event))
   :instance instance)
  instance)

;;; ---------------------------------------------------------------------------

(defun reconcile-direct-link-values (instance)
  ;;; Address any changes that need to be made to the direct link-slot due to
  ;;; a changes in link-slot arity or sorting (called by
  ;;; load-blackboard-repository):
  (let ((unit-class (class-of instance)))
    ;; Link slot processing: fix atomic, non-singular link-slot values and
    ;; sort if needed:
    (dolist (eslotd (class-slots unit-class))
      (when (typep eslotd 'effective-link-definition)
        (post-initialize-direct-link-slot 
         unit-class instance eslotd
         (slot-value-using-class unit-class instance eslotd))))))

;;; ---------------------------------------------------------------------------

(defmethod allocate-saved/sent-instance 
    ((class-prototype standard-unit-instance) slots slot-values)
  ;; Check that we are in a with-saving/sending-block:
  (unless (boundp '*forward-referenced-saved/sent-instances*)
    (outside-reading-saved/sent-objects-block-error 
     'allocate-saved/sent-instance))
  (with-blackboard-repository-locked ()
    (let* ((position 
            (flet ((fn (slot)
                     (eq 'instance-name (slot-definition-name slot))))
              (declare (dynamic-extent #'fn))
              (position-if #'fn slots)))
           (instance-name (nth position slot-values))
           (instance (find-instance-by-name 
                      instance-name (type-of class-prototype))))
      (cond
       ;; Instance was forward referenced, return the incomplete unit-instance
       ;; and remove it from the not-yet-defined (forward-referenced) instance
       ;; hash table:
       (instance 
        (remhash instance *forward-referenced-saved/sent-instances*)
        instance)
       ;; Instance was not forward referenced, so we allocate it and add it to
       ;; the instance hash table (in preparation for the remaining
       ;; initializations):
       (t (let* ((class (class-of class-prototype))
                 (instance (allocate-instance class)))
            (add-instance-to-instance-hash-table class instance instance-name)
            ;; Return the instance:
            instance))))))

;;; ---------------------------------------------------------------------------
;;;  Unit-instance-reference reader

(defmethod saved/sent-object-reader ((char (eql #\R)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name)
      (read stream t nil 't)
    ;; Handle old .bb files that used root-space-instance (remove eventually):
    (when (and (eq class-name 'root-space-instance)
               (eq instance-name 'root-space-instance))
      (return-from saved/sent-object-reader nil))
    (setf class-name (possibly-translate-class-name class-name))
    (or 
     ;; Instance is already present or has been forward referenced before:
     (find-instance-by-name instance-name class-name)
     ;; Otherwise, this is a forward-reference to a new unit instance; we
     ;; allocate an incomplete instance, add it to the instance-name hash
     ;; table of the unit-class, and record it as forward referenced:
     (let* ((class (find-class class-name 't))
            (instance (allocate-instance class))
            (*%%doing-initialize-instance%%* 't))
       (set-incomplete-instance-mark instance 0)
       (setf (instance-name-of instance) instance-name)
       (setf (standard-unit-instance.%%space-instances%% instance) nil)
       ;; Initialize all link slots to nil:
       (dolist (slot (class-slots class))
         (when (typep slot 'effective-link-definition)
           (let ((*%%allow-setf-on-link%%* 't))
             (setf (slot-value instance (slot-definition-name slot)) nil))))
       ;; Check that we are in a with-saving/sending-block:
       (unless (boundp '*forward-referenced-saved/sent-instances*)
         (outside-reading-saved/sent-objects-block-error
          'saved/sent-object-reader))
       ;; Record the instance as forward referenced and in the instance-name
       ;; hash table of the class:
       (setf (gethash instance *forward-referenced-saved/sent-instances*)
             't)
       ;; Update the peak count, if needed:
       (let ((count
              (hash-table-count *forward-referenced-saved/sent-instances*)))
         (when (>& count *forward-referenced-peak-count*)
           (setf *forward-referenced-peak-count* count)))
       (with-blackboard-repository-locked ()
         (add-instance-to-instance-hash-table class instance instance-name))
       instance))))
        
;;; ===========================================================================
;;;  Unit-instance utility funtions

(defun space-instances-of (instance)
  ;;; Returns the space-instances on which `instance' resides.  The result is
  ;;; *not* copied, so the user must not modify!
  (unless (incomplete-instance-p instance)
    (let ((space-instances (standard-unit-instance.%%space-instances%% instance)))
      (if (eq space-instances ':deleted)
          (operation-on-deleted-instance instance 'space-instances-of)
          space-instances))))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((instance standard-unit-instance) stream)
  (call-next-method)
  (cond ((instance-deleted-p instance) (format stream " [Deleted]"))
        ((incomplete-instance-p instance) (format stream " [Incomplete]")))
  (print-instance-slot-value instance 'instance-name stream))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :before ((instance standard-unit-instance)
                                      slot-names
                                      &key space-instances)  
  ;; To support reinitialize-instance and friends, remove instance from any
  ;; space-instances that aren't retained in the specified space-instances
  ;; value:
  (when (and (not *%%skip-gbbopen-shared-initialize-method-processing%%*)
             (slot-boundp instance '%%space-instances%%)
             (or (eq slot-names 't)
                 (memq '%%space-instances%% slot-names)))    
    (dolist (space-instance
                (standard-unit-instance.%%space-instances%% instance))
      (unless (memq space-instance space-instances)
        (remove-instance-from-space-instance instance space-instance)))
    ;; Remember any remaining space instances for use by the :after method:
    (locally
        (declare (special *%%existing-space-instances%%*))
      (setf *%%existing-space-instances%%* 
        (standard-unit-instance.%%space-instances%% instance)))))

;;; ---------------------------------------------------------------------------

(defun post-initialize-direct-link-slot (unit-class instance eslotd 
                                         current-value)
  ;;; Fix the direct slot's current value to match the link slot definition
  ;;; and return the dslotd of the link slot
  (when current-value
    (let* ((dslotd
            (effective-link-definition.direct-slot-definition eslotd))
           (sort-function 
            (direct-link-definition.sort-function dslotd))
           (sort-key
            (or (direct-link-definition.sort-key dslotd)
                #'identity))) 
      (cond
       ;; Singular link slot:
       ((direct-link-definition.singular dslotd)
        ;; fix-up non-atomic singular link slot initialization
        ;; values here; pretty late, but OK for now:
        (when (consp current-value)
          (with-events-disabled ()
            (setf (slot-value-using-class
                   unit-class instance
                   #-lispworks
                   eslotd
                   #+lispworks
                   (slot-definition-name eslotd))
                  (sole-element current-value)))))
       ;; Non-singular link slot:
       (t (let ((rewrite-slot nil))
            ;; fix-up atomic non-singular link slot initialization
            ;; values here; pretty late, but OK for now:
            (unless (consp current-value)
              (setf current-value (list current-value)
                    rewrite-slot 't))
            ;; Make sure the direct link-slot value is sorted, if so
            ;; specified:
            (when (and sort-function
                       ;; length > 1
                       (cdr current-value))
              (setf current-value (sort (copy-list current-value)
                                        sort-function
                                        :key sort-key)
                    rewrite-slot 't))
            (when rewrite-slot
              (with-events-disabled ()
                (setf (slot-value-using-class 
                       unit-class instance 
                       #-lispworks
                       eslotd
                       #+lispworks
                       (slot-definition-name eslotd))
                      current-value))))))
      ;; Return the (possibly changed) current value and the dslotd:
      (values current-value dslotd))))

;;; ---------------------------------------------------------------------------

(defun post-initialize-instance-slots (instance slot-names slots)
  ;; Performs post-initialization slot processing. At least one of
  ;; `slot-names' or `slots' should be nil (dealing with the difference
  ;; between having the slot-names in INITIALIZE-INSTANCE or the actual slots
  ;; in MAKE-DUPLICATE-INSTANCE and MAKE-DUPLICATE-INSTANCE-CHANGING-CLASS).
  (let ((unit-class (class-of instance)))
    ;; Link slot processing: fix atomic, non-singular link-slot values and
    ;; create link inverse pointers:
    (dolist (eslotd (class-slots unit-class))
      ;; Only process specified slots:
      (when (or (eq slot-names 't)
                (when slot-names 
                  (memq (slot-definition-name eslotd) slot-names))
                (when slots (memq eslotd slots)))
        (cond 
         ;; link slot:
         ((typep eslotd 'effective-link-definition)
          (let ((current-value 
                 (slot-value-using-class unit-class instance eslotd)))
            (when current-value
              (multiple-value-bind (current-value dslotd)
                  (post-initialize-direct-link-slot 
                   unit-class instance eslotd current-value)
                ;; do the inverse pointers for this link slot:
                (%do-ilinks dslotd instance (ensure-list current-value) 't)
                ;; signal the direct link event:
                (%signal-direct-link-event 
                 instance dslotd current-value current-value)))))
         ;; nonlink slot:
         #+(or digitool-mcl lispworks)
         ;; In implementations that don't use (setf slot-value-using-class)
         ;; to initialize slots, we must signal the nonlink-slot update
         ;; event ourselves:
         (t (let ((slot-name/def
                   #+lispworks
                   (slot-definition-name eslotd)
                   #+digitool-mcl
                   eslotd))
              (when (and (typep eslotd 'gbbopen-effective-slot-definition)
                         (slot-boundp-using-class unit-class instance 
                                                  slot-name/def))
                ;; signal the update-nonlink-slot event:
                #+OLD-EVENT-NAMES
                (signal-event-using-class
                 (load-time-value (find-class 'update-nonlink-slot-event))
                 :instance instance
                 :slot eslotd
                 :current-value (slot-value-using-class 
                                 unit-class instance 
                                 slot-name/def)
                 :initialization 't)
                (signal-event-using-class
                 (load-time-value (find-class 'nonlink-slot-updated-event))
                 :instance instance
                 :slot eslotd
                 :current-value (slot-value-using-class 
                                 unit-class instance 
                                 slot-name/def)
                 :initialization 't)))))))))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((instance standard-unit-instance)
                                     slot-names 
                                     &key space-instances)  
  (declare (inline class-of))  
  (unless *%%skip-gbbopen-shared-initialize-method-processing%%*
    (post-initialize-instance-slots instance slot-names nil)
    ;; Add this instance to the explicitly-specified space instances.  This
    ;; is ugly, but we first remove the supplied space instances stored in
    ;; the %%space-instances%% slot, and then we re-add them either directly
    ;; or via add-instance-to-space-instance based on the state kept in
    ;; *%%existing-space-instances%%* (set in the shared-initialize :before
    ;; method).
    (when (and (or (eq slot-names 't)
                   (memq '%%space-instances%% slot-names))
               space-instances)
      (setf (standard-unit-instance.%%space-instances%% instance) nil)
      (dolist (space-instance space-instances)
        (if (memq space-instance 
                  (locally (declare (special *%%existing-space-instances%%*))
                    *%%existing-space-instances%%*))
            (push space-instance
                  (standard-unit-instance.%%space-instances%% instance))
            (add-instance-to-space-instance instance space-instance))))))

;;; ---------------------------------------------------------------------------
;;; This :around method permits shared-initialize to set link slots without
;;; triggering errors by binding *%%allow-setf-on-link%%* and the 
;;; *%%existing-space-instances%%* placeholder.

(defmethod shared-initialize :around ((instance standard-unit-instance)
                                      slot-names &key)
  #+ecl (declare (ignore instance))
  (declare (ignore slot-names))
  (cond 
   (*%%skip-gbbopen-shared-initialize-method-processing%%*
    (let (;; allow initialization of link slots
            (*%%allow-setf-on-link%%* t))
      (call-next-method)))
   (t (let (;; allow initialization of link slots
            (*%%allow-setf-on-link%%* t)
            ;; Used to maintain any existing space-instance state between
            ;; shared-initialize :before and :after methods:
            (*%%existing-space-instances%%* nil))
        (declare (special *%%existing-space-instances%%*))
        (with-blackboard-repository-locked ()
          (call-next-method))))))

;;; ---------------------------------------------------------------------------

(defun add-instance-to-initial-space-instances (instance unit-class)
  ;; Add the unit instance to initial space instances
  (let ((initial-space-instances
	 (standard-unit-class.effective-initial-space-instances 
	  unit-class)))
    (cond 
     ;; computed initial-space instances:
     ((functionp initial-space-instances)
      (dolist (space-instance 
		  (ensure-list
		   (funcall initial-space-instances instance)))
	(add-instance-to-space-instance instance space-instance)))
     ;; constant initial-space instances:
     (t (add-instance-to-space-instance-paths 
         instance initial-space-instances)))))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :before ((instance standard-unit-instance) 
                                        &key)
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (when (standard-unit-class.abstract unit-class)      
      (error "Unit class ~s is abstract and cannot be instantiated."
             (class-name unit-class)))))

;;; ---------------------------------------------------------------------------

(defun maybe-initialize-instance-name-slot (unit-class instance)
  ;; If the instance-name slot is unbound in instance, generate it; then add
  ;; instance to the instance-name hash table:
  (let* ((slotd (find-effective-slot-definition-by-name
                 unit-class 'instance-name))
         (instance-name 
          (cond 
           ;; We already have an instance-name:
           ((slot-boundp-using-class unit-class instance slotd)
            (slot-value-using-class unit-class instance slotd))
           ;; Otherwise, use normal per-unit-class counter:
           (t
            ;; Initialize class counter, if needed (even if the class is using
            ;; the global counter):
            (when (eq (standard-unit-class.instance-name-counter unit-class)
                      unbound-value-indicator)
              (setf (standard-unit-class.instance-name-counter unit-class)
                    (initial-class-instance-number
                     (class-prototype unit-class))))
            ;; Increment and use the counter value:
            (setf (slot-value-using-class 
                   unit-class instance 
                   #-lispworks
                   slotd
                   #+lispworks
                   (slot-definition-name slotd))
                  ;; initialize the instance-name counter, if needed:
                  (next-class-instance-number instance))))))
    (add-instance-to-instance-hash-table unit-class instance instance-name)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((instance standard-unit-instance) 
                                       &key (space-instances 
                                             nil space-instances-p))
  (declare (ignore space-instances))
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (maybe-initialize-instance-name-slot unit-class instance)
    ;; add the unit instance to initial space instances, unless
    ;; :initial-space-instances was overridden by an explicit :space-instances
    ;; value:
    (unless space-instances-p
      (add-instance-to-initial-space-instances instance unit-class))))

;;; ---------------------------------------------------------------------------
;;; Create-instance-event signaling is done in this :around method to follow
;;; activities performed by primary and :before/:after methods.

(defmethod initialize-instance :around ((instance standard-unit-instance) 
                                        &key)
  (let ((*%%doing-initialize-instance%%* 't))
    (with-blackboard-repository-locked ()
      (call-next-method)))
  ;; signal the creation event:
  #+OLD-EVENT-NAMES
  (signal-event-using-class
   (load-time-value (find-class 'create-instance-event))
   :instance instance)
  (signal-event-using-class
   (load-time-value (find-class 'instance-created-event))
   :instance instance)
  instance)

;;; ---------------------------------------------------------------------------

(defun add-instance-to-instance-hash-table (unit-class instance instance-name)
  ;;; Note: the blackboard repository is expected to be held whenever
  ;;; this function is called.
  (let* ((instance-hash-table 
          (standard-unit-class.instance-hash-table unit-class))
         (existing-instance (gethash instance-name instance-hash-table)))
    (unless (eq instance existing-instance)
      (when existing-instance
	(cerror "Delete the existing instance and replace it with the new ~
               instance."
		"An instance of ~s named ~s already exists."
		(type-of existing-instance)
		(instance-name-of existing-instance))
	(delete-instance existing-instance))
      (setf (gethash instance-name instance-hash-table) instance))))

;;; ---------------------------------------------------------------------------

(defun remove-instance-from-instance-hash-table (unit-class instance-name)
  (remhash instance-name (standard-unit-class.instance-hash-table unit-class)))

;;; ---------------------------------------------------------------------------

(defun rename-instance-in-instance-hash-table (unit-class instance 
                                               old-name new-name)
  (let ((test (hash-table-test 
               (standard-unit-class.instance-hash-table unit-class))))
    (unless (funcall test old-name new-name)
      (with-blackboard-repository-locked ()
        ;; Add new-name first, in case there is a conflict:
        (add-instance-to-instance-hash-table unit-class instance new-name)
        ;; If OK or continued, remove the old-name from instance-hash-table
        ;; (we must check that the old-name does reference instance, as the
        ;; change-class procedure can have a different instance stored under
        ;; the old name and that must be retained):
        (when (eq instance 
                  (gethash old-name (standard-unit-class.instance-hash-table
                                     unit-class)))
          (remove-instance-from-instance-hash-table unit-class old-name))))))

;;; ===========================================================================
;;;  Describe instance

(defmethod hidden-nonlink-slot-names ((instance standard-unit-instance))
  ;; Returns a list of nonlink-slot-names that should not be shown
  ;; by describe-instance or describe-unit-class:
  #+ecl (declare (ignore instance))
  '(%%marks%% %%space-instances%%))

;;; ---------------------------------------------------------------------------

(defmethod describe-instance-slot-value ((instance standard-unit-instance)
                                         slot-name value
                                         &optional (stream *standard-output*))
  #+ecl (declare (ignore instance))
  (declare (ignore slot-name))
  (prin1 value stream))

;;; ---------------------------------------------------------------------------

(defmethod describe-instance ((instance standard-unit-instance) 
                               &optional (stream *standard-output*))
  (let ((class (class-of instance)))
    (format stream "~&~@(~s~) ~s~%" (class-name class) instance)
    (let ((non-link-slots nil)
          (link-slots nil))
      (dolist (eslotd (class-slots class))
        (cond 
         ;; link slot:
         ((typep eslotd 'effective-link-definition)
          (push eslotd link-slots))
         ;; non-link slot:
         (t (push eslotd non-link-slots))))
      (flet ((do-slot (eslotd)
               (let ((boundp (slot-boundp-using-class class instance eslotd))
                     (slot-name (slot-definition-name eslotd)))
                 (format stream "~&~4t~s:  " slot-name)
                 (if boundp
                     (describe-instance-slot-value
                      instance
                      slot-name                            
                      (slot-value-using-class class instance eslotd)
                      stream)
                     (princ "<unbound>" stream))
                 (terpri stream))))
        (format stream "~2tInstance name: ~s~%" (instance-name-of instance))
        (let* ((space-instances (standard-unit-instance.%%space-instances%% instance))
               (space-instance-names 
                (when (consp space-instances)
                  (mapcar #'instance-name-of space-instances))))
          (format stream "~2tSpace instances: ~:[None~;~:*~s~]~%"
                  space-instance-names))
        (format stream "~2tDimensional values:")
        (let ((dimension-specs 
               (sort (copy-list (dimensions-of (class-of instance)))
                     #'string< :key #'first)))
          (if dimension-specs
              (dolist (dimension-spec dimension-specs)
                (let* ((dimension-name (first dimension-spec))
                       (dimension-value
                        (instance-dimension-value instance dimension-name))) 
                  (format stream "~&~4t~s:  ~:[~s~;<unbound>~]~%"
                          dimension-name
                          (eq dimension-value unbound-value-indicator)
                          dimension-value)))
              (format stream " None~%")))
        (format stream "~2tNon-link slots:")
        (let ((slot-printed nil))
          (dolist (eslotd (sort non-link-slots #'string< 
                                :key #'slot-definition-name))
            (let ((slot-name (slot-definition-name eslotd)))
              (unless (or 
                       ;; not really hidden, but we treat this slot
                       ;; specially in describe:
                       (eq slot-name 'instance-name)
                       (memq slot-name (hidden-nonlink-slot-names instance)))
                (setf slot-printed t)
                (do-slot eslotd))))
          (unless slot-printed 
            (format stream " None~%")))
        (format stream "~2tLink slots:")
        (if link-slots
            (dolist (eslotd (sort link-slots #'string<
                                  :key #'slot-definition-name))
              (do-slot eslotd))
            (format stream " None~%")))))
  (values))

;;; ===========================================================================
;;;  Delete instace

(defmethod deleted-instance-class ((instance standard-unit-instance))
  #+ecl (declare (ignore instance))
  (load-time-value (find-class 'deleted-unit-instance)))

;;; ---------------------------------------------------------------------------

(defun change-deleted-instance-class (instance unit-class)
  (let ((deleted-instance-class (deleted-instance-class instance)))
    (change-class instance deleted-instance-class 
                  :original-class unit-class)
    (when (typep instance 'standard-unit-instance)
      (error "The deleted-instance-class ~s is a subclass of ~s"
             (class-name deleted-instance-class)
             'standard-unit-instance))))

;;; ---------------------------------------------------------------------------

(defmethod delete-instance ((instance standard-unit-instance))
  (declare (inline class-of))
  (let ((space-instances (standard-unit-instance.%%space-instances%% instance))
        (unit-class (class-of instance)))
    (cond 
     ;; Really want speed over a safety net? Really?
     (*skip-deleted-unit-instance-class-change*
      ;; remove from instance-hash-table:
      (remove-instance-from-instance-hash-table 
       unit-class (instance-name-of instance))
      ;; remove from all space instances:
      (when (consp space-instances)
        (dolist (space-instance space-instances)
          (remove-instance-from-space-instance instance space-instance)))
      ;; unlink all link slots:
      (delete-all-incoming-link-pointers instance)
      ;; Mark the (unchanged class) instance as deleted:
      (setf (standard-unit-instance.%%space-instances%% instance) ':deleted))
     ;; Although the class change is a bit expensive, the extra safety in
     ;; detecting stale references to deleted unit-instances is worth it...
     (t (change-deleted-instance-class instance unit-class))))
  instance)

;;; ---------------------------------------------------------------------------

(defmethod delete-instance :around ((instance standard-unit-instance))
  ;;; Deletion-event signaling is done in this :around method to surround 
  ;;; activities performed by primary and :before/:after methods
  (unless (instance-deleted-p instance) ; deleting a deleted instance is a noop
    (with-blackboard-repository-locked ()
      ;; signal the delete-instance event:
      (signal-event-using-class
       (load-time-value (find-class 'delete-instance-event))
       :instance instance)
      ;; delete the instance:
      (call-next-method)
      ;; signal the instance-deleted event:
      (signal-event-using-class
       (load-time-value (find-class 'instance-deleted-event))
       :instance instance)))
  ;; Return the instance:
  instance)

;;; ---------------------------------------------------------------------------

(defmethod delete-instance :around ((instance deleted-unit-instance)) 
  ;; Deleting a deleted instance is a noop, so just return the deleted
  ;; instance:
  instance)
 
;;; ---------------------------------------------------------------------------
;;;   Extended-unit-type-p 

(defun extended-unit-type-p (object unit-class-name)
  ;;; Returns true if `object' is "extended-type-p" of `unit-class-name'
  (with-full-optimization ()
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq unit-class-name 't) (typep object 'standard-unit-instance))
     ;; extended unit-class specification:
     ((consp unit-class-name)
      (destructuring-bind (unit-class-name subclass-indicator)
          unit-class-name
        (ecase subclass-indicator
          ((:plus-subclasses +)
           (locally 
             ;; Avoid compiler warnings (in CMUCL, SBCL, and SCL) due to
             ;; inability to generate inlined TYPEP at compile time:
             #+(or cmu sbcl scl) (declare (notinline typep))
             (typep object unit-class-name)))
          ((:no-subclasses =)
           (eq (type-of object) unit-class-name)))))
     ;; anything else we assume is a unit-class name:
     (t (eq (type-of object) unit-class-name)))))
  
;;; ===========================================================================
;;;   Instance renaming
;;;
;;; This doesn't work on Lisps that optimize defclass slot-writer methods
;;; rather than calling the (setf slot-value-using-class) method.  With such
;;; Lisps, we would have to attach to all the slot writer methods.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to control
;;; the use of slot reader/writer methods (so GBBopen must set this option to
;;; nil for unit classes--done in define-unit-class).

#-cmu
(defmethod (setf slot-value-using-class) :before
           (nv
            (class standard-unit-class)
            instance
            ;; instead of the effective-slot-definition, Lispworks
            ;; provides the slot name:
            (slot #+lispworks
                  (eql 'instance-name)
                  #-lispworks
                  effective-nonlink-slot-definition))
  (when #+lispworks 't
        #-lispworks (eq (slot-definition-name slot) 'instance-name)        
     (when (slot-boundp-using-class 
            class instance
            #+lispworks 'instance-name
            #-lispworks slot)
       (let ((ov (slot-value-using-class 
                  class instance 
                  #+lispworks 'instance-name
                  #-lispworks slot)))
         (rename-instance-in-instance-hash-table class instance ov nv)))))

;;; ===========================================================================
;;;   Update-nonlink-slot event signaling
;;;
;;; This doesn't work on Lisps that optimize defclass slot-writer methods
;;; rather than calling the (setf slot-value-using-class) method.  With such
;;; Lisps, we would have to attach to all the slot writer methods.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to control
;;; the use of slot reader/writer methods (so GBBopen must set this option to
;;; nil for unit classes--done in define-unit-class).

#-cmu
(defmethod (setf slot-value-using-class) :after
           (nv
            (class standard-unit-class)
            instance
            ;; instead of the effective-slot-definition, Lispworks
            ;; provides the slot name:
            (slot #+lispworks symbol
                  #-lispworks effective-nonlink-slot-definition))
  #+ecl (declare (ignore class))
  ;; must look up the slot object in Lispworks:
  #+lispworks
  (setf slot (car (member slot (class-slots class)
                          :test #'eq
                          :key 'slot-definition-name)))
  (when #-lispworks 't
        ;; only non-link slots!
        #+lispworks (typep slot 'effective-nonlink-slot-definition)    
      #+OLD-CLASS-NAMES    
      (signal-event-using-class 
       (load-time-value (find-class 'update-nonlink-slot-event))
       :instance instance
       :slot slot
       :current-value nv
       :initialization *%%doing-initialize-instance%%*)
      (signal-event-using-class 
       (load-time-value (find-class 'nonlink-slot-updated-event))
       :instance instance
       :slot slot
       :current-value nv
       :initialization *%%doing-initialize-instance%%*)))

;;; ---------------------------------------------------------------------------
;;;  CMUCL can't handle the above :before/:after combination, so we must use a
;;;  combined :around method until it is fixed (remains broken in 19e)

#+cmu
(defmethod (setf slot-value-using-class) :around
           (nv
            (class standard-unit-class)
            instance
            (slot effective-nonlink-slot-definition))
  (prog1
      (cond 
       ((and (eq (slot-definition-name slot) 'instance-name)
             (slot-boundp-using-class class instance slot))
        (let ((ov (slot-value-using-class class instance slot)))
          (rename-instance-in-instance-hash-table class instance ov nv)
          (call-next-method)))
       (t (call-next-method)))
    ;; signal the update event:
    #+OLD-CLASS-NAMES
    (signal-event-using-class 
     (load-time-value (find-class 'update-nonlink-slot-event))
     :instance instance
     :slot slot
     :current-value nv
     :initialization *%%doing-initialize-instance%%*)
    (signal-event-using-class 
     (load-time-value (find-class 'nonlink-slot-updated-event))
     :instance instance
     :slot slot
     :current-value nv
     :initialization *%%doing-initialize-instance%%*)))

;;; ===========================================================================
;;;   Find instance by name

(defun find-instance-by-name (instance-name &optional (unit-class-name 't)
                                                      errorp)
  ;;; Retrieves a unit-instance by its name.
  (flet ((find-it (unit-class)
           (let ((instance-hash-table
                  (standard-unit-class.instance-hash-table unit-class)))
             (gethash instance-name instance-hash-table))))
    (flet ((fn (unit-class plus-subclasses)
             (declare (ignore plus-subclasses))
             (let ((result (find-it unit-class)))
               (when result (return-from find-instance-by-name result)))))
      (declare (dynamic-extent #'fn))
      (map-extended-unit-classes #'fn unit-class-name)))
  (when errorp 
    (error "No instance named ~s of class ~s was found."
           instance-name
           unit-class-name)))

;;; ---------------------------------------------------------------------------

(defun find-all-instances-by-name (instance-name &optional (unit-class-name 't))
  ;;; Returns all unit-instances with `instance-name'.
  (let ((instances nil))
    (flet ((find-it (unit-class)
             (let ((instance-hash-table
                    (standard-unit-class.instance-hash-table unit-class)))
               (gethash instance-name instance-hash-table))))
      (flet ((fn (unit-class plus-subclasses)
               (declare (ignore plus-subclasses))
               (let ((result (find-it unit-class)))
                 (when result (push result instances)))))
        (declare (dynamic-extent #'fn))
        (map-extended-unit-classes #'fn unit-class-name)))
    instances))

;;; ===========================================================================
;;;   Change-class support
;;;
;;; There is a lot of change-class method complexity below to support 
;;; all unit-class combinations of change-class:
;;;    1. unit class to unit class
;;;    2. unit class to non-unit class
;;;    3. non-unit class to unit class
;;; as well as to have the change events signalled fully before and after the
;;; change.
;;;

;; Holds the prior space-instances of a unit-instance during a change-class
;; operation:
(defvar *%changing-class-space-instances%* nil)

;;; ---------------------------------------------------------------------------

(defmethod change-class :before ((instance standard-unit-instance)
				 (new-class class) 
                                 &key
                                 &allow-other-keys)
  ;;l This :before method (with typep tests below) handles class changes from a
  ;;; unit class to a unit class or from a unit class to a non-unit class.
  (declare (inline class-of))
  ;; We must ensure finalization, as the changed instance could be the first
  ;; instance of `new-class':
  (ensure-finalized-class new-class)
  (let ((unit-class (class-of instance))
        (non-unit-new-class (not (typep new-class 'standard-unit-class)))
        (new-class-slots (class-slots new-class)))
    ;; Remove instance from its current class-instance hash table:
    (remove-instance-from-instance-hash-table 
     unit-class (instance-name-of instance))
    ;; Also, remove instance from all space instances before changing its
    ;; class:
    (let ((space-instances (standard-unit-instance.%%space-instances%% instance)))
      (when (consp space-instances)
        (dolist (space-instance space-instances)
          (remove-instance-from-space-instance instance space-instance))))
    ;; Unlink instances in any link slots that aren't link slots in the new
    ;; class:
    (dolist (slot (class-slots unit-class))
      (when (typep slot 'effective-link-definition)
	(when (or non-unit-new-class
                  (let ((new-class-slot 
                         ;; (car (member ...)) with :test & :key often
                         ;; optimizes better than (find ...):
                         (car (member (slot-definition-name slot) 
                                      new-class-slots 
                                      :test #'eq
                                      :key #'slot-definition-name))))
                    (not (typep new-class-slot 'effective-link-definition))))
          (delete-incoming-link-pointer instance slot))))))

;;; ---------------------------------------------------------------------------

(defun add-changed-class-instance-to-space-instances (instance new-class
                                                      space-instances
                                                      space-instances-p)
  ;; Add changed-class instance to appropriate space instances:
  (cond 
   (space-instances-p
    ;; This is ugly, but if :space-instances have been specified, we first
    ;; remove the supplied space instances stored in the %%space-instances%%
    ;; slot, and then we re-add them using add-instance-to-space-instance to
    ;; do the storage and event signaling.
    (setf (standard-unit-instance.%%space-instances%% instance) nil)
    (dolist (space-instance 
                ;; Maintain the order:
                (reverse space-instances))
      (add-instance-to-space-instance instance space-instance)))
   ;; No :space-instances have been specified, so add `instance' to the
   ;; initial space instances:
   (t (let ((*warn-about-unusual-requests* nil)) ;; Don't complain if the
                                                 ;; instance is already on the
                                                 ;; space instance
      (add-instance-to-initial-space-instances instance new-class)))))


;;; ---------------------------------------------------------------------------

(defmethod change-class :after ((instance standard-object)
                                (new-class standard-unit-class) 
                                &key (space-instances 
				      nil space-instances-p)
                                &allow-other-keys)
  (typecase instance 
    ;; ------------------------------------------------------------------------
    ;; This portion of the :after method handles link processing and adding the
    ;; instance to space instances on class changes from a unit class to a unit
    ;; class
    (standard-unit-instance
     (let ((instance-name 
            (if (slot-boundp instance 'instance-name)
                (instance-name-of instance)
                (setf (instance-name-of instance)
                      (next-class-instance-number instance)))))
       (add-instance-to-instance-hash-table 
        new-class instance instance-name))
     ;; Add inverse pointers from instances pointed to by any link slots that
     ;; aren't present already:
     (dolist (slot (class-slots new-class))
       (when (typep slot 'effective-link-definition)
         (%do-ilinks (effective-link-definition.direct-slot-definition slot)
                     instance 
                     (ensure-list (slot-value-using-class new-class instance slot))
                     nil)))
     ;; Add the changed-class instance to its space instances:
     (add-changed-class-instance-to-space-instances 
      instance new-class 
      ;; Use space-instances, if specified, or the remembered ones:
      (or space-instances *%changing-class-space-instances%*)
      (or space-instances-p *%changing-class-space-instances%*)))
    ;; ------------------------------------------------------------------------
    ;; This portion of the :after method handles class changes from a non-unit
    ;; class to a unit class (so we must grab the master-instance lock here)
    (otherwise 
     (with-blackboard-repository-locked ()
       (let ((instance-name 
              (if (slot-boundp instance 'instance-name)
                  (instance-name-of instance)
                  (setf (instance-name-of instance)
                        (next-class-instance-number instance)))))
         (add-instance-to-instance-hash-table 
          new-class instance instance-name))
       ;; Add instance to appropriate space instances:
       (add-changed-class-instance-to-space-instances 
        instance new-class space-instances space-instances-p)))))

;;; ---------------------------------------------------------------------------
;;; Event signaling is done in this :around method to surround activities
;;; performed by primary and :before/:after methods

(defmethod change-class :around ((instance standard-unit-instance)
				 (new-class class) 
                                 &key)
  (declare (inline class-of))
  ;;; This :around method handles adding the instance to space instances on
  ;;; class changes from a unit class to a unit class or from a unit class to
  ;;; a non-unit class.
  (let ((previous-class (class-of instance)))
    (signal-event-using-class
     (load-time-value (find-class 'change-instance-class-event))
     :instance instance
     :new-class new-class)
    (with-blackboard-repository-locked ()
      ;; Remember the current space-instances of instance (for re-addition
      ;; later in the protocol):
      (let ((*%changing-class-space-instances%* (space-instances-of instance)))
        (call-next-method)))
    ;; Only signal the instance-changed event if new-class is a unit class:
    (when (typep new-class 'standard-unit-class)
      (signal-event-using-class
       (load-time-value (find-class 'instance-changed-class-event))
       :instance instance
       :previous-class previous-class)))
  instance)

;;; ---------------------------------------------------------------------------

(defmethod change-class :around ((instance standard-object)
				 (new-class standard-unit-class) 
                                 &key)
  #+ecl (declare (ignore new-class))
  ;;; This :around method handles event-signalling on class changes from a
  ;;; non-unit class to a unit class (or simply calls next method on changes
  ;;; from a unit class to another class):
  (cond 
   ;; Previous class was a unit class:
   ((typep instance 'standard-unit-instance)
    (let (;; CLISP's and ECL's internal CHANGE-CLASS implementation uses SETF to set
          ;; slots, so we must allow link-slot setting here:
          #+(or clisp ecl) (*%%allow-setf-on-link%%* 't))
      (call-next-method)))
   ;; Changing from a non-unit class to a unit class:
   (t (let ((previous-class (class-of instance)))
        (let (;; CLISP's and ECL's internal CHANGE-CLASS implementation uses
              ;; SETF to set slots, so we must allow link-slot setting here:
              #+(or clisp ecl) (*%%allow-setf-on-link%%* 't))
          (call-next-method))
        (signal-event-using-class
         (load-time-value (find-class 'instance-changed-class-event))
         :instance instance
         :previous-class previous-class))))
  instance)

;;; ===========================================================================
;;;   Class-instance mappers (and do-xxx versions)

(with-full-optimization ()
  (defun map-instances-given-class (fn unit-class)
    ;;; Internal instance mapping for a specific `unit-class' object.
    (declare (type function fn))
    (flet ((fn (key value)
             (declare (ignore key))
             (funcall fn value)))
      (declare (dynamic-extent #'fn))
      (maphash #'fn (standard-unit-class.instance-hash-table unit-class)))))

;;; ---------------------------------------------------------------------------

(defun map-instances-of-class (fn unit-class-name)
  ;;; This is the public interface to class-instance mapping.
  (let ((fn (coerce fn 'function)))
    (flet ((do-fn (unit-class plus-subclasses)
             (declare (ignore plus-subclasses))
             (map-instances-given-class (the function fn) unit-class)))
      (declare (dynamic-extent #'do-fn))
      (map-extended-unit-classes #'do-fn unit-class-name))))

;;; ---------------------------------------------------------------------------

(defmacro do-instances-of-class ((var unit-class-name) &body body)
  ;;; Do-xxx variant of map-instances-of-class.
  `(block nil
     (flet ((.fn. (,var) ,@body))
       (declare (dynamic-extent #'.fn.))
       (map-instances-of-class #'.fn. ,unit-class-name))))

;;; ---------------------------------------------------------------------------

(defun find-instances-of-class (unit-class-name)
  (let ((instances nil))
    (flet ((fn (instance) (push instance instances)))
      (declare (dynamic-extent #'fn))
      (map-instances-of-class #'fn unit-class-name))
    instances))

;;; ---------------------------------------------------------------------------

(defun make-instances-of-class-vector (unit-class-name &key adjustable)
  ;; Returns a newly allocated vector (adjustable, if desired) with pointers
  ;; to all unit instances of unit-class-name.  Note that, once created, this
  ;; vector is not maintained with newly created/deleted unit instances.
  (let* ((size (class-instances-count unit-class-name))
         (vector (make-array  (list size) 
                              :fill-pointer 0 
                              :adjustable adjustable)))
    (do-instances-of-class (instance unit-class-name)
      (vector-push instance vector))
    vector))

;;; ---------------------------------------------------------------------------

(defun map-sorted-instances-of-class (fn unit-class-name predicate &key key)
  ;;; This function is convenient for occasional presentation purposes;
  ;;; it creates and sorts a vector of all instances, so it is expensive!
  (let ((vector (make-instances-of-class-vector unit-class-name))
        (fn (coerce fn 'function))
        (predicate (coerce predicate 'function)))
    (map nil (the function fn) 
         (if key 
             (sort vector predicate :key key)
             (sort vector predicate)))))

;;; ---------------------------------------------------------------------------

(defmacro do-sorted-instances-of-class ((var unit-class-name 
                                         predicate &key key) &body body)
  ;;; Do-xxx variant of map-sorted-instances-of-class.
  `(block nil
     (flet ((.fn. (,var) ,@body))
       (declare (dynamic-extent #'.fn.))
       (map-sorted-instances-of-class
        #'.fn.
        ,unit-class-name 
        ,predicate 
        ,@(when key `(:key ,key))))))

;;; ===========================================================================
;;;  Dimension-value access

(with-full-optimization ()
  (defun internal-instance-dimension-value (instance dimension-name
                                            &optional into-cons)
    ;;; Returns five values: the dimension value, the dimension-value type,
    ;;;                      the comparison-type, the composite type, and the
    ;;;                      ordering-dimension-name (for a series composite)
    ;;; Called for top performance, but this internal version does not check
    ;;; for deleted unit instances.
    (declare (inline class-of))
    (let* ((unit-class (class-of instance))
           (cdv
            ;; (car (member ...)) with :test & :key often optimizes better
            ;; than (find ...):
            (car (member
                  dimension-name
                  (standard-unit-class.effective-dimensional-values 
                   unit-class)
                  :test #'eq
                  :key #'cdv.dimension-name))))
      (unless cdv
        (error "~s is not a dimension of ~s." dimension-name instance))
      (values 
       ;; get the dimension value:
       (funcall (the function (cdv.value-fn cdv)) instance into-cons)
       (cdv.dimension-value-type cdv)
       (cdv.comparison-type cdv)
       (cdv.composite-type cdv)
       (cdv.ordering-dimension-name cdv)))))

;;; ---------------------------------------------------------------------------

(defun instance-dimension-value (instance dimension-name)
  ;;; The public function.
  ;;; Returns five values: the dimension value, the dimension-value type,
  ;;;                      the comparison-type, the composite type, and the 
  ;;;                      ordering-dimension-name (for a series composite)
  #+check-for-deleted-instances
  (check-for-deleted-instance instance 'instance-dimension-value)
  #+check-for-incomplete-instances
  (check-for-incomplete-instance instance 'instance-dimension-value)
  (internal-instance-dimension-value instance dimension-name))

;;; ---------------------------------------------------------------------------

(defun instance-dimension-values (instance &optional (dimension-names 't))
  ;;; Returns an alist of the current dimension values (specified by 
  ;;; `dimension-names'); if a single dimension-name symbol is provided
  ;;; only the appropriate acons is returned.
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (flet ((make-dimension-value-acons (dimension-name)
             (cons dimension-name
                   (instance-dimension-value instance dimension-name))))
      (if (symbolp dimension-names)
          (if (eq dimension-names 't)
              (flet ((fn (cdv)
                       (make-dimension-value-acons (cdv.dimension-name cdv))))
                (declare (dynamic-extent #'fn))
                (mapcar #'fn
                        (standard-unit-class.effective-dimensional-values 
                         unit-class)))
              (make-dimension-value-acons dimension-names))
          (mapcar #'make-dimension-value-acons dimension-names)))))

;;; ===========================================================================
;;;  With-changing-dimension-values

(defun update-instance-locators (instance old-dimension-values 
                                 dimensions-being-changed verbose)
  ;;; Updates the locators for `instance' for all dimensions in
  ;;; `dimensions-being-changed' on the %%space-instances%% of `instance'
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (unless (and
              ;; Is the instance complete?
              (not (incomplete-instance-p instance))
              ;; Did the instance actually move?
              (if (eq 't dimensions-being-changed)
                  (flet ((fn (old-dimension-value)
                           (equal (cdr old-dimension-value)
                                  (instance-dimension-value 
                                   instance (car old-dimension-value)))))
                    (declare (dynamic-extent #'fn))
                    (every #'fn old-dimension-values))
                  (flet ((fn (dimension-name)
                           (equal (cdr (assq dimension-name 
                                             old-dimension-values))
                                  (instance-dimension-value
                                   instance dimension-name))))
                    (declare (dynamic-extent #'fn))
                    (every #'fn dimensions-being-changed))))
      (dolist (space-instance 
                  (standard-unit-instance.%%space-instances%% instance))
        #+OLD-CLASS-NAMES
        (signal-event-using-class
         (load-time-value
          (find-class 'move-instance-within-space-instance-event))
         :instance instance
         :space-instance space-instance)
        (dolist (storage (storage-objects-for-add/move/remove
                          unit-class space-instance))
          ;; these two are a bit heavy handed, but OK for now... 
          ;; improve/integrate them soon!!!
          (remove-instance-from-storage 
           instance storage old-dimension-values 
           dimensions-being-changed verbose)
          (add-instance-to-storage instance storage verbose))
        (signal-event-using-class
         (load-time-value
          (find-class 'instance-moved-within-space-instance-event))
         :instance instance
         :space-instance space-instance)))))

;;; ---------------------------------------------------------------------------

(defmacro with-changing-dimension-values ((instance 
                                           &rest dimensions-being-changed)
                                          &body body)
  (with-once-only-bindings (instance)
    (with-gensyms (current-dimension-values)
      ;; We must remember all the dimension values, even if we are changing 
      ;; only a single/small number of them, as the other dimension values 
      ;; may also be needed to update the storage locators:
      `(let ((,current-dimension-values
              (instance-dimension-values ,instance t)))
         (multiple-value-prog1 (progn ,@body)
           (update-instance-locators
            ,instance ,current-dimension-values 
            ',(or dimensions-being-changed 't) nil))))))

;;; ---------------------------------------------------------------------------

(defun inconsistent-instance-locators-error (instance storage problem)
  (error "Instance ~s is ~a in ~s" instance problem storage))

;;; ---------------------------------------------------------------------------

(defun check-instance-locators (instance)
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    ;; Check each storage on each space instance:
    (dolist (space-instance 
                (standard-unit-instance.%%space-instances%% instance))
      (dolist (storage (storage-objects-for-add/move/remove
                        unit-class space-instance))
        (check-instance-storage-locators instance storage)))))

;;; ===========================================================================
;;;   Parser for unit-class/instance specifiers (placed here rather than next
;;;   to parse-unit-class-specifier in units.lisp to optimize
;;;   standard-unit-instance typep test)

(defun parse-unit-class/instance-specifier (unit-class/instance-spec)
  ;;; Handles unit-class and instance(s) specifiers (used in add-event-function
  ;;; and friends).  Returns two values: unit-class or instance(s) and
  ;;; plus-subclasses indicator (plus-subclasses is nil for unit-instances).
  (with-full-optimization ()
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq unit-class/instance-spec 't) 
      (values (load-time-value (find-class 'standard-unit-instance)) 't))
     ;; a unit instance:
     ((typep unit-class/instance-spec 'standard-unit-instance)
      (values unit-class/instance-spec nil))
     ;; extended unit-class specification:
     ((consp unit-class/instance-spec)
      (if (flet ((fn (x) (typep x 'standard-unit-instance)))
            (declare (dynamic-extent #'fn))
            (every #'fn unit-class/instance-spec))
          (values unit-class/instance-spec nil)
          (destructuring-bind (unit-class-name subclass-indicator)
              unit-class/instance-spec
            (let ((unit-class (find-unit-class unit-class-name)))
              (values unit-class 
                      (ecase subclass-indicator
                        ((:plus-subclasses +) 't)
                        ((:no-subclasses =) nil)))))))
     ;; anything else we assume is a unit-class-name or unit-class:
     (t (values (find-unit-class unit-class/instance-spec) nil)))))

;;; ---------------------------------------------------------------------------

(defun parse-unit-class-specifier (unit-class-spec)
  (with-full-optimization ()
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq unit-class-spec 't) 
      (values (load-time-value (find-class 'standard-unit-instance)) 't))
     ;; extended unit-class specification:
     ((consp unit-class-spec)
      (destructuring-bind (unit-class-name subclass-indicator)
          unit-class-spec
        (let ((unit-class (find-unit-class unit-class-name)))
          (values unit-class 
                  (ecase subclass-indicator
                    ((:plus-subclasses +) 't)
                    ((:no-subclasses =) nil))))))
     ;; anything else we assume is a unit-class-name or unit-class:
     (t (values (find-unit-class unit-class-spec) nil)))))

;;; ---------------------------------------------------------------------------

(defun parse-unit-classes-specifier (unit-classes-spec)
  (cond
   ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
   ((eq unit-classes-spec 't) 
    (load-time-value `((,(find-class 'standard-unit-instance) . t))))
   ((consp unit-classes-spec)
    (flet ((do-one-spec (unit-class-spec)
             (if (consp unit-class-spec)
                 (destructuring-bind (unit-class-name subclass-indicator)
                     unit-class-spec
                   (let ((unit-class (find-unit-class unit-class-name)))
                     `(,unit-class . ,(ecase subclass-indicator
                                        ((:plus-subclasses +) 't)
                                        ((:no-subclasses =) nil)))))
                 ;; anything else we assume is a unit-class-name or
                 ;; unit-class:
                 `(,(find-unit-class unit-class-spec) . nil))))
      (let ((possible-subclass-indicator (second unit-classes-spec)))
        (cond 
         ;; simply an extended unit-class specification?
         ((memq possible-subclass-indicator 
                '(:plus-subclasses + :no-subclasses =))
          (list (do-one-spec unit-classes-spec)))
         ;; a list of specifications:
         (t (mapcar #'do-one-spec unit-classes-spec))))))
   ;; anything else we assume is a unit-class-name or unit-class:
   (t `((,(find-unit-class unit-classes-spec) . nil)))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
