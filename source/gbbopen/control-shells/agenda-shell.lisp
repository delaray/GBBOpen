;;;; -*- Mode:Common-Lisp; Package:AGENDA-SHELL; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/control-shells/agenda-shell.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 25 02:31:03 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Agenda-Based Control Shell 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-07-04 File created.  (Corkill)
;;;  07-15-04 Added control-shell stepping.  (Corkill)
;;;  07-22-04 Added KS trigger removal.  (Corkill)
;;;  07-26-04 Added control-shell step result reporting.  (Corkill)
;;;  08-05-04 Removed hacks supporting CMUCL-18e non-compliances.  (Corkill)
;;;  01-24-05 Added continue-past-quiescence option [dangerous!].  (Corkill)
;;;  01-25-05 Added :quiescence stepping and stepping-type-specific
;;;           disabling command.  (Corkill)
;;;  04-03-05 Added UNDEFINE-KS macro.  (Corkill)
;;;  04-25-05 Added control-shell output stream control.  (Corkill)
;;;  10-21-05 Added polling-function "hibernation" support for uniprocessing
;;;           CLs.  (Corkill)
;;;  12-20-05 Added control-shell pause.  (Corkill)
;;;  03-24-06 Added control-shell-cycle-event.  (Corkill)
;;;  10-05-06 Added ksa-class metaclass check.  (Corkill)
;;;  11-13-06 Added ABORT-KS-EXECUTION.  (Corkill)
;;;  08-27-07 Renamed CONTROL-SHELL-STARTED-P to CONTROL-SHELL-RUNNING-P.
;;;           (Corkill)
;;;  04-15-08 Add threading-started checks for CMUCL and LispWorks.  (Corkill)
;;;  06-27-08 Add user-accessable PENDING-KSAS-OF, EXECUTED-KSAS-OF, and
;;;           OBVIATED-KSAS-OF control-shell-object readers, 
;;;           & CURRENT-CONTROL-SHELL.  (Corkill)
;;;  02-22-11 Renamed some events to better suggest their timing.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :agenda-shell)

;;; Import needed entities (MOP entities are imported from the :gbbopen package
;;; to hide implementation-specific MOP package differences):
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen::*%%allow-setf-on-link%%*
            gbbopen::do-evfns
            gbbopen::effective-slot-definition-class
	    gbbopen::evfn-blk.ks-triggers
            gbbopen::evfn-describer
            gbbopen::map-extended-unit-classes
            gbbopen::map-instances-given-class
            gbbopen::queue.lock
	    gbbopen::show-evfn-describer-headers
            gbbopen::slot-value-using-class
	    gbbopen::standard-unit-class.lock
	    gbbopen::standard-unit-class.instance-hash-table
            ;; Threading checkers for CMUCL and LispWorks:
            #+(and cmu mp)
            portable-threads::check-idle-process
            #+lispworks
            portable-threads::check-for-multiprocessing-started)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(==                          ; not yet documented
            abort-ks-execution
            activation-cycle-of
	    agenda-shell-node-state     ; not yet documented
	    awaken-control-shell	; not yet documented
            collect-trigger-instances
            control-shell               ; class not yet documented
	    control-shell-running-p
	    cs.pause                    ; not documented
            current-control-shell
            define-ks
            describe-ks
	    ensure-ks
            execute-ksa                 ; not yet documented
            executed-ksas-of
            execution-cycle-of
            exit-all-control-shell-threads ; not yet documented
            exit-control-shell
            exit-control-shell-thread   ; not yet documented
            find-ks-by-name
            ks
            ks-enabled-p
            ks-of
            ksa
            ksa-queue                   ; not documented
            make-ks-activation          ; not yet documented
            most-positive-rating
            most-negative-rating
            obviate-ksa                 ; not yet documented
            obviated-ksas-of
            obviation-cycle
            ordered-ksa-queue
            pending-ksas-of
            rating
            rating-of
            restart-control-shell
            select-ksa-to-execute       ; not yet documented
	    sole-trigger-event-of
            sole-trigger-instance-of
            spawn-control-shell-thread  ; not yet documented
            start-control-shell
            trigger-events-of
            undefine-ks
            with-control-shell-instance ; not yet documented
            )))

;;; ===========================================================================
;;;   Constants, Types, and Globals

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Restrict KSA ratings to X3J13-guaranteed fixnum values
  (defconstant most-positive-rating 32767)
  (defconstant most-negative-rating -32768))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype rating ()
    '(integer #.most-negative-rating #.most-positive-rating)))

;;; ---------------------------------------------------------------------------
;;;  Global access variable:

(defvar == nil)

;;; ---------------------------------------------------------------------------
;;;  Control-shell threads:

(defvar *control-shell-threads* nil)

;;; ---------------------------------------------------------------------------
;;;  *CS* is used to hold the "current" control-shell unit instance

(defvar *cs* nil)

;;; ---------------------------------------------------------------------------
;;;  Indicates when a ks-execution-catcher is established in the
;;;  current dynamic extent:

(defvar *within-abort-ks-execution-catcher* nil)

;;; ---------------------------------------------------------------------------

(define-class ks-triggers ()
  ((triggers :initform nil)
   (retriggers :initform nil)
   (obviation-triggers :initform nil)))

;;; ===========================================================================
;;;   KST -- KS-trigger descriptor
;;;
;;;  A dotted pair (ks . attributes), where attributes contains the
;;;  the following flags:
;;;     propagate-event-classes
;;;     propagate-unit-classes

(defun kst.ks (kst)
  (car kst))

(defcm kst.ks (kst)
  `(car (the cons ,kst)))

;;; ---------------------------------------------------------------------------
;;; The flag accessors:

(macrolet 
    ((do-flag (flag index)
       `(progn
          (defun ,flag (kst)
            (declare (cons kst))
            (flag-set-p (cdr kst) ,index))
          
          (defcm ,flag (kst)
            `(flag-set-p (cdr (the cons ,kst)) ,,index))
          
          (defun (setf ,flag) (nv kst)
            (declare (cons kst))
            (setf (cdr kst)
                  (if nv
                      (set-flag (cdr kst) ,index)
                      (clear-flag (cdr kst) ,index)))
            nv))))
  (do-flag kst.propagate-event-classes 0)
  (do-flag kst.propagate-unit-classes 1))

;;; ---------------------------------------------------------------------------

(defun make-kst (ks propagate-event-classes propagate-unit-classes)
  (let ((flags 0))
    (when propagate-event-classes (setf flags (set-flag flags 0)))
    (when propagate-unit-classes (setf flags (set-flag flags 1)))
    (cons ks flags)))

;;; ===========================================================================
;;;   Generic Function Declarations

(defgeneric collect-trigger-instances (obj))
(defgeneric describe-ks (ks))
(defgeneric execute-ksa (ks ksa cs))
(defgeneric make-ks-activation (ks events initargs cs))
(defgeneric obviate-ksa (ks ksa cs))
(defgeneric select-ksa-to-execute (cs))
(defgeneric sole-trigger-event-of (ksa))
(defgeneric sole-trigger-instance-of (obj))

;;; ===========================================================================
;;;   Control-Shell State

(define-unit-class control-shell ()
  ((cycle :initform 0)
   (ks-activations-count :initform 0)
   (executed-ksas-count :initform 0)
   (obviated-ksas-count :initform 0)
   (current-ksa :initform nil)
   (event-buffer :initform nil)
   (events-being-processed :initform nil)
   (event-buffer-lock :initform (make-lock :name "event-buffer"))
   (pending-ksas :initform (make-queue :class 'ordered-ksa-queue
                                       :key #'rating-of)
                 :reader pending-ksas-of
                 :writer (setf control-shell.pending-ksas))
   (executed-ksas :initform (make-queue :class 'ksa-queue)
                  :reader executed-ksas-of
                  :writer (setf control-shell.executed-ksas))
   (obviated-ksas :initform (make-queue :class 'ksa-queue)
                  :reader obviated-ksas-of
                  :writer (setf control-shell.obviated-ksas))
   (fifo-queue-ordering :initform 't :type boolean)
   (hibernating :initform nil :type boolean)
   (hibernating-cv :initform (make-condition-variable))
   (hibernate-on-quiescence :initform nil :type boolean) 
   (awaken-on-event :initform 't :type boolean)
   (run-polling-functions :initform #+threads-not-available 't
                                    #-threads-not-available nil
                          :type boolean)
   (continue-past-quiescence :initform nil :type boolean) 
   (thread :initform nil)
   (save-executed-ksas :initform nil :type boolean)
   (save-obviated-ksas :initform nil :type boolean)
   (stepping :initform nil)
   (stepping-stream :initform *query-io*)
   (minimum-ksa-execution-rating :initform 1 :type rating)
   (print :initform 't)
   (output-stream :initform *trace-output*)
   (pause :initform nil)
   (in-control-shell-loop-p :initform nil))
  (:retain t)
  (:instance-name-comparison-test equal)
  (:generate-accessors t :exclude pending-ksas executed-ksas obviated-ksas)
  (:generate-accessors-format :prefix)
  (:generate-accessors-prefix #.(dotted-conc-name '#:cs)))

;;; ---------------------------------------------------------------------------

(defmethod omitted-slots-for-saving/sending ((control-shell control-shell))
  #+ecl (declare (ignore control-shell))
  (list* 'event-buffer-lock
         ;; If we record the run-polling-functions value, then we risk
         ;; problems if an in-progress application without polling is
         ;; restarted on a CL without threads.  On the other hand, if someone
         ;; has forced polling on a threaded CL, then we lose that setting.
         ;; We assume the former situation is the more frequent/helpful and
         ;; allow run-polling-functions to be initialized rather than
         ;; saved/sent:
         'run-polling-functions
         'hibernating-cv
         'thread
         'stepping-stream
         'output-stream
         ;; a saved control-shell is never running:
         'in-control-shell-loop-p
         (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod delete-instance ((control-shell control-shell))
  ;; delete ksa-queue elements:
  (map-queue #'delete-instance (pending-ksas-of control-shell))
  (map-queue #'delete-instance (executed-ksas-of control-shell))
  (map-queue #'delete-instance (obviated-ksas-of control-shell))
  ;; delete ksa-queue headers:
  (delete-instance (pending-ksas-of control-shell))
  (delete-instance (executed-ksas-of control-shell))
  (delete-instance (obviated-ksas-of control-shell))
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defun current-control-shell ()
  (let ((cs *cs*))
    (and (typep cs 'control-shell)
         (not (instance-deleted-p cs))
         cs)))

(defcm current-control-shell ()
  (with-gensyms (cs)
    `(let ((,cs *cs*))
       (and (typep ,cs 'control-shell)
            (not (instance-deleted-p ,cs))
            ,cs))))

;;; ---------------------------------------------------------------------------
;;;  Multinode support

(define-class agenda-shell-node-state (gbbopen-node-state)
  (cs))

;;; ---------------------------------------------------------------------------

(defmethod save-gbbopen-node-state ((node-state agenda-shell-node-state))
  (call-next-method)
  (setf (cs-of node-state) *cs*))

;;; ---------------------------------------------------------------------------

(defmethod restore-gbbopen-node-state ((node-state agenda-shell-node-state))
  (call-next-method)
  (setf *cs* (cs-of node-state)))

;;; ===========================================================================
;;;   KS Unit-Class

(define-unit-class ks ()
  ((activation-predicate
    :initform nil)
   (activations
    :link (ksa ks :singular t))
   (enabled
    :accessor ks-enabled-p
    :initform t)
   (execution-function
    :initform nil)
   (obviation-predicate
    :initform nil)
   (pending-activations
    :link (ksa pending-activation :singular t))
   (precondition-function
    :initform nil)
   (rating
    :initform 1
    :type rating)
   (retrigger-function
    :initform nil)
   (revalidation-predicate
    :initform nil)
   (ksa-class 
    :initform 'ksa)
   (trigger-events
    :initform '()
    :type list)
   (retrigger-events
    :initform '()
    :type list)
   (obviation-events
    :initform '()
    :type list))
  (:retain :propagate)
  (:export-class-name t)
  (:generate-accessors t :exclude enabled))

;;; ---------------------------------------------------------------------------

(defmethod reinitialize-instance :before ((ks ks) &rest args)
  (declare (ignore args))
  ;; remove all triggers of old `ks':  
  (remove-ks-triggers ks))

;;; ---------------------------------------------------------------------------

(defmethod delete-instance :before ((ks ks))
  ;; remove all triggers of `ks':
  (remove-ks-triggers ks))
  
;;; ===========================================================================
;;;   KSA unit-class and queues

(define-unit-class ksa-queue (queue)
  ()
  ;; reset-gbbopen should retain ksa-queue instances (unless an :all-classes
  ;; reset-gbbopen is requested); propagate retention to subclasses:
  (:retain :propagate))

;;; ---------------------------------------------------------------------------

(define-unit-class ordered-ksa-queue (ksa-queue ordered-queue)
  ())

;;; ---------------------------------------------------------------------------

(define-unit-class ksa (queue-element)
  ((activation-cycle :initform nil)
   (execution-cycle :initform nil)
   (obviation-cycle :initform nil)
   (ks 
    :link (ks activations)
    :singular t)
   (pending-activation
    :singular t
    :link (ks pending-activations))
   (rating
    :initform 1
    :type rating)
   (trigger-events
    :reader trigger-events-of
    :initform nil))
  (:metaclass standard-ksa-class)
  (:generate-accessors t :exclude trigger-events)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((instance ksa) stream)
  (call-next-method)
  (print-instance-slot-value instance 'ks stream)
  (print-instance-slot-value instance 'rating stream))

;;; ---------------------------------------------------------------------------

(defmethod effective-slot-definition-class ((class standard-ksa-class)
                                            &key name &allow-other-keys)
  #+ecl (declare (ignore class))
  (if (eq name 'rating)
      (load-time-value (find-class 'effective-rating-slot-definition))
      (call-next-method)))

;;; ---------------------------------------------------------------------------
;;;  Verify that user-defined KSA classes have the proper metaclass:

(defmethod initialize-instance :before ((ksa ksa)
                                        &key)
  (declare (inline class-of))
  (let ((metaclass (class-of ksa)))
    (check-type metaclass standard-ksa-class)))

;;; ---------------------------------------------------------------------------
;;; Move a pending KSA on the queue when its rating is changed.  This doesn't
;;; work on Lisps that optimize defclass slot-writer methods rather than
;;; calling the (setf slot-value-using-class) method.  With those Lisps, we
;;; could attach to all the slot writer methods, but we haven't done so yet.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to
;;; control the use of slot reader/writer methods (so we set this option to
;;; nil for unit classes).

(defmethod (setf slot-value-using-class) :around
           (nv (class standard-ksa-class) (ksa ksa) 
            (slot #-lispworks effective-rating-slot-definition
                  #+lispworks (eql 'rating)))
  #+ecl (declare (ignore slot class))
  (cond 
   ;; If we are being called from initialize-instance:
   (*%%allow-setf-on-link%%* (call-next-method))
   ;; Otherwise:
   (t (when (or (execution-cycle-of ksa)
                (obviation-cycle-of ksa))
        (error "KSA ~s is no longer pending." ksa))
      (let ((queue (on-queue-p ksa)))
        ;; Should be on the pending-ksas queue, but we'll check just in case:
        (when (eq queue (pending-ksas-of *cs*))
          (with-lock-held ((queue.lock queue))
            (call-next-method)
            (move-ksa-on-queue ksa))))
      ;; return the new rating:
      nv)))

;;; ===========================================================================
;;;   Define-KS

(defmacro define-ks (name &rest initargs
                     &key (ks-class 'ks)
                     &allow-other-keys)  
  `(ensure-ks 
    ',ks-class ',name
    ,@(loop for (indicator value) on initargs by #'cddr
	  nconc
	    (case indicator
	      ;; remove :ks-class value (handled above):
	      (:ks-class)
	      ;; non-evaluated values; quote them unless they were
	      ;; already quoted:
	      ((:ksa-class 
		:trigger-events :retrigger-events :obviation-events)
	       (list indicator 
		     (if (and (consp value) (eq (first value) 'quote))
			 value
			 (list 'quote value))))
	      ;; other values are evaluated (generally results in
	      ;; user-expected behavior):
	      (otherwise (list indicator value))))))

;;; ---------------------------------------------------------------------------

(defun ensure-ks (class-name name &rest args)
  (declare (dynamic-extent args))
  (let* ((existing-ks (find-instance-by-name name '(ks :plus-subclasses)))
         (ks (cond (existing-ks
                    (apply #'reinitialize-instance existing-ks 
                           :instance-name name
                           args)
                    ;; CLISP, Clozure, ECL, and Lispworks don't return the
                    ;; instance from reinitialize-instance!
                    #+(or clisp clozure ecl lispworks)
                    existing-ks)
                   (t (apply #'make-instance class-name 
                             :instance-name name
                             args)))))
    (check-type ks ks)
    ks))
          
;;; ---------------------------------------------------------------------------

(defmethod do-evfns :after (evfn-blk event-class args)
  ;;; Uses control-shell interface to event signaling (this method). Creates
  ;;; an event instance, pushes it onto the event buffer, and awakens the
  ;;; agenda shell, if necessary.
  (let ((ks-triggers (evfn-blk.ks-triggers evfn-blk)))
    (when ks-triggers
      (let ((event (apply #'make-instance (class-name event-class)
                          :ks-triggers ks-triggers
                          args))
            (cs (or (current-control-shell)
		    ;; Look for a control-shell thread:
		    #-threads-not-available
                    (flet ((fn (thread)
                             (symbol-value-in-thread '*cs* thread)))
                      (declare (dynamic-extent #'fn))
                      (some #'fn *control-shell-threads*)))))
        (cond (cs (with-lock-held ((cs.event-buffer-lock cs))
                    (push event (cs.event-buffer cs))
                    #+threads-not-available
                    (setf (cs.hibernating cs) nil)
                    #-threads-not-available
                    (with-lock-held ((cs.hibernating-cv cs))
                      (when (and (cs.hibernating cs)
                                 (cs.awaken-on-event cs))
                        (setf (cs.hibernating cs) nil)
                        (condition-variable-signal (cs.hibernating-cv cs))))))
	      (*warn-about-unusual-requests*
	       (warn "No control shell is servicing event ~s"
		     event)))))))

;;; ---------------------------------------------------------------------------

(defmethod evfn-describer :after (evfn-blk fn)
  ;;; Control-shell extension to event-function describing mechanism:
  (when (or (eq fn 't) (eq fn nil))
    (let ((ks-triggers (evfn-blk.ks-triggers evfn-blk)))
      (when ks-triggers
        (let ((triggers (triggers-of ks-triggers))
              (obviates (obviation-triggers-of ks-triggers)) 
              (retriggers (retriggers-of ks-triggers)))
          (labels ((describe-it (label ksts)
                     (when ksts
                       (format t "~&~6t~a: ~@<~{~s~^, ~_~}~@:>~%" 
			       label
                               (flet ((fn (kst)
                                        (instance-name-of (kst.ks kst))))
                                 (declare (dynamic-extent #'fn))
                                 (mapcar #'fn ksts))))))
            (when (or triggers obviates retriggers)
              (show-evfn-describer-headers)
              (describe-it "Triggers" triggers)
              (describe-it "Obviates" obviates)
              (describe-it "Retriggers" retriggers))))))))

;;; ---------------------------------------------------------------------------

(defun add-ks-trigger-to-evfn-blk (evfn-blk plus-subevents plus-subclasses 
                                   ks slot-name)
  (let ((ks-triggers (or (evfn-blk.ks-triggers evfn-blk)
                         (setf (evfn-blk.ks-triggers evfn-blk)
                               (make-instance 'ks-triggers)))))
    (pushnew (make-kst ks plus-subevents plus-subclasses)
             (slot-value ks-triggers slot-name)
             :test #'eq
             :key #'kst.ks)))

;;; ---------------------------------------------------------------------------

(defun add-ks-triggers (ks)
  (flet ((setup (trigger-events ks-triggers-slot-name)
           (dolist (trigger-event trigger-events)
             (apply #'add-event-function nil
                    (append trigger-event 
                            `(:evfn-blk-fn add-ks-trigger-to-evfn-blk
                              :evfn-blk-fn-args 
                                 (,ks ,ks-triggers-slot-name)))))))
    ;; trigger events:
    (setup (trigger-events-of ks) 'triggers)
    ;; obviation events:
    (setup (obviation-events-of ks) 'obviation-triggers)
    ;; retrigger events:
    (setup (retrigger-events-of ks) 'retriggers)))

;;; ---------------------------------------------------------------------------

(defun remove-ks-trigger-from-evfn-blk (evfn-blk ks slot-name)
  (let ((ks-triggers (evfn-blk.ks-triggers evfn-blk)))
    (when ks-triggers
      (setf (slot-value ks-triggers slot-name)
            (delete ks (slot-value ks-triggers slot-name)
                    :test #'eq
                    :key #'kst.ks)))))

;;; ---------------------------------------------------------------------------

(defun remove-ks-triggers (ks)
  (flet ((remove-em (trigger-events ks-triggers-slot-name)
           (dolist (trigger-event trigger-events)
             (apply #'remove-event-function nil
                    (append trigger-event 
                            `(:evfn-blk-fn remove-ks-trigger-from-evfn-blk
                              :evfn-blk-fn-args 
                                 (,ks ,ks-triggers-slot-name)))))))
    ;; triggers:
    (remove-em (trigger-events-of ks) 'triggers)
    ;; obviation triggers:
    (remove-em (obviation-events-of ks) 'obviation-triggers)
    ;; retriggers:
    (remove-em (retrigger-events-of ks) 'retriggers)))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :around ((ks ks) slot-names 
                                      &key rating 
                                           precondition-function
                                           instance-name)
  (declare (ignore slot-names))
  (when (and precondition-function rating)
    (error "Both ~s and ~s were specified for KS ~s."
           ':precondition-function
           ':rating
           instance-name))
  (when (and rating (constantp rating))
    (check-type rating rating))
  (call-next-method)
  (add-ks-triggers ks))

;;; ===========================================================================
;;;   Undefine-KS (syntactic sugar for easy deletion of a KS object)

(defmacro undefine-ks (name &key)
  `(let ((ks (find-ks-by-name ',name)))
     (if ks 
         (delete-instance ks)
         (undefined-ks-message ',name))))

;;; ===========================================================================
;;;   Aborting KS execution

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-abort-ks-execution-catcher ((&key report) &body body)
    (if report
        `(let* ((current-thread (current-thread))
                (result
                 (catch 'abort-ks-execution-catcher 
                   (let ((*within-abort-ks-execution-catcher* current-thread))
                     ,@body))))
           (when (and (eq result current-thread)
                      (cs.print cs))
             (format (cs.output-stream cs)
                     "~&;; KS execution ~s was aborted.~%"
                     (cs.current-ksa cs))))
        `(catch 'abort-ks-execution-catcher 
           (let ((*within-abort-ks-execution-catcher* (current-thread)))
             ,@body)))))

;;; ---------------------------------------------------------------------------

(defun abort-ks-execution 
    (&optional 
     #-threads-not-available
     (cs (or (current-control-shell)
             ;; Look for a control-shell thread:
             (flet ((fn (thread)
                      (symbol-value-in-thread '*cs* thread)))
               (declare (dynamic-extent #'fn))
               (some #'fn *control-shell-threads*))
             (error "No control-shell thread was found"))))
  (cond 
   ((eq (current-thread) *within-abort-ks-execution-catcher*)
    (throw 'abort-ks-execution-catcher *within-abort-ks-execution-catcher*))
   (t #-threads-not-available
      (flet ((fn ()
               (when *within-abort-ks-execution-catcher*
                 (throw 'abort-ks-execution-catcher
                   *within-abort-ks-execution-catcher*))))
        (declare (dynamic-extent #'fn))
        (run-in-thread (cs.thread cs) #'fn)))))

;;; ===========================================================================
;;;   Agenda-Shell Event Definitions

(define-event-class control-shell-event (non-instance-event)
  ()
  (:abstract t)
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class start-control-shell-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-started-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class restart-control-shell-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-restarted-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-cycle-event (control-shell-event)
  (cycle)
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

(define-event-class quiescence-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class control-shell-hibernation-event (control-shell-event)
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-hibernating/awakened-event 
    (control-shell-event)
  ()
  (:abstract t)
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-hibernating-event 
    (control-shell-hibernating/awakened-event )
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class control-shell-awakened-event
    (control-shell-hibernating/awakened-event )
  ()
  (:metaclass non-instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class ksa-event (control-shell-event instance-event)
  (cycle)
  (:abstract t)
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class ksa-activation-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class ksa-activated-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class ksa-execution-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class ksa-executing-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class ksa-obviation-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class ksa-obviated-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

#+OLD-EVENT-NAMES
(define-event-class ksa-retrigger-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ---------------------------------------------------------------------------

(define-event-class ksa-retriggered-event (ksa-event)
  ()
  (:metaclass instance-event-class)
  (:export-class-name t)
  (:system-event t))                      

;;; ===========================================================================
;;;   Agenda Shell utilities

(defun find-ks-by-name (ks-name)
  (find-instance-by-name ks-name '(ks :plus-subclasses)))

;;; ---------------------------------------------------------------------------

(defun undefined-ks-message (ks-name)
  (format t "~&;; KS ~s is not defined~%" ks-name)
  (values))  

;;; ---------------------------------------------------------------------------

(defmethod describe-ks ((ks-name symbol))
  (let ((ks (find-ks-by-name ks-name)))
    (cond ((null ks)
           (undefined-ks-message ks-name)
           (values))
          (t (describe-ks ks)))))

(defmethod describe-ks ((ks ks))
  (format t "~2&KS: ~s~@[ [disabled]~]" 
          (instance-name-of ks)
          (not (ks-enabled-p ks)))
  (flet ((show-it (obj title control-string)
           (when obj (format t "~&~2t~a:~25t~@?" 
                             title control-string obj))))
    (show-it (trigger-events-of ks) "Trigger events" "~s")
    (show-it (obviation-events-of ks) "Obviation events" "~s")
    (show-it (retrigger-events-of ks) "Rerigger events" "~s")
    (let ((precondition-function (precondition-function-of ks)))
      (cond (precondition-function
             (show-it precondition-function 
                      "Precondition function" "~w"))
            (t (show-it (rating-of ks) "Rating" "~s"))))
    (show-it (execution-function-of ks) "Execution function" "~w"))
  (values))

;;; ---------------------------------------------------------------------------

(defmacro maybe-control-shell-step ((cs operation object &rest args)
                                    &rest body)
  `(let* ((.cs. ,cs)
          (.operation. ,operation)
          (== ,object)
          (.stepping. (cs.stepping .cs.))
          (.do-step-p. (and .stepping.
                           (or (eq .stepping. 't)
                               (memq .operation. .stepping.)))))
     (when .do-step-p.
       (control-shell-step .cs. .operation. ,@args))
     ,@(when body
         `((let ((.result-list. (multiple-value-list (progn ,@body))))
             (when .do-step-p.
               (control-shell-step-result .cs. .operation. .result-list.))
             (values-list .result-list.))))))

;;; ---------------------------------------------------------------------------

(defparameter *legal-stepping-options* 
    ;;; This list must be kept in synch with the options supported by 
    ;;; control-shell-step: 
    '(:activation-predicate 
      :ks-activation
      :ksa-execution
      :obviation-predicate
      :precondition-function
      :process-event 
      :quiescence
      :retrigger-function
      :revalidation-predicate))

;;; ---------------------------------------------------------------------------

(defun control-shell-step (cs operation predicate-arg
                           &aux (stream (cs.stepping-stream cs)))
  (flet ((help ()
           (format stream
                   "~&Stepping commands (follow with <Return>):~%~
                    ~3td       Disable this kind of stepping (~s)~%~
                    ~3te       Enable another kind of stepping~%~
                    ~3tf       Evaluate a form~%~
                    ~3th or ?  Help (this text)~%~
                    ~3tq       Quit (disable all stepping and continue)~%~
                    ~3ts       Show enabled stepping kinds~%~
                    ~3tx       Exit control shell~%~
                    ~3t=       Describe the object of interest (bound to ==)~%~
                    ~3t+       Enable all stepping~%~
                    ~3t-       Disable all stepping~%~
                    ~3t<Space> Continue (resume processing)~%"
                   operation))
         (prompt (control-string &rest args)
           (declare (dynamic-extent args))
           (format stream "~&>> CS Step (cycle ~s):~%~3t~?... " 
                   (cs.cycle cs) control-string args)
           (force-output stream)))
    (loop
      (ecase operation
        ;;; Update the *legal-stepping-options* list and 
        ;;; control-shell-step-result if these supported options change: 
        (:activation-predicate 
         (prompt "About to execute activation predicate for KS ~s~%~3ton ~s"
                 (instance-name-of predicate-arg)
                 (type-of ==)))
        (:ks-activation
         (prompt "About to activate KS ~s~%~3ton ~w" 
                 (instance-name-of predicate-arg)
                 (if (cddr ==)
                     (mapcar #'type-of ==)
                     (type-of (first ==)))))
        (:ksa-execution
         (prompt "About to execute KSA ~s" ==))
        (:obviation-predicate
         (prompt "About to execute obviation predicate for KS ~s~%~3ton ~s"
                 (instance-name-of predicate-arg)
                 (type-of ==)))
        (:precondition-function
         (prompt "About to execute precondition function for KS ~s~%~3ton ~s"
                 (instance-name-of predicate-arg)
                 (type-of ==)))
        (:process-event 
         (prompt "About to process event ~s" ==))
        (:quiescence
         (prompt "About to signal quiescence"))
        (:retrigger-function
         (prompt "About to execute retrigger function for KSA ~s~%~3ton ~s"
                 predicate-arg
                 (type-of ==)))
        (:revalidation-predicate
         (prompt "About to execute revalidation predicate for KS ~s"
                 (instance-name-of predicate-arg))))
      (case (char-downcase (read-char-immediately stream))
        (#\d 
         (let ((current-stepping (cs.stepping cs)))        
           (setf (cs.stepping cs)
                 (remove operation 
                         (if (eq current-stepping 't)
                             *legal-stepping-options*
                             current-stepping)
                         :test #'eq)))
         (format stream "~&~s stepping disabled~%" operation))
        (#\e
	 (let ((disabled-stepping-options
		(unless (eq (cs.stepping cs) 't)
		  (set-difference *legal-stepping-options* (cs.stepping cs)
				  :test #'eq))))
	   (cond
	    (disabled-stepping-options 
	     (format stream "~&Stepping is disabled for:")
	     (let ((max-index
		    (loop for type in disabled-stepping-options and
			  i from 1 
			finally (return i) do 
			  (format stream "~%~2t~2d ~s" i type))))
	       (format stream "~&Enable stepping type [1-~s]: " max-index)
	     (force-output stream)
	       (let ((option-to-enable
		      (let ((result (read)))
			(cond ((and (integerp result)
				    (<= 1 result max-index))
			       (nth (1-& result) disabled-stepping-options))
			      ((car (memq result disabled-stepping-options)))))))
		 (cond 
		  (option-to-enable
		   (push option-to-enable (cs.stepping cs))
		   (format stream "~&~s stepping enabled~%" option-to-enable))
		  (t (format stream "You must enter an integer between 1 and ~s~%"
			     max-index))))))
	    (t (format stream "~&All stepping types are currently enabled~%")))))
        (#\f
         (format stream "~&Form: ")
         (force-output stream)
         (format stream "~{~s~^~%~}"
                 (multiple-value-list (eval (read stream)))))
        ((#\h #\?) 
         (help))
        (#\q 
         (setf (cs.stepping cs) nil)
         (format stream "~&All stepping disabled~%")
         (return))
        (#\s
	 (let ((current-stepping
		(let ((stepping (cs.stepping cs)))
		  (if (eq stepping 't)
		      *legal-stepping-options*
		      stepping))))
	   (if current-stepping
	       (format stream "~&Stepping is enabled for:~{~%~2t~s~}"
		       current-stepping)
	       (format stream "~&All stepping is currently disabled~%"))))
	(#\x (when (nicer-y-or-n-p 
                    "Do you really want to exit the control shell? ")
	       (exit-control-shell)))
        (#\+
         (setf (cs.stepping cs) 't)
         (format stream "~&All stepping enabled~%"))
        (#\- 
         (setf (cs.stepping cs) nil)
         (format stream "~&All stepping disabled~%"))
        (#\= 
         (describe ==))
        (#\space 
         (return))
        (otherwise (help))))))

;;; ---------------------------------------------------------------------------

(defun control-shell-step-result (cs operation result-values
                                  &aux (stream (cs.stepping-stream cs)))
  (flet ((returned (predicate)
           (format stream "~&<< ~a returned: ~:i~{~w~^;~_~}~%"
                   predicate result-values)))
    (ecase operation
      (:activation-predicate 
       (returned "Activation predicate"))
      (:ks-activation)
      (:ksa-execution
       (returned (format nil "KSA ~s" (instance-name-of ==))))
      (:obviation-predicate
       (returned "Obviation predicate"))
      (:precondition-function
       (returned "Precondition function"))
      (:process-event)
      (:quiescence)
      (:retrigger-function)
      (:revalidation-predicate
       (returned "Revalidation predicate")))))

;;; ===========================================================================
;;;   Triggering-event collectors

(defmethod sole-trigger-event-of ((ksa ksa))
  ;;; Return the sole trigger event in the KSA's events:
  (sole-element (trigger-events-of ksa)))

;;; ===========================================================================
;;;   Triggering-instance collectors

(defmethod sole-trigger-instance-of ((ksa ksa))
  ;;; Return the sole trigger instance in the KSA's events:
  (let ((trigger-instance nil))
    (dolist (event (trigger-events-of ksa))
      (let ((instance (sole-trigger-instance-of event)))
        (when instance
          (when trigger-instance
            (error "Multiple trigger instances found for KSA ~s" ksa))
          (setf trigger-instance instance))))
    trigger-instance))

;;; --------------------------------------------------------------------------

(defmethod sole-trigger-instance-of ((event single-instance-event))
  ;;; Return the trigger instance in the single-instance-event:
  (instance-of event))
  
;;; --------------------------------------------------------------------------

(defmethod sole-trigger-instance-of ((event multiple-instance-event))
  ;;; Return the sole trigger instance in the multiple-instance-event (if
  ;;; there is only one); otherwise signal an error:
  (let ((instances (instances-of event)))
    (if (rest instances)
        (error "Multiple trigger instances found for event ~s" event)
        (first instances))))
  
;;; --------------------------------------------------------------------------

(defmethod sole-trigger-instance-of ((event non-instance-event))
  #+ecl (declare (ignore event))
  nil)
  
;;; --------------------------------------------------------------------------

(defmethod sole-trigger-instance-of ((events cons))
  ;;; Return the sole trigger instance in events:
  (let ((trigger-instance nil))
    (dolist (event events)
      (let ((instance (sole-trigger-instance-of event)))
        (when instance
          (when trigger-instance
            (error "Multiple trigger instances found in events ~s" events))
          (setf trigger-instance instance))))
    trigger-instance))

;;; ---------------------------------------------------------------------------

(defmethod collect-trigger-instances ((ksa ksa))
  ;;; Return all the trigger instances in the KSA's events:
  (let ((trigger-instances nil))
    (dolist (event (trigger-events-of ksa))
      (dolist (trigger-instance 
                  (ensure-list (collect-trigger-instances event)))
        (pushnew trigger-instance trigger-instances :test #'eq)))
    trigger-instances))

;;; --------------------------------------------------------------------------

(defmethod collect-trigger-instances ((event single-instance-event))
  ;;; Return the trigger instance in the single-instance-event (as a list):
  (list (instance-of event)))
  
;;; --------------------------------------------------------------------------

(defmethod collect-trigger-instances ((event multiple-instance-event))
  ;;; Return the trigger instances in the multiple-instance-event:
  (list (instances-of event)))
  
;;; --------------------------------------------------------------------------

(defmethod collect-trigger-instances ((event non-instance-event))
  #+ecl (declare (ignore event))
  nil)
  
;;; --------------------------------------------------------------------------

(defmethod collect-trigger-instances ((events cons))
  ;;; Return all the trigger instances in events:
  (let ((trigger-instances nil))
    (dolist (event events)
      (dolist (trigger-instance (collect-trigger-instances event))
        (pushnew trigger-instance trigger-instances :test #'eq)))
    trigger-instances))

;;; ===========================================================================
;;;   Agenda Shell operations

(defmethod make-ks-activation (ks events initargs cs)
  (let* ((cycle (cs.cycle cs))
	 (ksa (apply #'make-instance (ksa-class-of ks)
		     :pending-activation ks
		     :ks ks
		     :activation-cycle cycle
		     :trigger-events events
		     initargs)))
    (check-type ksa ksa)
    (insert-on-queue ksa (pending-ksas-of cs))
    (incf (cs.ks-activations-count cs))
    (with-update-stat (agenda-shell-ks-stats.activation ks))
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-activation-event))
     :instance ksa
     :cycle cycle)
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-activated-event))
     :instance ksa
     :cycle cycle)
    ksa))

;;; ---------------------------------------------------------------------------

(defmethod select-ksa-to-execute (cs)
  (let ((ksa (first-queue-element (pending-ksas-of cs))))
    (when (and ksa (>=& (rating-of ksa)
                        (cs.minimum-ksa-execution-rating cs)))
      ksa)))
      
;;; ---------------------------------------------------------------------------

(defmethod execute-ksa ((ks ks) (ksa ksa) cs)
  (let ((cycle (cs.cycle cs)))
    (setf (execution-cycle-of ksa) cycle)
    (incf (cs.executed-ksas-count cs))
    (unlinkf (pending-activations-of ks) ksa)
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-execution-event))
     :instance ksa 
     :cycle cycle)
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-executing-event))
     :instance ksa 
     :cycle cycle))
  (with-update-stat (agenda-shell-ks-stats.execution ks)
    (let ((execution-function (execution-function-of ks)))
      (when execution-function
        (funcall execution-function ksa)))))

;;; ---------------------------------------------------------------------------

(defmethod obviate-ksa ((ks ks) (ksa ksa) cs)
  (when (or (execution-cycle-of ksa)
            (obviation-cycle-of ksa))
    (error "KSA ~a is not a pending KSA." ksa))
  (let ((cycle (cs.cycle cs)))  
    (setf (obviation-cycle-of ksa) cycle)
    (incf (cs.obviated-ksas-count cs))
    (remove-from-queue ksa)
    (unlinkf (pending-activations-of ks) ksa)
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-obviation-event))
     :instance ksa
     :cycle cycle)
    (signal-event-using-class
     (load-time-value (find-event-class 'ksa-obviated-event))
     :instance ksa
     :cycle cycle)
    (if (cs.save-obviated-ksas cs)
	(insert-on-queue ksa (obviated-ksas-of cs))
	(delete-instance ksa))))

;;; --------------------------------------------------------------------------

(defun move-ksa-on-queue (ksa)
  ;;; Change the queue position of `ksa' when its rating is changed
  ;;; (queue.lock has been grabbed surrounding this call):
  (let ((queue (on-queue-p ksa)))
    (when queue
      (remove-from-queue ksa)
      (insert-on-queue ksa queue))))

;;; ---------------------------------------------------------------------------

(defun process-event-buffer (cs)
  ;; First, move the current events (now placed in chronological order) to an
  ;; "in processing" list of events. New events go into the event buffer for
  ;; processing in the next CS cycle:
  (with-lock-held ((cs.event-buffer-lock cs))
    (setf (cs.events-being-processed cs)
          ;; There might be some unprocessed events left if saved/sent was
          ;; performed during process-event-buffer:
          (nconc (cs.events-being-processed cs) 
                 (nreverse (cs.event-buffer cs))))
    (setf (cs.event-buffer cs) nil))
  ;; Now process each event:
  (let (event)
    (while (cs.events-being-processed cs)
      ;; We can't pop the event yet--just in case the user decides to
      ;; save or exit in the stepper--so we only peek at the event:
      (let ((event (first (cs.events-being-processed cs))))
        (maybe-control-shell-step (cs :process-event event event)))
      ;; Now we can pop the event and proceed:
      (setf event (pop (cs.events-being-processed cs)))
      (let ((ks-triggers (standard-event-instance.ks-triggers event)))
        ;; debugging check if ks-triggers is nil
        #+debugging-gbbopen
        (progn
          (printv event (object-address event 't))
          (unless ks-triggers
            (describe event)
            (pprint (all-threads))
            (error "Internal error: KS triggers of event ~s is nil"
                   event)))
        ;; clear these out, now that we are done with them!
        (setf (standard-event-instance.ks-triggers event) nil)
        ;; process interested trigger KSs:
        (dolist (kst (and 
                      #-debugging-gbbopen
                      ks-triggers
                      (triggers-of ks-triggers)))
          (let ((ks (kst.ks kst)))
            (when (and (not (instance-deleted-p ks))
                       (ks-enabled-p ks))
              (let ((activation-predicate (activation-predicate-of ks)))
                (when (or (not activation-predicate)
                          (progn
                            (maybe-control-shell-step 
                             (cs :activation-predicate event ks)
                             (with-update-stat 
                                 (agenda-shell-ks-stats.activation-predicate ks)
                               (funcall activation-predicate ks event)))))
                  (let ((precondition-function (precondition-function-of ks))
                        (rating (rating-of ks))
                        (initargs nil))
                    (when precondition-function
                      (multiple-value-setq (rating initargs)
                        (maybe-control-shell-step 
                         (cs :precondition-function event ks)
                         (with-update-stat 
                             (agenda-shell-ks-stats.precondition-function ks)
                           (funcall precondition-function ks event))))
                      (when (eq rating ':stop)
                        (when (cs.print cs)
                          (format (cs.output-stream cs)
                                  "~&;; Explicit :stop issued by KS ~
                                        ~s precondition function~%"
                                  (instance-name-of ks)))
                        (apply #'exit-control-shell ':stop initargs)))
                    (when rating
                      (let ((events (list event)))
                        (maybe-control-shell-step (cs :ks-activation events ks))
                        (make-ks-activation ks 
                                            events
                                            (list* :rating rating initargs)
                                            cs)))))))))
        ;; process interested obviation KSs:
        (dolist (kst (obviation-triggers-of ks-triggers))
          (let ((ks (kst.ks kst)))
            (unless (instance-deleted-p ks)
              (let ((obviation-predicate (obviation-predicate-of ks)))
                (dolist (ksa (pending-activations-of ks))             
                  (when (or (not obviation-predicate)
                            (progn
                              (maybe-control-shell-step 
                               (cs :obviation-predicate event ks)
                               (with-update-stat 
                                   (agenda-shell-ks-stats.obviation-predicate ks)
                                 (funcall obviation-predicate ksa event)))))
                    (obviate-ksa ks ksa cs)))))))
        ;; process interested retrigger KSs:
        (dolist (kst (retriggers-of ks-triggers))
          (let ((ks (kst.ks kst)))
            (unless (instance-deleted-p ks)
              (let ((retrigger-function (retrigger-function-of ks)))
                (when retrigger-function
                  (dolist (ksa (pending-activations-of ks))
                    (maybe-control-shell-step 
                     (cs :retrigger-function event ksa))
                    #+OLD-EVENT-NAMES
                    (signal-event-using-class
                     (load-time-value (find-event-class 'ksa-retrigger-event))
                     :instance ksa 
                     :cycle (cs.cycle cs))
                    (signal-event-using-class
                     (load-time-value (find-event-class 'ksa-retriggered-event))
                     :instance ksa 
                     :cycle (cs.cycle cs))
                    (with-update-stat 
                        (agenda-shell-ks-stats.retrigger-function ks)
                      (funcall retrigger-function ksa event))))))))))))

;;; ===========================================================================
;;;   Agenda Shell loop

(defun exit-control-shell (&rest result-values)
  (declare (dynamic-extent result-values))
  (throw 'exit-control-shell (values-list result-values)))

;;; ---------------------------------------------------------------------------

(defun hibernate-control-shell (cs)
  #+OLD-EVENT-NAMES
  (signal-event-using-class
   (load-time-value (find-event-class 'control-shell-hibernation-event)))
  (signal-event-using-class
   (load-time-value (find-event-class 'control-shell-hibernating-event)))
  ;; On a uniprocess CL we must "busy wait" (but not too fast) until something
  ;; awakens the control-shell:
  (cond 
   ((cs.run-polling-functions cs)
    (setf (cs.hibernating cs) 't)
    (while (cs.hibernating cs)
      (let ((sleep-time 0.5))
	(when (cs.run-polling-functions cs)
	  ;; run the polling functions, if so requested:
	  (let ((start-time (get-internal-real-time)))
	    (run-polling-functions)
	    (decf sleep-time (/ (- (get-internal-real-time) start-time)
				#.(float internal-time-units-per-second)))))
	;; sleep remainder of one-half second period (unless polling functions
	;; take longer than that):
	(when (plusp sleep-time)
	  (sleep sleep-time)))))
   (t #+threads-not-available
      (error "Polling functions must be used when threads are not available.")
      #-threads-not-available
      (with-lock-held ((cs.hibernating-cv cs))
        (setf (cs.hibernating cs) 't)
        (while (cs.hibernating cs)
          (condition-variable-wait (cs.hibernating-cv cs))))))
  (signal-event-using-class
   (load-time-value (find-event-class 'control-shell-awakened-event))))

;;; ---------------------------------------------------------------------------

(defun awaken-control-shell (cs)
  ;; Note: CS can also be awakened by do-evfns :after method:
  #+threads-not-available
  (setf (cs.hibernating cs) nil)
  #-threads-not-available
  (with-lock-held ((cs.hibernating-cv cs))
    (setf (cs.hibernating cs) nil)
    (condition-variable-signal (cs.hibernating-cv cs))))

;;; ---------------------------------------------------------------------------

(defun control-shell-loop (cs)
  (let* ((start-runtime (get-internal-run-time))
         (start-time (get-universal-time))
         (quiescence-handled-p nil))
    (setf (cs.in-control-shell-loop-p cs) 't)
    (unwind-protect 
	(multiple-value-prog1
	    (catch 'exit-control-shell 
	      (loop
		;; yield at the start of each cycle to allow pending
		;; threads, I/O, and events to progress:
		(thread-yield)
		;; run the polling functions, if so requested:
		(when (cs.run-polling-functions cs)
		  (run-polling-functions))
		;; pause, if requested:
		(when (cs.pause cs)
		  (hibernate-control-shell cs))
		;; signal the "real" start of the cycle
		(signal-event-using-class
                 (load-time-value (find-event-class 'control-shell-cycle-event))
                 :cycle (incf (cs.cycle cs)))
		;; when recording stats, record the cycle:
		(let ((stats *control-shell-stats*))
		  (when stats
		    (incf (car stats))))
		;; operate on the latest events:
		(process-event-buffer cs)
		;; look for a KSA to execute:
		(let ((ksa (select-ksa-to-execute cs)))
		  (cond
		   ;; no executable KSA found, quiescence:
		   ((not ksa)
		    (maybe-control-shell-step (cs :quiescence nil nil))
		    (cond
		     ;; if first time through quiescence state, signal the
		     ;; quiescence event:
		     ((not quiescence-handled-p)
		      ;; don't indicate that we've handled the event if we are
		      ;; continuing indefinitely:
		      (unless (cs.continue-past-quiescence cs)
			(setf quiescence-handled-p t))
		      (signal-event-using-class
                       (load-time-value (find-event-class 'quiescence-event))))
                     ;; if second consecutive time in quiescence state without
		     ;; finding a KSA to execute, then hibernate or exit:
		     (t (cond
			 ;; hibernate if requested:
			 ((cs.hibernate-on-quiescence cs)
			  (hibernate-control-shell cs))
			 ;; otherwise, exit the control shell:
			 (t (when (cs.print cs)
			      (format (cs.output-stream cs)
				      "~&;; No executable KSAs remain, ~
                                        exiting control shell~%"))
			    (throw 'exit-control-shell ':quiescence))))))
		   ;; We have a KSA to execute:
		   (t (setf quiescence-handled-p nil)
		      (let* ((ks (ks-of ksa))
			     (revalidation-predicate
			      (revalidation-predicate-of ks)))
			(cond 
			 ((or (null revalidation-predicate)
			      (progn
				(maybe-control-shell-step 
				 (cs :revalidation-predicate ksa ks)
				 (with-update-stat 
				     (agenda-shell-ks-stats.revalidation-predicate ks)
				   (funcall revalidation-predicate ksa)))))
			  ;; execute the KSA:
			  (let (result-values)
			    (maybe-control-shell-step 
			     (cs :ksa-execution ksa ks)
			     (remove-from-queue ksa)
                             (setf (cs.current-ksa cs) ksa)
			     (unwind-protect
                                 (with-abort-ks-execution-catcher ()
                                   (setf result-values
                                     (multiple-value-list
                                      (execute-ksa ks ksa cs))))
                               (setf (cs.current-ksa cs) nil)))
			    (cond 
			     ((cs.save-executed-ksas cs)
			      (insert-on-queue ksa (executed-ksas-of cs)))
			     (t (delete-instance ksa)))
			    (when (eq (car result-values) ':stop)
			      (when (cs.print cs)
				(format (cs.output-stream cs)
					"~&;; Explicit :stop issued by KS ~s~%"
					(instance-name-of ks)))
			      (apply #'exit-control-shell 
				     ':stop (rest result-values)))))
			 ;; obviate the KSA, due to a nil result from
			 ;; executing the revalidation-predicate:
			 (t (obviate-ksa ks ksa cs)))))))))
	  ;; on exit:
	  (when (cs.print cs)
	    (format (cs.output-stream cs)
		    "~&;; Control shell ~s exited: ~s cycle~:p completed~
                     ~%;; Run time: ~a~
                     ~%;; Elapsed time: ~a~%"
                    (instance-name-of cs)
		    (cs.cycle cs)
		    (pretty-run-time-duration
		     (- (get-internal-run-time) start-runtime))
		    (pretty-duration
		     (- (get-universal-time) start-time))))
	  (let ((pending-ksa-count (queue-length (pending-ksas-of cs))))
	    (when (and (cs.print cs) (plusp pending-ksa-count))
	      (format (cs.output-stream cs)
		      "~&;; ~s pending KSA~:p remain~@[s~] on the queue~%"
                  pending-ksa-count
                  (= pending-ksa-count 1)))))
      ;; always clear the loop-p flag:
      (setf (cs.in-control-shell-loop-p cs) nil))))

;;; ---------------------------------------------------------------------------

(defun control-shell-is-running-already (cs operation)
  (warn 
   "The control shell named ~s is already running in ~:[another~;this~] ~
    thread; ignoring ~s request."
   (instance-name-of cs)
   (eq (current-thread) (cs.thread cs))
   operation))

;;; ---------------------------------------------------------------------------

(defun control-shell-running-p (&optional (cs (current-control-shell)))
  (and (typep cs 'control-shell)
       (not (instance-deleted-p cs))
       (cs.in-control-shell-loop-p cs)))

;;; ---------------------------------------------------------------------------

(defun start-control-shell (&key 
                            (awaken-on-event 't)
                            (continue-past-quiescence nil)
                            (fifo-queue-ordering 't)
                            (hibernate-on-quiescence nil)
                            (minimum-ksa-execution-rating 1)
                            (instance-name 1)
			    (output-stream *trace-output*)
			    (pause nil)
                            (print 't)
			    (run-polling-functions
			        #+threads-not-available 't
				#-threads-not-available nil)
                            (save-executed-ksas nil)
                            (save-obviated-ksas nil)
                            (stepping nil)
                            (stepping-stream *query-io*))
  ;; Check that the idle process is running on CMUCL:
  #+(and cmu mp)
  (check-idle-process 't)
    ;; Check that threading is running on LispWorks:
  #+lispworks
  (check-for-multiprocessing-started 't)
  ;; There is a control shell that is running in this thread:
  (when (control-shell-running-p)
    (unless (nicer-y-or-n-p "A running control-shell instance named ~s is ~
                             associated with this thread.~%Delete it and ~
                             start the new one in its place? "
                        (instance-name-of *cs*))
      (format *query-io* "~&Ignoring ~s request.~%"
              'start-control-shell)
        (return-from start-control-shell nil))
      ;; OK to proceed with the new control shell, so abort the
      ;; current one:
      (exit-control-shell ':aborted))
  (let ((cs (find-instance-by-name instance-name 'control-shell)))
    ;; We found the named control shell:
    (when cs
      ;; It's running already in another thread:
      (when (and (control-shell-running-p cs)
                 (not (eq (current-thread) (cs.thread cs))))
        (control-shell-is-running-already cs 'start-control-shell)
        (return-from start-control-shell nil))
      ;; It's defunct, so we delete it:
      (delete-instance cs)))
  ;; Delete old *cs* instance, if present (and its queue headers):
  (when (and (typep *cs* 'control-shell)
             (not (instance-deleted-p *cs*)))
    (delete-instance *cs*))
  ;; Start it up:
  (setf *cs*
        (make-instance 'control-shell 
          :awaken-on-event awaken-on-event
          :continue-past-quiescence continue-past-quiescence
          :hibernate-on-quiescence hibernate-on-quiescence
          :instance-name instance-name
          :minimum-ksa-execution-rating minimum-ksa-execution-rating
          :output-stream output-stream
          :pause pause
          :print print
          :run-polling-functions run-polling-functions
          :save-executed-ksas save-executed-ksas
          :save-obviated-ksas save-obviated-ksas
          :stepping stepping
          :stepping-stream stepping-stream
          ;; Non-specifiable values:
          :pending-ksas (make-queue :class 'ordered-ksa-queue
                                    :test (if fifo-queue-ordering
                                              ;; SBCL optimizes these into
                                              ;; function objects that aren't
                                              ;; named (so they can't be
                                              ;; saved/sent)--use symbols
                                              ;; instead:
                                              #+sbcl '>
                                              #-sbcl #'>
                                              #+sbcl '>=
                                              #-sbcl #'>=)
                                    :key #'rating-of)
          :thread (current-thread)))
  (when (cs.print *cs*)
    (format (cs.output-stream *cs*) 
            "~&;; Control shell ~s started~%"
            (instance-name-of *cs*)))
  #+OLD-EVENT-NAMES
  (signal-event-using-class
   (load-time-value (find-event-class 'start-control-shell-event)))
  (signal-event-using-class
   (load-time-value (find-event-class 'control-shell-started-event)))
  (control-shell-loop *cs*))
   
;;; ---------------------------------------------------------------------------

(defun restart-control-shell (&key (instance-name 1))
  ;; Check that threading is running on CMUCL & LispWorks:
  #+(and cmu mp)
  (check-idle-process 't)
  #+lispworks
  (check-for-multiprocessing-started 't)
  (let ((cs (find-instance-by-name instance-name 'control-shell)))
     ;; No control-shell instance found:
    (unless cs
      (warn "No control shell named ~s was found; ignoring ~s request."
            instance-name
            'restart-control-shell)
      ;; return nil
      (return-from restart-control-shell nil))
    ;; We found the named control shell, it's running already in
    ;; another thread:
    (when (and (control-shell-running-p cs)
               (not (eq (current-thread) (cs.thread cs))))
      (control-shell-is-running-already cs 'restart-control-shell)
      (return-from restart-control-shell nil))

    ;; We found the named control shell, but the current control-shell
    ;; in this thread is different:
    (when (and (typep *cs* 'control-shell)
               (not (instance-deleted-p *cs*))
               (not (equal (instance-name-of *cs*) instance-name)))
      (unless (nicer-y-or-n-p "Another control-shell instance named ~s ~
                               (~@[not ~]running) is associated with this ~
                               thread.~%Delete it and restart ~s in its place? "
                        (instance-name-of *cs*)
                        (not (control-shell-running-p *cs*))
                        instance-name)
        (format *query-io* "~&Ignoring ~s request.~%"
                'restart-control-shell)
        (return-from restart-control-shell nil))
      ;; OK to proceed with the found control shell, so abort the
      ;; current one:
      (exit-control-shell ':aborted))
    ;; Ready to restart, but its running already in this thread:
    (when (control-shell-running-p cs)
      (control-shell-is-running-already cs 'restart-control-shell)
      (return-from restart-control-shell nil))
    ;; Finally, we can restart it up:
    (setf *cs* cs)
    (setf (cs.thread cs) (current-thread))
    (when (cs.print cs)
      (format (cs.output-stream cs)
              "~&;; Control shell ~s restarting after cycle ~s~%"
              (instance-name-of cs)
              (cs.cycle cs)))
    #+OLD-EVENT-NAMES
    (signal-event-using-class
     (load-time-value (find-event-class 'restart-control-shell-event)))
    (signal-event-using-class
     (load-time-value (find-event-class 'control-shell-restarted-event)))
    (control-shell-loop cs)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-control-shell-instance (() &body body)
    `(let ((*cs* nil))
       ,@body)))

;;; ---------------------------------------------------------------------------

(defun spawn-control-shell-thread (&rest initargs)
  (declare (dynamic-extent initargs))
  #+threads-not-available
  (declare (ignore initargs))
  #+threads-not-available
  (threads-not-available 'spawn-control-shell-thread)
  #-threads-not-available
  (let ((control-shell-thread
         (spawn-thread 
          "Agenda Shell"
          #'(lambda (initargs)
              (with-control-shell-instance ()
                (apply #'start-control-shell
                       (append initargs '(:hibernate-on-quiescence t)))))
          initargs)))
    (atomic-push control-shell-thread *control-shell-threads*)
    control-shell-thread))

;;; ---------------------------------------------------------------------------

(defun exit-control-shell-thread (control-shell-thread)
  #+threads-not-available
  (declare (ignore control-shell-thread))
  #+threads-not-available
  (portable-threads:threads-not-available 'exit-control-shell-thread)
  #-threads-not-available
  (progn
    (setf *control-shell-threads* 
      (delq-one control-shell-thread *control-shell-threads*))
    (run-in-thread control-shell-thread #'exit-control-shell)))
  
;;; ---------------------------------------------------------------------------

(defun exit-all-control-shell-threads ()
  (mapc #'exit-control-shell-thread *control-shell-threads*))

;;; ===========================================================================
;;;  The Agenda Shell is fully loaded

(pushnew ':agenda-shell *features*)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
