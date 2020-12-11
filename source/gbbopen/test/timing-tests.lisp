;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/timing-tests.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Apr 25 14:01:27 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *            Very Basic GBBopen and CL Timing Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-18-07 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :portable-threads))

;;; ---------------------------------------------------------------------------

(defparameter *test-size* 
    #-ecl 500000
    ;; ECL is much slower at making instances:
    #+ecl 20000)

;;; ---------------------------------------------------------------------------

(defvar *master-instance-lock* 
    (make-recursive-lock :name "Master instance lock"))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro time-it (&body body)
    (let ((start-time-sym (gensym)))
      `(let ((,start-time-sym (get-internal-run-time)))
         ,@body
         (format t " ~,2f seconds"
                 (/ (float (- (get-internal-run-time) ,start-time-sym))
                    (float internal-time-units-per-second)))))))

;;; ---------------------------------------------------------------------------

(define-class empty-standard-instance ()
  ())

;;; ---------------------------------------------------------------------------

(define-class non-empty-standard-instance ()
   ((instance-name :accessor instance-name-of)
    (%%space-instances%% :initarg :space-instances :initform nil))
   (:generate-accessors-format :prefix)
   (:generate-accessors t :exclude instance-name)
   (:generate-initargs t :exclude %%space-instances%%))

;;; ---------------------------------------------------------------------------

(define-class nomethods-instance (non-empty-standard-instance)
  ()
  (:metaclass testing-metaclass))

(define-class before-method-instance (non-empty-standard-instance)
  ()
  (:metaclass testing-metaclass))

(define-class after-method-instance (non-empty-standard-instance)
  ()
  (:metaclass testing-metaclass))

(define-class around-method-instance (non-empty-standard-instance)
  ()
  (:metaclass testing-metaclass))

(define-class around-locking-method-instance (non-empty-standard-instance)
  ()
  (:metaclass testing-metaclass))

(define-class before-and-after-methods-instance
    (before-method-instance
     after-method-instance)
  ()
  (:metaclass testing-metaclass))

(define-class baal-methods-instance (around-locking-method-instance
                                     before-and-after-methods-instance)
  ()
  (:metaclass testing-metaclass))

(define-class bahal-methods-instance (around-locking-method-instance
                                      before-method-instance)
  ()
  (:metaclass testing-metaclass))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :before ((instance before-method-instance)
                                      slot-names
                                      &key)
  (declare (ignore slot-names))
  instance)

(defmethod shared-initialize :after ((instance after-method-instance)
                                     slot-names 
                                     &key)  
  (declare (ignore slot-names))
  instance)

(defmethod shared-initialize :after ((instance bahal-methods-instance)
                                     slot-names 
                                     &key)  
  (declare (ignore slot-names))
  (declare (inline class-of))
  (let* ((class (class-of instance))
         (instance-hash-table 
          (testing-metaclass.instance-hash-table class))
         (instance-name 
          (incf (testing-metaclass.instance-name-counter
                 class))))
    (setf (gethash instance-name instance-hash-table) instance))
  instance)

(defmethod shared-initialize :around ((instance around-method-instance)
                                      slot-names &key)
  (declare (ignore slot-names))
  ;; allow initialization of link slots:
  (let ((*%%allow-setf-on-link%%* t)
        ;; Used to maintain any existing space-instance state between
        ;; shared-initialize :before and :after methods:
        (*%%existing-space-instances%%* nil))
    (declare (special *%%allow-setf-on-link%%* 
                      *%%existing-space-instances%%*))
    (call-next-method)))

(defmethod shared-initialize :around ((instance around-locking-method-instance)
                                      slot-names &key)
  (declare (ignore slot-names))
  ;; allow initialization of link slots:
  (let ((*%%allow-setf-on-link%%* t)
        ;; Used to maintain any existing space-instance state between
        ;; shared-initialize :before and :after methods:
        (*%%existing-space-instances%%* nil))
    (declare (special *%%allow-setf-on-link%%* 
                      *%%existing-space-instances%%*))
    (with-lock-held (*master-instance-lock*)
      (call-next-method))))

;;; ---------------------------------------------------------------------------

(defmethod reset-testing-metaclass ((class testing-metaclass))
  (setf (testing-metaclass.instance-name-counter class) 0)
  (setf (testing-metaclass.instance-hash-table class)
        (make-hash-table :test 'eq)))

;;; ---------------------------------------------------------------------------

(define-unit-class empty-unit-instance ()
  ())

;;; ---------------------------------------------------------------------------

(defun forced-format (&rest args)
  (declare (dynamic-extent args))
  (apply #'format t args)
  (force-output))

;;; ---------------------------------------------------------------------------

(defun empty-standard-instance-timing-test ()
  (format t "~&;; Creating ~:d empty standard instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'empty-standard-instance))))

;;; ---------------------------------------------------------------------------

(defun non-empty-standard-instance-timing-test ()
  (format t "~&;; Creating ~:d non-empty standard instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'non-empty-standard-instance))))

;;; ---------------------------------------------------------------------------

(defun nomethods-instance-timing-test ()
  (format t "~&;; Creating ~:d nomethods instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'nomethods-instance))))

;;; ---------------------------------------------------------------------------

(defun before-method-instance-timing-test ()
  (format t "~&;; Creating ~:d before-method instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'before-method-instance))))

;;; ---------------------------------------------------------------------------

(defun after-method-instance-timing-test ()
  (format t "~&;; Creating ~:d after-method instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'after-method-instance))))

;;; ---------------------------------------------------------------------------

(defun around-method-instance-timing-test ()
  (format t "~&;; Creating ~:d around-method instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'around-method-instance))))

;;; ---------------------------------------------------------------------------

(defun around-locking-method-instance-timing-test ()
  (format t "~&;; Creating ~:d around-locking method instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'around-locking-method-instance))))

;;; ---------------------------------------------------------------------------

(defun before-and-after-methods-instance-timing-test ()
  (format t "~&;; Creating ~:d before-and-after-methods instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'before-and-after-methods-instance))))

;;; ---------------------------------------------------------------------------

(defun baal-methods-instance-timing-test ()
  (format t "~&;; Creating ~:d before, after, and around locking methods ~
                  instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'baal-methods-instance))))

;;; ---------------------------------------------------------------------------

(defun hash-table-timing-test ()
  (let ((ht (make-hash-table :test 'eq))
        (counter 0))
    (format t "~&;; Timing ~:d hash-table insertions..."
            *test-size*)
    (time-it (dotimes (i *test-size*)
               (declare (fixnum i))
               (setf (gethash (incf counter) ht) counter)))))

;;; ---------------------------------------------------------------------------

(defun bahal-methods-instance-timing-test ()
  (format t "~&;; Creating ~:d before, around locking, and ht-after methods ~
                  instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'bahal-methods-instance)))
  (reset-testing-metaclass (find-class 'bahal-methods-instance)))

;;; ---------------------------------------------------------------------------

(defun empty-unit-instance-timing-test ()
  (format t "~&;; Creating ~:d empty unit instances..."
          *test-size*)
  (time-it (dotimes (i *test-size*)
             (declare (fixnum i))
             (make-instance 'empty-unit-instance))))

;;; ---------------------------------------------------------------------------

(defun do-timing-tests ()
  ;; Prepare for the tests:
  (unless (confirm-if-blackboard-repository-not-empty-p)
    (return-from do-timing-tests))
  (reset-gbbopen)
  (make-instance 'empty-standard-instance)
  (make-instance 'non-empty-standard-instance)
  (make-instance 'non-empty-standard-instance)
  (make-instance 'nomethods-instance)
  (make-instance 'before-method-instance)
  (make-instance 'after-method-instance)
  (make-instance 'around-method-instance)
  (make-instance 'around-locking-method-instance)
  (make-instance 'before-and-after-methods-instance)
  (make-instance 'baal-methods-instance)
  (make-instance 'bahal-methods-instance)
  (reset-testing-metaclass (find-class 'bahal-methods-instance))
  (make-instance 'empty-unit-instance)
  (reset-gbbopen)
  ;; Now do the tests:
  (empty-standard-instance-timing-test)
  (non-empty-standard-instance-timing-test)
  (nomethods-instance-timing-test)
  (before-method-instance-timing-test)
  (after-method-instance-timing-test)
  (around-method-instance-timing-test)
  (around-locking-method-instance-timing-test)
  (before-and-after-methods-instance-timing-test)
  (baal-methods-instance-timing-test)
  (hash-table-timing-test)
  (bahal-methods-instance-timing-test)
  (empty-unit-instance-timing-test))

;;; ---------------------------------------------------------------------------

(when (and (boundp '*autorun-modules*) 
           *autorun-modules*)
  (do-timing-tests))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


