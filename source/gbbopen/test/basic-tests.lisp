;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/basic-tests.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 25 16:14:16 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Simple Trip-Test File for GBBopen
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
;;;  07-20-02 File created.  (Corkill)
;;;  06-08-04 Added additional find-instances tests.  (Corkill)
;;;  06-11-07 Converted :prefix format accessors to "-of" format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*)))

;; Bring in some internal functions for testing purposes...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen::direct-link-definition.sort-function
            gbbopen::standard-unit-class.abstract)))

;;; ---------------------------------------------------------------------------

(defvar *skip-redefinition-testing* nil)

;;; ---------------------------------------------------------------------------

;; Allegro, CLISP, ECL, and Lispworks initialize CLOS classes at compile time,
;; so we need the test-size value in the compilation environment on those
;; implementations:
(eval-when (#+(or clisp allegro ecl lispworks) :compile-toplevel
              :load-toplevel :execute)
  (defparameter *timing-tests-size* 10000))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%timing-result%*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro brief-timer (n &body body)
    (cond 
     ;; Iterating timer:
     (n
      `(with-full-optimization ()
         ;; Do one untimed trial to prepare everything:
         (let ((i 0))
           (declare (ignorable i))
           (setf *%timing-result%* ,@body))
         (let ((%start-time% (get-internal-run-time)))
           (dotimes (i (& ,n))
             (declare (fixnum i))
             (setf *%timing-result%* ,@body))
           (locally (declare (notinline -))
             (- (get-internal-run-time) %start-time%)))))
     (t 
      `(with-full-optimization ()
         (let ((%start-time% (get-internal-run-time)))
           (setf *%timing-result%* ,@body)
           (locally (declare (notinline -))
             (- (get-internal-run-time) %start-time%))))))))

;;; ---------------------------------------------------------------------------

(defun fformat (stream &rest args)
  ;; Format followed by a force-output on the stream:
  (declare (dynamic-extent args))
  (apply #'format stream args)
  (force-output (if (eq stream 't) *standard-output* stream)))

;;; ---------------------------------------------------------------------------

(defun format-ticks (ticks)
  (format t "~6,2f seconds"
          (locally (declare (optimize (speed 0) (safety 3)))
            (/ ticks
               #.(float internal-time-units-per-second)))))
  
;;; ---------------------------------------------------------------------------

(declaim (special u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 cu1))

;;; ---------------------------------------------------------------------------

(define-class not-a-unit ()
  (non-unit-slot))

(define-unit-class abstract ()
  ;; An inherited slot:
  (x) 
  (:abstract t))

(define-unit-class uc-1 (abstract) 
  ((y :initarg :zzz
      :documentation "Another basic slot with an add'l initarg.")
   (slot-3 :initarg :z 
           :accessor z-of)
   (classification :initform :unknown)
   (amphibious :initform nil)
   (link-1 :link :reflexive
           :writer (setf blah)
           :singular t)
   (link-2 :link (uc-2 backlink-2))
   (link-3 :link (uc-2 backlink-3)
           :sort-function #'<
           :sort-key 'instance-name-of))
  (:default-initargs :x 1 :zzz 2)
  (:generate-accessors t :exclude slot-3)
  (:documentation "This is uc-1")
  (:dimensional-values
   (x :mixed x)
   (y (:point number) #'identity y)
   (z :point #'(lambda (instance)
                 (if (slot-boundp instance 'slot-3) 
                     (z-of instance)
                     unbound-value-indicator)))
   (classification (:element eq) classification)
   (amphibious :boolean amphibious))
  (:initial-space-instances (bb sub-bb space-1))
  (:estimated-instances 10))
  
(defmethod print-instance-slots ((obj uc-1) stream)
  (call-next-method)
  (when (and (slot-boundp obj 'x)
             (slot-boundp obj 'y)
             (slot-boundp obj 'slot-3))
    (format stream " (~s ~s ~s)"
            (slot-value obj 'x)
            (slot-value obj 'y)
            (slot-value obj 'slot-3))))

(define-unit-class forward-referenced-supers (uc-2 uc-3) ())

(define-unit-class uc-2 (uc-1 not-a-unit) 
  ((backlink-2 :link (uc-1 link-2 :singular nil)
	       :singular nil)
   (backlink-3 :link (uc-1 link-3)))
  (:estimated-instances *timing-tests-size*)
  (:use-global-instance-name-counter t))

(define-unit-class uc-2-clone (uc-2)
  ())

(define-unit-class uc-2-on-space-2 (uc-2)
  ()
  (:initial-space-instances (bb sub-bb space-2))
  (:estimated-instances *timing-tests-size*)
  (:use-global-instance-name-counter t))

(define-unit-class uc-3 (uc-1) 
  ((link-1 :documentation "Added doc for link-1.")))

(define-unit-class uc-4 (uc-3)
  ()
  (:dimensional-values 
   (x :interval x))
  (:initial-space-instances 
   #'(lambda (ui)
       (declare (ignore ui))
       (find-space-instance-by-path '(bb sub-bb space-1)))))

(define-unit-class uc-set (uc-4)
  ()
  (:dimensional-values 
   (x :mixed :set x)
   (classification (:element eq) :set classification)))

(define-unit-class uc-sequence (uc-4)
  ()
  (:dimensional-values 
   (x :point :sequence x)
   (y :point :sequence y)))

(define-unit-class uc-series (uc-4)
  ((time :initform nil))
  (:dimensional-values 
   (time :point :sequence time)
   (x :point :ascending-series time x)
   (y :point :ascending-series time y)))

(define-space-class my-space-instance (standard-space-instance)
  ((my-space-slot)))

(define-event-class my-event (non-instance-event)
  ((my-event-arg1 :initform nil)
   (my-event-arg2 :initform nil)))

;;; ---------------------------------------------------------------------------

(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

(defmethod print-instance-slots ((obj link-ptr-with-value) stream)
  (call-next-method)
  (print-instance-slot-value obj 'value stream))

;;; ---------------------------------------------------------------------------
;;;  Check link definitions

(check-link-definitions)

;;; ===========================================================================
;;;  Basic timing functions

(defun make-time-test (n &optional (class-name 'uc-2) qualifier-string)
  (declare (fixnum n))
  (fformat t "~&;; Running make-instance~@[ ~a~] timing test (~:d instance~:p)..." 
           qualifier-string
           n)
  (make-instance 'uc-1 :x 0 :y 0)
  (format-ticks (brief-timer n (make-instance class-name :x i :y i))))

;;; ---------------------------------------------------------------------------

(defun nonlink-slot-access-time-test (n)
  (declare (fixnum n))
  (setf n (min most-positive-fixnum (* n 100)))
  (let ((unit-instance (make-instance 'uc-2 :y 0))
        (clos-instance (make-instance 'not-a-unit :non-unit-slot 0)))
    ;; CLOS reading:
    (fformat t "~&;; Running CLOS-slot-reading timing test (~:d read~:p)..."
             n)
    (let ((clos-time (brief-timer n (non-unit-slot-of clos-instance))))
      (format-ticks clos-time))
    ;; non-link reading:
    (fformat t "~&;; Running nonlink-slot-reading timing test (~:d read~:p)..."
             n)
    (let ((ui-time (brief-timer n (x-of unit-instance))))
      (format-ticks ui-time))
    ;; link reading:
    (fformat t "~&;; Running link-slot-reading timing test (~:d read~:p)..."
             n)
    (let ((ui-time (brief-timer n (link-2-of unit-instance))))
      (format-ticks ui-time))
    ;; CLOS writing:
    (fformat t "~&;; Running CLOS-slot-writing timing test ~
                     (~:d write~:p)..."
             n)
    (let ((time (brief-timer n (setf (non-unit-slot-of clos-instance) i))))
      (format-ticks time))
    ;; non-link writing:
    (fformat t "~&;; Running nonlink-slot-writing timing test ~
                     (~:d write~:p)..."
             n)
    (let ((time (brief-timer n (setf (x-of unit-instance) i))))
      (format-ticks time))
    ;; link writing:
    (fformat t "~&;; Running linkf timing test (~:d write~:p)..."
             n)
    (let ((time (brief-timer n (linkf (link-2-of unit-instance)
                                      unit-instance))))
      (format-ticks time))))

;;; ---------------------------------------------------------------------------

(defun make-and-link-time-test (n)
  (declare (fixnum n))
  (fformat t "~&;; Running make-instance & link timing test ~
                   (~:d instance~:p)..." 
           n)
  (let ((x (make-instance 'uc-1 :x 0 :y 0)))
    (format-ticks
     (brief-timer n (setf x (make-instance 'uc-2 :x i :y i :backlink-2 x))))))

;;; ---------------------------------------------------------------------------

(defun retrieval-time-test (n *use-marking*)
  (declare (fixnum n))
  (dotimes (i n)
    (declare (fixnum i))
    (find-instances 'uc-2 '(bb sub-bb space-1) 
                    `(=& y ,(expand-point& n 5)))))

;;; ---------------------------------------------------------------------------

#+not-timed
(defun sorted-retrieval-time-test (n *use-marking*)
  (declare (fixnum n))
  (let* ((x (make-instance 'uc-1))
         (dslotd (gbbopen::get-dlslotd-from-reader 'link-2-of x))
         (save-value (direct-link-definition.sort-function dslotd)))
    (setf (direct-link-definition.sort-function dslotd) '<)
    (dotimes (i n)
      (declare (fixnum i))
      (linkf (link-2-of x) (make-instance 'uc-2)))
    (setf (direct-link-definition.sort-function dslotd) save-value)))

;;; ---------------------------------------------------------------------------

(defun marking-find-time-test (n)
  (fformat t "~&;; Running mark-based retrieval timing test ~
                  (~:d instance~:p)..." n)
  (time (retrieval-time-test n 't)))

;;; ---------------------------------------------------------------------------

(defun hashing-find-time-test (n)
  (fformat t "~&;; Running hash-based retrieval timing test ~
                  (~:d instance~:p)..." n)
  (time (retrieval-time-test n nil)))

;;; ---------------------------------------------------------------------------

(defun change-class-time-test (n)
  (fformat t "~&;; Running change-class timing test (~:d instance~:p)..."
           n)
  (let ((uc-2-clone-class (find-class 'uc-2-clone)))
    (flet ((change-classes ()
             (flet ((change-it (instance)
                      (change-class instance uc-2-clone-class)))
               (declare (dynamic-extent #'change-it))
               (map-instances-of-class #'change-it 'uc-2))))
      (format-ticks (brief-timer nil (change-classes))))))

;;; ---------------------------------------------------------------------------

(defun class-based-delete-time-test (n)
  (fformat t "~&;; Running class-based deletion timing test ~
                   (~:d instance~:p)..."
           n)
  (flet ((class-based-delete ()
           (map-instances-of-class
            #'delete-instance 
            '(abstract :plus-subclasses))))
    (format-ticks (brief-timer nil (class-based-delete)))))

;;; ---------------------------------------------------------------------------

(defun space-instance-based-delete-time-test (n)
  (fformat t "~&;; Running space-instance-based deletion timing test ~
                   (~:d instance~:p)..."
           n)
  (flet ((space-instance-based-delete ()
           (map-instances-on-space-instances 
            #'delete-instance 
            't '(bb sub-bb space-1))))
    (format-ticks (brief-timer nil (space-instance-based-delete)))))

;;; ---------------------------------------------------------------------------

(defun do-time-tests (&optional (n *timing-tests-size*))
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Starting timings...~%")
  (map-instances-of-class #'delete-instance '(abstract :plus-subclasses))
  (reset-unit-class 't)
  (make-space-instance 
   '(bb sub-bb space-1) 
   :dimensions (dimensions-of 'uc-1)
   :storage `(((uc-1 :plus-subclasses) y uniform-buckets :layout (0 ,n 10)))
   :make-parents t)
  (make-space-instance 
   '(bb sub-bb space-2) 
   :dimensions (dimensions-of 'uc-1)
   :storage `((uc-2-on-space-2) classification hashed)
   :make-parents t)
  ;; Measure instance creation:
  (make-time-test n)
  ;; Measure instance creation with space insertion onto enumerated storage:
  (progn
    (make-time-test n 'uc-2-on-space-2 "(enumerated storage)")
    (map-instances-of-class #'delete-instance 'uc-2-on-space-2))
  ;; Measure nonlink-slot access:
  (nonlink-slot-access-time-test n)
  ;; Measure change-class:
  (change-class-time-test n)
  ;; Measure class-based deletion:
  (class-based-delete-time-test n)
  ;; Measure instance creation & linking:
  (make-and-link-time-test n)
  ;; Measure  hashing & marking:
  (marking-find-time-test n)
  (hashing-find-time-test n)
  ;; Measure space-instance-based deletion:
  (space-instance-based-delete-time-test n)
  (format t "~&;; ~50,,,'-<-~>~%"))
  
;;; ===========================================================================
;;;  Basic tests

(defun make-space-instances (&optional space-1-storage)
  (make-space-instance '(bb) :allowed-unit-classes nil)
  (make-space-instance '(bb sub-bb) :allowed-unit-classes nil)
  (make-space-instance '(bb sub-bb space-1)
                       :allowed-unit-classes '((uc-1 :plus-subclasses))
		       :dimensions (dimensions-of 'uc-1)
                       :storage space-1-storage)
  (make-space-instance '(bb sub-bb space-2) 
                       :class 'my-space-instance
		       :my-space-slot infinity
		       :dimensions (dimensions-of 'uc-1)
		       :allowed-unit-classes '(uc-1)))

;;; ---------------------------------------------------------------------------

(defun make-test-sets (space-1-storage)
  (delete-all-space-instances)
  (make-space-instances space-1-storage)
  (let ((incomposite-set nil)
	(composite-set nil)
        (incomposite-elements
	 '((u1 uc-1 (1 1 1)  :unknown nil)
	   (u2 uc-1 (1 1 10) :car nil)
	   (u3 uc-1 (1 2 3)  :truck nil)
	   (u4 uc-2 (1 2 3)  :bus nil)
	   (u5 uc-1 (2 1 1)  :car nil)
	   (u6 uc-2 (2 2 2)  :duck-boat 't)
	   (u7 uc-1 ((1 2) (2 5) 4) :unknown nil)
	   (u8 uc-1 ((1. 2) (2 . 5) 4) :unknown nil)
	   (u9 uc-1 (#(1 2) #(2 5) 4) :unknown nil)
	   (u10 uc-1 (#.infinite-interval
		      #.infinite-interval
		      #.infinite-interval) :unknown nil)
	   (u11 uc-1 (5 2 5) :bus nil)
	   (u12 uc-1 (10 2 5) :bus nil)))
	(composite-elements
	 '((cu1 uc-set ((10 15 20) 5 5) (:bus :car) nil))))
    (flet ((make-instances (elements)
             (flet ((fn (element)
                      (destructuring-bind (symbol unit-class-name (x y z) 
                                           classification amphibious?)
                          element
                        (setf (symbol-value symbol)
                              (make-instance unit-class-name
                                :instance-name symbol
                                :x x :y y :z z
                                :classification classification
                                :amphibious amphibious?)))))
               (declare (dynamic-extent #'fn))
               (mapcar #'fn elements))))
      (setq incomposite-set (make-instances incomposite-elements))
      (setq composite-set (make-instances composite-elements)))
    (values incomposite-set composite-set)))

;;; ---------------------------------------------------------------------------
;;;  Make sure that CLOS and MOP are behaving appropriately:

(defvar *method-trail* nil)

(define-class checker-metaclass (standard-class) ())
  
(defmethod validate-superclass ((class checker-metaclass) 
                                (superclass standard-class))
  #+ecl (declare (ignore class superclass))
  't)

(define-class one-slot-instance () 
  ((slot :initform 0))
  (:metaclass checker-metaclass))

(defmethod slot-value-using-class :before ((class checker-metaclass)
                                           instance
                                           slot)
  #+ecl (declare (ignore class))
  (declare (ignore instance slot))
  (push 'slot-value-using-class *method-trail*))

(defmethod (setf slot-value-using-class) :before (nv 
                                                  (class checker-metaclass)
                                                  instance
                                                  slot)
  #+ecl (declare (ignore class))
  (declare (ignore nv instance slot))
  (push 'setf-slot-value-using-class *method-trail*))

;;; ---------------------------------------------------------------------------

(defun clos/mop-tests ()
  (format t "~&;; Running basic CLOS/MOP tests...~%")
  (let ((instance (make-instance 'one-slot-instance))
        (*method-trail* nil))
    (slot-value instance 'slot)
    (unless (memq 'slot-value-using-class *method-trail*)
      (error "~s did not call ~s"
             'slot-value
             'slot-value-using-class))
    (setf *method-trail* nil)
    (setf (slot-value instance 'slot) 1)
    (unless (memq 'setf-slot-value-using-class *method-trail*)
      (error "~s did not call ~s"
             '(setf slot-value)
             '(setf slot-value-using-class)))))

;;; ---------------------------------------------------------------------------

(defvar *test-link-ptrs* nil)
    
(defun make-ptr-obj (instance value)
  (if *test-link-ptrs*
      (make-instance 'link-ptr-with-value 
        :link-instance instance
        :value value)
      instance))

;;; ---------------------------------------------------------------------------

(defun link-tests ()
  (format t "~&;; Link/unlink tests (using ~
                  ~:[direct pointers~;link-ptr objects~])...~%"
          *test-link-ptrs*)
  (map-instances-of-class #'delete-instance '(abstract :plus-subclasses))
  (reset-unit-class 't)
  (flet ((incorrect-link-slot-value (op slot unit value expected-value)
           (error "Error in ~a: Incorrect ~s link-slot value ~s ~
                 (~s expected) in ~s" 
                  op slot value expected-value unit)))
    (let* ((u2 (make-instance 'uc-1 :x 1 :y 2))
           (u2-ptr-obj (make-ptr-obj u2 2))
	   (u3 (make-instance 'uc-2 :x 2 :y 1))
           (u3-ptr-obj (make-ptr-obj u3 3))
	   (u4 (make-instance 'uc-2 :x 2 :y 2))
           (u4-ptr-obj (make-ptr-obj u4 4))
	   (u5 (make-instance 'uc-2 :x 2 :y 3))
           (u5-ptr-obj (make-ptr-obj u5 5))
	   (u1 (make-instance 'uc-1 :x 1 :y 1 
                              :link-1 u2-ptr-obj :link-2 u3-ptr-obj)))
      ;; check initial link values:
      (let ((expected-value u2-ptr-obj)
	    (value (link-1-of u1)))
	(unless (eq value expected-value)
	  (incorrect-link-slot-value 
	   "singular link initarg" 'link-1 u1 value expected-value)))
      (let ((expected-value (list u3-ptr-obj))
	    (value (link-2-of u1)))
	(unless (equal value expected-value)
	  (incorrect-link-slot-value 
	   "non-singular link initarg" 'link-2 u1 value expected-value)))
      ;; unlink-all values:
      (unlinkf-all (link-1-of u1))
      (let ((expected-value nil)
	    (value (link-1-of u1)))
	(unless (eq value expected-value)
	  (incorrect-link-slot-value 
	   "signular unlinkf-all" 'link-1 u1 value expected-value)))
      (unlinkf-all (link-2-of u1))
      (let ((expected-value nil)
	    (value (link-2-of u1))) 
	(unless (eq value expected-value)
	  (incorrect-link-slot-value 
	   "non-singular unlinkf-all" 'link-2 u1 value expected-value)))      
      ;; empty singular linkf:
      (progn
        (linkf (link-1-of u1) u2-ptr-obj)
        (let ((expected-value u2-ptr-obj)
              (value (link-1-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular linkf" 'link-1 u1 value expected-value)))
        (let ((expected-value u1)
              (value (link-1-of u2)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular linkf (i-value)" 'link-1 u2 value expected-value))))
      ;; singular unlinkf:
      (progn
        (unlinkf (link-1-of u1) u2-ptr-obj)
        (let ((expected-value nil)
              (value (link-1-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular unlinkf" 'link-1 u1 value expected-value)))
        (let ((expected-value nil)
              (value (link-1-of u2)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular unlinkf (i-value)" 'link-1 u2 value expected-value))))
      ;; singular unlinkf-all:
      (progn
        (linkf (link-1-of u1) u2-ptr-obj)
        (unlinkf-all (link-1-of u1))
        (let ((expected-value nil)
              (value (link-1-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular unlinkf-all" 'link-1 u1 value expected-value)))
        (let ((expected-value nil)
              (value (link-1-of u2)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular unlinkf-all (i-value)" 
	     'link-1 u2 value expected-value))))
      ;; non-empty singular linkf:
      (progn
        (linkf (link-1-of u1) u2-ptr-obj)
        (with-error-handling 
            (progn (linkf (link-1-of u1) u1)
                   (error "Uncaught linkf of non-empty singular link slot ~
                           ~s in ~s"
                          'link-1 u1))))
      ;; non-empty singular link-setf:
      (progn
        (unlinkf-all (link-1-of u1))
        (linkf (link-1-of u1) u2-ptr-obj)
        (link-setf (link-1-of u1) u3-ptr-obj)
        (let ((expected-value u3-ptr-obj)
              (value (link-1-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular link-setf" 'link-1 u1 value expected-value)))
        (let ((expected-value u1)
              (value (link-1-of u3)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular link-setf (i-value)" 'link-1 u3 value expected-value)))
        (let ((expected-value nil)
              (value (link-1-of u2)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular link-setf (old i-value)" 'link-1 u2 value expected-value)))
        (unlinkf-all (link-1-of u1)))
      ;; atomic multi-linkf:
      (progn
        (linkf (link-2-of u1) u3-ptr-obj)
        (let ((expected-value (list u3-ptr-obj))
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "atomic multi-linkf" 'link-2 u1 value expected-value)))
	;; atomic multi-unlinkf:
        (unlinkf (link-2-of u1) u3-ptr-obj)
        (let ((expected-value nil)
              (value (link-2-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value
             "atomic multi-unlinkf" 'link-2 u1 value expected-value))))
      ;; atomic multi-unlinkf-all:
      (progn
        (linkf (link-2-of u1) u3-ptr-obj)
        (unlinkf-all (link-2-of u1))
        (let ((expected-value nil)
              (value (link-2-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value
             "atomic multi-unlinkf-all" 'link-2 u1 value expected-value))))
      ;; multi-linkf:
      (progn
        (linkf (link-2-of u1) (list u3-ptr-obj u4-ptr-obj u5-ptr-obj u3 u4))
        (let ((expected-value (list u5-ptr-obj u4-ptr-obj u3-ptr-obj))
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value 
             "multi-linkf" 'link-2 u1 value expected-value)))
        ;; multi-unlinkf:
        (unlinkf (link-2-of u1) (list u3-ptr-obj u5-ptr-obj u3-ptr-obj))
        (let ((expected-value (list u4-ptr-obj))
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-unlinkf" 'link-2 u1 value expected-value)))
        ;; check again, but this time with bare instance pointers first:
        (unlinkf-all (link-2-of u1))
        (linkf (link-2-of u1) (list u3 u4 u5-ptr-obj u4-ptr-obj u3-ptr-obj))
        (let ((expected-value (list u5-ptr-obj u4 u3))
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-linkf (#2)" 'link-2 u1 value expected-value))))
      ;; multi-unlinkf-all:
      (progn
        (linkf (link-2-of u1) (list u3-ptr-obj u4-ptr-obj u5-ptr-obj u3-ptr-obj u4-ptr-obj))
        (unlinkf-all (link-2-of u1))
        (let ((expected-value nil)
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-unlinkf-all" 'link-2 u1 value expected-value))))
      ;; multi-link-setf:
      (progn
        (linkf (link-2-of u1) (list u3-ptr-obj u4-ptr-obj))
        (link-setf (link-2-of u1) (list u4-ptr-obj u5-ptr-obj u4-ptr-obj u5-ptr-obj))
        (let ((expected-value (list u4-ptr-obj u5-ptr-obj))
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-link-setf" 'link-2 u1 value expected-value)))
        ;; check unlinkf-ish behavior:
        (link-setf (link-2-of u1) nil)
        (let ((expected-value nil)
              (value (link-2-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-link-setf" 'link-2 u1 value expected-value))))
      ;; multi-linkf (sorted):
      (progn
        (linkf (link-3-of u1) (list u3-ptr-obj u4-ptr-obj u5-ptr-obj u3 u4-ptr-obj))
        (let ((expected-value (list u3-ptr-obj u4-ptr-obj u5-ptr-obj))
              (value (link-3-of u1)))
          (unless (equal value expected-value)
            (incorrect-link-slot-value
             "multi-linkf (sorted)" 'link-3 u1 value expected-value))))
      ;; empty singular linkf:
      (let ((x (make-instance 'uc-1 :link-1 u1)))
        (let ((expected-value u1)
              (value (link-1-of x)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular linkf (initarg)" 'link-1 x value expected-value)))
        (let ((expected-value x)
              (value (link-1-of u1)))
          (unless (eq value expected-value)
            (incorrect-link-slot-value 
             "singular link-setf (initarg i-value)" 
             'link-1 u1 value expected-value)))
        (delete-instance x))
      ;; cleanup
      (mapc #'delete-instance (list u1 u2 u3 u4 u5))
      ;; double-check
      (let ((instances (find-instances 't 't 't)))
        (when instances
          (error "Cleanup of initial instances failed: ~s." instances))))))

;;; ---------------------------------------------------------------------------

(defun basic-tests ()
  (format t "~&;; Running basic GBBopen tests...~%")
  ;; Cleanup:
  (reset-gbbopen)
  ;; Describe-unit-class check:
  (describe-unit-class 'uc-1)
  ;; Basic space-instance checks:
  (make-space-instances)
  ;; Describe-space-instance and describe-instance checks:
  (let ((space-instance (find-space-instance-by-path '(bb sub-bb space-1))))
    (describe-space-instance space-instance)
    (describe-instance space-instance))
  ;; Basic creation/deletion checks:
  (unless (standard-unit-class.abstract (find-class 'abstract))
    (error "Abstract class was not set"))
  (when (with-error-handling (make-instance 'abstract)
          ;; ECL thinks the with-error-handling returned value is t (the
          ;; default is no values) unless specified explicitly (an ECL bug to
          ;; track down!):
          #+ecl (values))
    (error "Abstract class allowed instance creation"))
  (when (standard-unit-class.abstract (find-class 'uc-1))
    (error "Abstract class was incorrectly set"))
  (let ((x (make-instance 'uc-1 :x 0 :y 0)))
    (when (with-error-handling
              (setf (slot-value x 'link-1) 3))
      (error "Link-slot setf slot-value protection failed: ~s" x))
    ;; The next test will fail if slot-value-using-class isn't called
    ;; by writer methods:
    (when (with-error-handling
              (setf (link-1-of x) 3))
      (error "Link-slot setf accessor protection failed: ~s" x))
    (unless (eq x (find-instance-by-name (instance-name-of x) 'uc-1))
      (error "Unable to find instance: ~s" x))
    (add-instance-to-space-instance
     x (find-space-instance-by-path '(bb sub-bb space-2)))
    (let ((instances (space-instances-of x)))
      (unless (and (= (length instances) 2)
                   (member
                    (find-space-instance-by-path '(bb sub-bb space-2))
                    instances
                    :test #'equal)
                   (member
                    (find-space-instance-by-path '(bb sub-bb space-1))
                    instances
                    :test #'equal))
        (error "Incorrect space-instances-of value.")))
    (remove-instance-from-space-instance
     x (find-space-instance-by-path '(bb sub-bb space-1)))
    (delete-instance x)
    (unless (instance-deleted-p x)
      (error "Deleted unit was not marked as deleted"))
    (when (eq x (find-instance-by-name (instance-name-of x) 'uc-1))
      (error "Deleted unit ~s was retrieved by ~s"
             x 'find-instance-by-name))
    (when (member x (find-instances 'uc-1 (find-space-instances '(bb *)) 't))
      (error "Deleted unit ~s was retrieved by ~s"
             x 'find-instances)))

  ;;; Renamed instance checks:
  (let ((x (make-instance 'uc-1 :instance-name 0 :x 0 :y 0)))
    (setf (instance-name-of x) -1)
    (unless (eq x (find-instance-by-name -1 'uc-1))
      (error "Renamed instance ~s by (setf instance-name-of) was not found ~
              by name."
             x))
    (when (eq x (find-instance-by-name 0 'uc-1))
      (error "Renamed instance ~s by (setf instance-name-of) was still found ~
              by its old name."
             x))
    (setf (slot-value x 'instance-name) -2)
    (unless (eq x (find-instance-by-name -2 'uc-1))
      (error "Renamed instance ~s by (setf slot-value) was not found by name."
             x))
    (when (eq x (find-instance-by-name -1 'uc-1))
      (error "Renamed instance ~s by (setf slot-value) was still found ~
              by its old name."
             x)))   
  
  ;;; Initarg checks:
  (flet ((check-values (class)
           (flet ((incorrect-initarg-value (slot unit)
                    (error "Incorrect ~s initarg value in ~s:" slot unit)))
             (let ((x (make-instance class)))
               (with-changing-dimension-values (x x)
                 (unless (= (x-of x) 1)
                   (incorrect-initarg-value 'x x))
                 (unless (= (y-of x) 2)
                   (incorrect-initarg-value 'y x))
                 (incf (x-of x))
                 (unless (= (x-of x) 2)
                   (incorrect-initarg-value 'x x))
                 (incf (slot-value x 'x)) 
                 (unless (= (x-of x) 3)
                   (incorrect-initarg-value 'x x))
                 (unless (and (slot-boundp x 'link-1)
                              (null (link-1-of x)))
                   (error "Link slot ~s is not initially nil in ~s" 
			  'link-1 x)))
               (delete-instance x)))))
    (check-values 'uc-1)
    (check-values 'uc-2))

  ;;; Link checks (using direct pointers):
  (let ((*test-link-ptrs* nil))
    (link-tests))
  ;;; Link checks (using link-ptr objects):
  (let ((*test-link-ptrs* 't))
    (link-tests))

  ;;; My-space-instance check:
  (unless (= infinity (my-space-slot-of
		       (find-space-instance-by-path '(bb sub-bb space-2))))
    (error "Unable to find infinity in my-space-slot"))

  ;;; Event Function Checks:
  (format t "~&;; Event function tests...~%")
  (defun evfn-noop (event-class &rest args)
    (declare (ignore event-class args))
    nil)
  (add-event-function #'evfn-printv 'my-event)
  (signal-event 'my-event :my-event-arg1 3))

;;; ---------------------------------------------------------------------------

(defun lisp-capability-tests ()
  (format t "~&;; Performing Common Lisp capability checks...~%")
  ;; Check infinity value:
  (flet ((check-infinity-value (symbol value-symbol)
	   (when (= (symbol-value symbol) (symbol-value value-symbol))
	     (format t "~&;; ** ~s is represented as ~s~%"
		     symbol
		     value-symbol))))
    (check-infinity-value 'infinity$ 'most-positive-single-float)
    (check-infinity-value 'infinity$$ 'most-positive-double-float))
  ;; Check infinity printing:
  (let* ((*print-readably* 't)
	 (infinity-string (format nil "~s" infinity)))
    (unless (or (string-equal infinity-string "#@single-float-infinity")
		(string-equal infinity-string "#@double-float-infinity"))
      (format t "~&;; ** Infinity is printed as: ~a~%"
	      infinity-string)))
  ;; Check that a redefined unit-class remains eq:
  (if *skip-redefinition-testing*
      (format t "~&;; ** Skipping class-redefinition tests.~%")
      (let ((old-u1-class (find-unit-class 'uc-1))
            (old-u2-class (find-unit-class 'uc-2)))
        (allow-redefinition
         (eval '(define-unit-class uc-1 (abstract) 
                 ;; Need to explore why the following breaks ECL:
                 (#-ecl redefined)))
         (eval '(define-unit-class uc-2 (uc-1 not-a-unit) 
                 (also-redefined))))
        (unless (and (eq old-u1-class (find-unit-class 'uc-1))
                     (eq old-u2-class (find-unit-class 'uc-2)))
          (format t "~&;; ** Redefined unit-classes are not eq.~%"))))
  ;; Check that writer-method-class is actually called to determine the
  ;; class of writer methods:
  (let* ((unit-class (find-unit-class 'standard-unit-instance))
	 (direct-methods 
          (cond ((fboundp 'specializer-direct-methods)
                 (specializer-direct-methods unit-class))
                (t (format t "~&;; ** ~s is not supported.~%" 
                           'specializer-direct-methods)
                   nil))))
    (unless (flet ((fn (method)
                     (typep method 'gbbopen::nonlink-writer-method)))
              (declare (dynamic-extent #'fn))
              (member-if #'fn direct-methods))
      (format t "~&;; ** ~s is not supported.~%" 
	      'writer-method-class))))

;;; ---------------------------------------------------------------------------

(defun find-tests (space-1-storage)
  ;;; Find/Filter/Map Checks:
  (format t "~&;;     Instance find/filter/map tests using ~s~%" space-1-storage)
  (reset-gbbopen)
  (multiple-value-bind (incomposite-set composite-set)
      (make-test-sets space-1-storage)
    (let* ((test-set (append composite-set incomposite-set))
	   (u1-set (list u1 u2 u3 u5 u7 u8 u9 u10 u11 u12))
	   (all-space-instances (find-space-instances '(*)))
	   (space-1 (find-space-instance-by-path '(bb sub-bb space-1))))
      (mapc #'check-instance-locators all-space-instances)
      (labels
	  ((space-instance-names (space-instances)
	     (if (listp space-instances)
		 (mapcar #'instance-name-of space-instances)
		 (instance-name-of space-instances)))
	   (do-find-tests (use-marking?)
	     (flet ((do-map-test (unit-classes space-instances expected-result)
		      (let ((result nil))
                        (flet ((fn (instance) (push instance result)))
                          (declare (dynamic-extent #'fn))
                          (map-instances-on-space-instances
                           #'fn unit-classes space-instances 
                           :use-marking use-marking?))
			(unless (set-equal result expected-result)
			  (error "Wrong results from ~
                                  map-instances-on-space-instances ~
                                  ~s ~s~_Missing: ~s~_Extra: ~s~
                                  ~_Expected: ~s~_Returned: ~s"
				 unit-classes 
				 (space-instance-names space-instances)
				 (set-difference expected-result result)
				 (set-difference result expected-result)
				 expected-result
				 result))))
		    (do-find-test (unit-classes space-instances pattern 
				   expected-result)
		      (let ((result (find-instances
				     unit-classes space-instances pattern 
				     :use-marking use-marking?)))
			(unless (set-equal result expected-result)
			  (error "Wrong results from find-instances ~
                                  ~s ~s ~s~_Missing: ~s~_Extra: ~s~
                                  ~_Expected: ~s~_Returned: ~s"
				 unit-classes
				 (space-instance-names space-instances)
				 pattern
				 (set-difference expected-result result)
				 (set-difference result expected-result)
				 expected-result
				 result)))))
	       (format t "~&;;     Map tests...~%")
	       (do-map-test 't space-1 test-set)
	       (do-map-test 't '(* space-1) test-set)
	       (do-map-test 'uc-1 space-1 u1-set)
	       (do-map-test '(uc-1 uc-2) space-1 incomposite-set)
	       (format t "~&;;     Find tests...~%")
	       (do-find-test 't all-space-instances 't test-set)
	       (do-find-test 't '(bb *) 't test-set)
	       (do-find-test 't all-space-instances ':all test-set)
	       (do-find-test 'uc-1 (append all-space-instances 
					   all-space-instances) 
			     't
			     u1-set)
	       (do-find-test 't all-space-instances 
			     '(= x 1)
			     (list u1 u2 u3 u4))
	       (do-find-test 't all-space-instances 
			     '(=& x 1)
			     (list u1 u2 u3 u4))
	       (do-find-test 't all-space-instances 
			     '(/= x 1)
			     (list u5 u6 u7 u8 u9 u10 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(> x 2)
			     (list u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(>= x 2)
			     (list u5 u6 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(< x 2)
			     (list u1 u2 u3 u4))
	       (do-find-test 't all-space-instances 
			     '(<= x 2)
			     (list u1 u2 u3 u4 u5 u6 u7 u8 u9))
	       (do-find-test 't all-space-instances 
			     '(starts x 2)
			     (list u5 u6))
	       (do-find-test 't all-space-instances 
			     '(ends x 2)
			     (list u5 u6 u7 u8 u9))
	       (do-find-test 't all-space-instances 
			     '(= (x y) (1 1))
			     (list u1 u2))
	       (do-find-test 't all-space-instances 
			     '(starts (x y) (2 1))
			     (list u5))
	       (do-find-test 't all-space-instances 
			     '(ends (x y) (1 2))
			     (list u3 u4))
	       (do-find-test 't all-space-instances 
			     '(is classification :car)
			     (list u2 u5 cu1))
	       (do-find-test 't space-1
			     '(and (is-eq classification :car) (= x 1))
			     (list u2))
	       (do-find-test 't space-1
			     '(and (is-eq classification :car) (= x 1))
			     (list u2))
	       (do-find-test 't space-1
			     '(and (is classification :car) (= x 1))
			     (list u2))
	       (do-find-test 'uc-1 all-space-instances 
			     '(covers (x y z) ((1 2) (2 3) 4))
			     (list u7 u8 u9 u10))
	       (do-find-test 'uc-1 space-1
			     '(covers (x y z) ((1 . 2) (2 . 3) 4))
			     (list u7 u8 u9 u10))
	       (do-find-test 'uc-1 space-1
			     '(covers (x y z) (#(1 2) #(2 3) 4))
			     (list u7 u8 u9 u10))
	       (do-find-test 't all-space-instances 
			     '(within x (1.5 3))
			     (list u5 u6))
	       (do-find-test 't all-space-instances 
			     '(within x (1.5 . 3))
			     (list u5 u6))
	       (do-find-test 't all-space-instances 
			     '(within x #(1.5 3))
			     (list u5 u6))
	       (do-find-test 't all-space-instances 
			     '(within (x y z) ((1 3) (2 5) (2 4)))
			     (list u3 u4 u6 u7 u8 u9))
	       (do-find-test 't all-space-instances 
			     '(or (<= (x y) (1 2)) (or (>= (x y) (3 . 2))))
			     (list u1 u2 u3 u4 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(not (within (x y) ((1 3) (2 . 5))))
			     (list u1 u2 u5 u10 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     `(within x #.infinite-interval)
			     test-set)
	       (do-find-test 't all-space-instances 
			     '(and (within x (0 5)) (not (= x 2)))
			     (list u1 u2 u3 u4 u7 u8 u9 u11))
	       (do-find-test 't all-space-instances 
			     '(overlaps (x y) ((0 3.0) (3.0 4)))
			     (list u7 u8 u9 u10))
	       (do-find-test 't all-space-instances 
			     '(true amphibious)
			     (list u6))
	       (do-find-test 't all-space-instances 
			     '(false amphibious)
			     (list u1 u2 u3 u4 u5 u7 u8 u9 u10 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(eqv amphibious anything-true)
			     (list u6))
	       (do-find-test 't all-space-instances 
			     '(eqv amphibious nil)
			     (list u1 u2 u3 u4 u5 u7 u8 u9 u10 u11 u12 cu1))
	       (do-find-test 't all-space-instances 
			     '(not (or (within x (0 1)) (within x (6 10))))
			     (list u5 u6 u7 u8 u9 u10 u11))
	       (do-find-test 't all-space-instances 
			     '(and (< x 1) (> x 2))
			     nil)
	       (do-find-test 't all-space-instances 
			     '(and (< z 1) (> z 2))
			     nil)
	       (do-find-test 'uc-set space-1
			     '(= x 10)
			     (list cu1))
	       (do-find-test 't all-space-instances 
			     '(or (= x 2) 
			       (and (eqv amphibious 'yes)
				(false amphibious)))
			     (list u5 u6)))
	     ;; check one pattern-based mapper, just in case:
	     (let ((result nil)
		   (p '(not (within (x y) ((1 . 3) #(2 5))))))
               (flet ((fn (instance) (push instance result)))
                 (declare (dynamic-extent #'fn))
                 (map-instances-on-space-instances
                  #'fn 't all-space-instances 
                  :pattern p 
                  :use-marking use-marking?))
	       (unless (set-equal result (list u1 u2 u5 u10 u11 u12 cu1))
		 (error 
		  "Wrong results from map-instances-on-space-instances ~s"
		  p)))))
	;; use hashing:
	(format t "~&;;   Hashing tests...~%")
	(do-find-tests nil)
	;; use marking:
	(format t "~&;;   Marking tests...~%")
	(do-find-tests 't))
      ;; Filter-instances checks:
      (format t "~&;;   Filter-instance tests...~%")
      (let ((p '(is-equalp classification :car)))
	(unless (set-equal (filter-instances test-set p)
			   (list u2 u5 cu1))
	  (error "Wrong results from filter-instances ~s" p)))
      (let ((p '(is-eq classification :truck)))
	(unless (set-equal (filter-instances (cons u3 test-set) p)
			   (list u3 u3))
	  (error "Wrong results from filter-instances ~s" p)))))
  (format t "~&;;     Done with ~s tests~%" space-1-storage))

;;; ---------------------------------------------------------------------------

(defun event-function-tests ()
  ;; Cleanup and prepare:
  (reset-gbbopen)
  (make-space-instances)
  (remove-all-event-functions 't '(uc-1 :plus-subclasses))
  (map-instances-of-class #'delete-instance '(uc-1 :plus-subclasses))
  (let ((signaled-events nil)
	(u2 (make-instance 'uc-2))
	(u3 (make-instance 'uc-2)))
    (format t "~&;; Running event-function tests...~%")
    (labels
	((evfn-tester (&rest args)
	   (push args signaled-events))
	 (args-match? (expected-args candidate-args)
	   (and (eq (getf expected-args ':instance)
		    (getf candidate-args ':instance))
		(eq (getf expected-args ':slot-name)
		    (let ((slotd (getf candidate-args ':slot)))
		      (when slotd
			(gbbopen::slot-definition-name slotd))))
		(equal (getf expected-args ':current-value)
		       (getf candidate-args ':current-value))
		(equal (getf expected-args ':added-instances)
		       (getf candidate-args ':added-instances))
		(equal (getf expected-args ':removed-instances)
		       (getf candidate-args ':removed-instances))
		(eq (getf expected-args ':directp)
		    (getf candidate-args ':directp))))
	 (check-event (event-name &rest expected-args)
	   (declare (dynamic-extent expected-args))
	   (let ((event (find-class event-name))
		 (signaled-event-args nil))
	     ;; look for the expected event:
	     (setq signaled-events
               (flet ((fn (candidate-event-args)
                        (when (and (eq event (first candidate-event-args))
                                   (args-match? expected-args 
                                                (rest candidate-event-args)))
                          (setq signaled-event-args candidate-event-args))))
                 (declare (dynamic-extent #'fn))
                 (delete-if #'fn signaled-events :count 1)))
	     (unless signaled-event-args
	       (error "Event ~s~{ ~s~} was not signaled.
                      ~_Signaled events: ~s" 
		      event-name
		      expected-args
		      signaled-events))))
	 (check-for-unprocessed-events ()
	   (when signaled-events
	     (error "Unexpected events remain: ~s" signaled-events))))
      ;; events to check:
      (add-event-function  #'evfn-tester 'instance-created-event 'uc-1) 
      (add-event-function  #'evfn-tester 'delete-instance-event 'uc-1) 
      (add-event-function  #'evfn-tester 'instance-deleted-event 'uc-1)
      (add-event-function  #'evfn-tester 'nonlink-slot-updated-event 'uc-1 
			   :slot-name 'x)
      (add-event-function  #'evfn-tester 'link-event 'uc-1 
			   :slot-name 'link-2)
      (add-event-function  #'evfn-tester 'link-event 'uc-2 
			   :slot-name 'backlink-2)
      (add-event-function  #'evfn-tester 'unlink-event 'uc-1 
			   :slot-name 'link-2)
      (add-event-function  #'evfn-tester 'unlink-event 'uc-2 
			   :slot-name 'backlink-2)
      ;; do the checking:
      (format t "~&;;   make-instance events...~%")
      (let ((u1 (make-instance 'uc-1 :link-2 u2)))
	(check-event 'instance-created-event 
		     :instance u1)
	(check-event 'link-event 
		     :instance u1
		     :slot-name 'link-2
		     :current-value (list u2)
		     :added-instances (list u2)
		     :directp 't)	
	(check-event 'link-event 
		     :instance u2
		     :slot-name 'backlink-2
		     :current-value (list u1)
		     :added-instances (list u1)
		     :directp nil)	
	(check-event 'nonlink-slot-updated-event
		     :instance u1
		     :slot-name 'x
		     :current-value 1)	
	(check-for-unprocessed-events)
        (format t "~&;;   nonlink slot-update event...~%")
	(setf (x-of u1) 0)
	(check-event 'nonlink-slot-updated-event 
		     :instance u1
		     :slot-name 'x
		     :current-value 0)
	(check-for-unprocessed-events)
        (format t "~&;;   link events...~%")
	(linkf (link-2-of u1) u3)
	(check-event 'link-event 
		     :instance u1
		     :slot-name 'link-2
		     :current-value (list u3 u2)
		     :added-instances (list u3)
		     :directp 't)	
	(check-event 'link-event 
		     :instance u3
		     :slot-name 'backlink-2
		     :current-value (list u1)
		     :added-instances (list u1)
		     :directp nil)	
	(check-for-unprocessed-events)
        (format t "~&;;   unlink events...~%")
	(unlinkf (link-2-of u1) u2)
	(check-event 'unlink-event 
		     :instance u1
		     :slot-name 'link-2
		     :current-value (list u3)
		     :removed-instances (list u2)
		     :directp 't)	
	(check-event 'unlink-event 
		     :instance u2
		     :slot-name 'backlink-2
		     :current-value nil
		     :removed-instances (list u1)
		     :directp nil)	
	(check-for-unprocessed-events)
        (format t "~&;;   link-ptr events...~%")
        (let ((u3-ptr (make-instance 'link-ptr-with-value 
                        :link-instance u3
                        :value -3)))
          (linkf (link-2-of u1) u3-ptr)
          ;; Exisiting link, so events should not be signaled:
          (check-for-unprocessed-events)
          ;; But check for added link-ptr results
          #+SOON check-them-please
          (format t "~&;;   unlink-ptr events...~%")
          (let ((u2-ptr (make-instance 'link-ptr-with-value
                          :link-instance u2
                          :value -2)))
            (unlinkf (link-2-of u1) u2-ptr)
            #+SOON
            (check-event 'unlink-event 
                         :instance u1
                         :slot-name 'link-2
                         :current-value (list u3-ptr)
                         :removed-instances (list u2-ptr)
                         :directp 't)
            #+SOON
            (check-event 'unlink-event 
                         :instance u2-ptr
                         :slot-name 'backlink-2
                         :current-value nil
                         :removed-instances (list u1)
                         :directp nil)
            (check-for-unprocessed-events)))
        (format t "~&;;   delete-instance events...~%")
        (delete-instance u1)
	(check-event 'delete-instance-event
		     :instance u1)
        (check-event 'unlink-event 
                     :instance u1 
                     :slot-name 'link-2
                     :current-value nil 
                     :removed-instances (list u3)
                     :directp 't)
	(check-event 'unlink-event 
		     :instance u3
		     :slot-name 'backlink-2
		     :current-value nil
		     :removed-instances (list u1)
		     :directp nil)
	(check-event 'instance-deleted-event
		     :instance u1)
        (check-for-unprocessed-events))))
  (remove-all-event-functions 't '(uc-1 :plus-subclasses))
  (values))

;;; ===========================================================================
;;;   Run the tests

(defun all-tests ()
  (unless (confirm-if-blackboard-repository-not-empty-p)
    (return-from all-tests))
  (clos/mop-tests)
  (basic-tests)
  (format t "~&;;~%")
  (event-function-tests)
  ;; Finds:
  (format t "~&;;~%")
  (find-tests '((uc-1 x unstructured)))
  (find-tests '((uc-1 amphibious boolean)))
  (find-tests '((uc-1 classification hashed :size 20)))
  (find-tests '((uc-1 x uniform-buckets :layout (0 10 3))))
  (find-tests '(((uc-1 :no-subclasses) x uniform-buckets :layout (0 10 3))))
  (find-tests '(((uc-1 :plus-subclasses) x uniform-buckets :layout (0 10 3))))
  (find-tests '(((uc-1 uc-2) x uniform-buckets :layout (0 10 3))))
  (find-tests '((uc-1 x uniform-buckets :layout (0 10 3))
		(uc-2 x uniform-buckets :layout (0 10 2.5))))
  (find-tests '((uc-1 x uniform-buckets :layout (0 10 1))))
  (find-tests '((uc-1 (x y) uniform-buckets 
		      :layout ((0 10 3)
                               ;; to be different, start y at 1:
			       (1 10 4)))))
  (find-tests '((uc-1 (x y) uniform-buckets 
		      :layout ((0 10 1)
			       (1 10 1)))))
  (find-tests '((uc-1 (x y) uniform-buckets 
		      :layout ((0 10 .2)
			       (.5 10 .3))))) 
  ;; Check link consistency:
  (check-all-instance-links)
  ;; Time test:
  (reset-gbbopen)
  (do-time-tests *timing-tests-size*)
  (reset-gbbopen)
  ;; Common Lisp capability tests (performed last, as uc-1 and uc-2 unit-class
  ;; definitions are changed during testing):
  (lisp-capability-tests))

;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (all-tests))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


