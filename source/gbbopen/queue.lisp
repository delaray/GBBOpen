;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/queue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Sep  2 04:56:20 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         Doubly-Linked Queue Mixin and Manipulation Functions
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
;;; These queue mixins are intended for fairly heavy-weight, synchronized
;;; queue operations (such as control-shell agenda management).
;;;
;;; Limitation: A queue-element object can not reside on more than one queue
;;;             at a time.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-07-04 File created.  (Corkill)
;;;  03-15-04 Added unordered queue.  (Corkill)
;;;  03-21-04 Added ON-QUEUE-P.  (Corkill)
;;;  08-20-06 Added DO-QUEUE syntactic sugar.  (Corkill)
;;;  09-02-12 Added CLEAR-QUEUE and missing delete-instance queue method.
;;;           (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :portable-threads))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clear-queue                 ; to be documented soon 
            do-queue
            first-queue-element
            insert-on-queue
            last-queue-element
            ordered-queue
            make-queue
            map-queue
            next-queue-element
            nth-queue-element
            on-queue-p
            previous-queue-element
            queue-element
            queue
            queue-length
            remove-from-queue
            show-queue)))

;;; ---------------------------------------------------------------------------

(defgeneric clear-queue (queue))
(defgeneric first-element-of (queue))
(defgeneric first-queue-element (queue))
(defgeneric insert-on-queue (element queue))
(defgeneric last-queue-element (queue))
(defgeneric map-queue (fn queue))
(defgeneric next-queue-element (queue))
(defgeneric nth-queue-element (n queue))
(defgeneric previous-queue-element (queue))
(defgeneric queue-length (queue &optional recount-p))
(defgeneric remove-from-queue (element))
(defgeneric show-queue (queue &key start end show-element-function))

;;; ===========================================================================
;;;   Doubly-Linked Queue Class Definitions

(define-unit-class queue-pointers ()
  ;;; Queue-pointers is a common abstract class for queue and queue-element
  ;;; classes
  ((next
    :link (queue-pointers previous :singular t)
    :singular t
    :accessor queue.next
    :initform nil)
   (previous
    :link (queue-pointers next :singular t)
    :singular t
    :accessor queue.previous
    :initform nil)
   (header
    :reader on-queue-p
    :writer (setf queue.header)
    :initform nil))
  (:generate-accessors nil)
  (:abstract t))

;;; ---------------------------------------------------------------------------

(define-unit-class queue (queue-pointers)
  ((lock
    ;; This lock is recursive to allow things like:
    ;;   (map-queue #'delete-instance ..), etc.
    :initform (make-recursive-lock :name "Queue Lock")
    :reader queue.lock)
   ;; Note: queue length is not protected against errors that might 
   ;;       occur during insertion/deletion.  Therefore, it is possible
   ;;       that the length value becomes inaccurate.  This value is
   ;;       not used directly in any queue code, so this is only an
   ;;       annoyance rather than a fatal problem...
   (length :initform 0
           :type fixnum))
  (:generate-accessors-format :prefix)
  (:generate-accessors t :exclude lock)
  (:generate-initargs t :exclude lock))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((queue queue)
                                       &key)
  (setf (queue.header queue) queue)
  (linkf (queue.next queue) queue))

;;; ---------------------------------------------------------------------------
;;; Remove all queue-elements from a deleted queue:

(defmethod delete-instance ((queue queue))
  (clear-queue queue)
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defmethod omitted-slots-for-saving/sending ((queue queue))
  #+ecl (declare (ignore queue))
  (list* 'lock 'length (call-next-method)))

;;; ===========================================================================
;;;  Ordered-queue

(define-unit-class ordered-queue (queue)
  ((key
    :initform #+sbcl 'identity
              #-sbcl #'identity
    :reader ordered-queue.key)
   (test
    :initform #+sbcl '>
              #-sbcl #'>
    :reader ordered-queue.test))
  (:generate-accessors nil))

;;; ===========================================================================
;;;  Queue-element

(define-unit-class queue-element (queue-pointers)
  ())

;;; Ensure that any deleted queue-elements are removed from their lists:
(defmethod delete-instance ((element queue-element))
  (remove-from-queue element)
  (call-next-method))

;;; ---------------------------------------------------------------------------
;;; Syntactic sugar for making a queue...

(defun make-queue (&rest initargs 
                   &key (class 'queue) &allow-other-keys)
  (declare (dynamic-extent initargs))
  (assert (subtypep class 'queue) (class)
    "Class ~s is not a subclass of ~s."
    class
    'queue)
  (apply #'make-instance class (remove-property initargs :class)))

;;; --------------------------------------------------------------------------

(defmethod insert-on-queue ((element queue-element) (queue queue))
  ;;; Inserts `element' at the end of an unordered queue.
  (let ((current-queue (on-queue-p element)))
    (when current-queue
      (cerror "Remove it from ~:*~s and then perform the insertion."
              "~s is already on queue ~s."
              element
              current-queue)
      (remove-from-queue element)))
  (with-lock-held ((queue.lock queue))
    ;; Do the insertion:
    (let ((previous (queue.previous queue)))
      (link-setf (queue.previous element) previous)
      (link-setf (queue.next element) queue))
    (setf (queue.header element) queue)
    (incf& (queue.length queue)))
  ;; Return the element:
  element)

;;; --------------------------------------------------------------------------

(defmethod insert-on-queue ((element queue-element) 
                            (queue ordered-queue))
  ;;; Inserts `element' at the proper place in an ordered queue based on the
  ;;; test associated with the queue (the default test is #'>, which yields a
  ;;; numerically decreasing queue).
  (let ((current-queue (on-queue-p element)))
    (when current-queue
      (cerror "Remove it from ~:*~s and then perform the insertion."
              "~s is already on queue ~s."
              element
              current-queue)
      (remove-from-queue element)))
  (with-lock-held ((queue.lock queue))
    (let* ((ptr (queue.next queue))
           (key (ordered-queue.key queue))
           (test (ordered-queue.test queue))
           (element-value (funcall key element)))
      ;; Find the proper position (ptr immediately after element position):
      (until (or
              ;; no more elements
              (eq ptr queue)
              ;; test is true
              (funcall test element-value (funcall key ptr)))
        (setf ptr (queue.next ptr)))
      ;; Do the insertion:
      (let ((previous (queue.previous ptr)))
        (link-setf (queue.previous element) previous)
        (link-setf (queue.next element) ptr))
      (setf (queue.header element) queue)
      (incf& (queue.length queue))))
  ;; Return the element:
  element)

;;; ---------------------------------------------------------------------------

(defmethod remove-from-queue ((element queue-element))
  ;;; Removes `element' from the queue on which it resides:
  (let ((queue (on-queue-p element)))
    (when queue                  ;; do nothing, if `element' is not on a queue
      (with-lock-held ((queue.lock queue))
        (let ((next (queue.next element))
              (previous (queue.previous element)))
          (link-setf (queue.next previous) next))
        (decf& (queue.length queue))
        (setf (queue.header element) nil)))
    ;; Return the element
    element))

;;; ---------------------------------------------------------------------------

(defmethod clear-queue ((queue queue))
  ;;; Rapidly remove all elements from `queue':
  (flet ((remove-it (element)
           (setf (queue.next element) nil)
           (setf (queue.previous element) nil)
           (setf (queue.header element) nil)))
    (declare (dynamic-extent #'remove-it))
    (let ((*%%allow-setf-on-link%%* 't))
      (map-queue #'remove-it queue)
      (setf (queue.length queue) 0)
      (setf (queue.next queue) queue)
      (setf (queue.previous queue) queue))))

;;; ---------------------------------------------------------------------------

(defmethod first-queue-element ((queue queue))
;;;  Returns the first element in `queue' or nil if the queue is empty
  (with-lock-held ((queue.lock queue))
    (let ((next (queue.next queue)))
      (unless (eq queue next) next))))

;;; ---------------------------------------------------------------------------

(defmethod next-queue-element ((element queue-element))
;;;  Returns the element following `element' in a queue or nil the element
;;;  is the last
  (let ((queue (on-queue-p element)))
    (with-lock-held ((queue.lock queue))
      (let ((next (queue.next element)))
        (unless (eq queue next) next)))))

;;; ---------------------------------------------------------------------------

(defmethod previous-queue-element ((element queue-element))
;;;  Returns the element that precedes  `element' in a queue or nil the
;;;  element is the first
  (let ((queue (on-queue-p element)))
    (with-lock-held ((queue.lock queue))
      (let ((previous (queue.previous element)))
        (unless (eq queue previous) previous)))))

;;; ---------------------------------------------------------------------------

(defmethod nth-queue-element ((n #-(or clisp ecl) fixnum 
                                 ;; CLISP & ECL don't have a fixnum class:
                                 #+(or clisp ecl) integer)
                              (queue queue))
;;;  Returns the nth element in `queue' or nil if the queue is shorter than
;;;  `n'.  If `n' is negative, return the nth element counting backward
;;;  from the end of the queue (one origin)
  (with-lock-held ((queue.lock queue))
    (let ((ptr queue))
      (if (minusp& n)
          ;; From the end
          (dotimes (i (-& n))
            (declare (fixnum i))
            (setf ptr (queue.previous ptr))
            (when (eq queue ptr)
              (return-from nth-queue-element nil)))
          ;; From the start
          (dotimes (i (1+& n))
            (declare (fixnum i))
            (setf ptr (queue.next ptr))
            (when (eq queue ptr)
              (return-from nth-queue-element nil))))
      ptr)))

;;; ---------------------------------------------------------------------------

(defmethod last-queue-element ((queue queue))
;;;  Returns the last element in `queue' or nil if the queue is empty
  (with-lock-held ((queue.lock queue))
    (let ((previous (queue.previous queue)))
      (unless (eq queue previous) previous))))

;;; --------------------------------------------------------------------------

(defmethod map-queue (fn (queue queue))
  ;;; Applys `fn' to each element in `queue' queue order.
  (with-lock-held ((queue.lock queue))
    (let ((ptr (queue.next queue))
          element)
      (until (eq ptr queue)
        ;; set pointers before calling `fn', in case the function deletes the
        ;; element!
        (setf element ptr)
        (setf ptr (queue.next ptr))
        (funcall fn element)))))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-queue ((var queue) &body body)
    ;; Do-xxx version of map-queue:
    (with-once-only-bindings (queue)
      (with-gensyms (elt ptr)
        `(with-lock-held ((queue.lock ,queue))
           (let ((,ptr (queue.next ,queue))
                 ,elt)
             (until (eq ,ptr ,queue)
               ;; set pointers before calling `fn', in case the codebody
               ;; deletes the element!
               (setf ,elt ,ptr)
               (setf ,ptr (queue.next ,ptr))
               (let ((,var ,elt)) ,@body))))))))

;;; --------------------------------------------------------------------------

(defmethod queue-length ((queue queue) &optional recount-p)
  ;;; Returns the number of elements in `queue'.  If `recount-p' is true,
  ;;; the length is recounted and updated.
  (if recount-p
      (let ((count 0))
        (do-queue (element queue)
          (declare (ignore element))
          (incf& count))
        (setf (queue.length queue) count))
      (queue.length queue)))

;;; --------------------------------------------------------------------------

(defmethod show-queue ((queue queue)
                       &key (start 0) end 
                            (show-element-function
                             'standard-show-queue-element))
  ;;; Displays the contents of `queue', limited by `start' and `end'.
  ;;; Each element is displayed by `show-element-function'.
  (cond
   ((not (first-queue-element queue))
    (format t "~&The queue is empty.~%"))
   (t (let ((ptr (nth-queue-element start queue))
            (index start))
        (while (and ptr (or (null end) (>& end index)))
          (funcall show-element-function index ptr)
          (incf& index)
          (setf ptr (next-queue-element ptr))))))
  (values))

;;; --------------------------------------------------------------------------

(defun standard-show-queue-element (index element)
  (format t "~&~5d.~5t~s~%" index element))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
