;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/unstructured-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun May  9 01:43:23 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Unstructured Space-Instance Storage
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-23-06 Split out from storage.lisp.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::flag-set-p)))

;;; ===========================================================================
;;;  Unstructured storage

(define-class unstructured-storage (storage)
  ((instances :initform (make-keys-only-hash-table-if-supported :test 'eq)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defun print-unstructured-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (format *trace-output* 
          "~&;; - ~s: Using ~s unstructured storage (~s instance~:p)~&"
          't 
          (dimension-names-of storage)
          (hash-table-count (instances-of storage))))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
                                    (storage unstructured-storage)
                                    verbose)
  (when verbose (print-unstructured-storage-usage-message storage))
  (setf (gethash instance (instances-of storage)) 't))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage unstructured-storage)
                                         old-dimension-values 
                                         dimensions-being-changed verbose)
  (declare (ignore old-dimension-values dimensions-being-changed))
  (when verbose (print-unstructured-storage-usage-message storage))
  (remhash instance (instances-of storage)))

;;; ---------------------------------------------------------------------------

(defmethod check-instance-storage-locators ((instance standard-unit-instance)
                                            (storage unstructured-storage))
  (unless (gethash instance (instances-of storage))
    (inconsistent-instance-locators-error instance storage "missing")))

;;; ---------------------------------------------------------------------------

(defun do-unstructured-map-actions (fn storage 
				    disjunctive-dimensional-extents verbose)
  (declare (ignore disjunctive-dimensional-extents))
  (declare (type function fn))
  (when verbose (print-unstructured-storage-usage-message storage))  
  (maphash fn (instances-of storage))
  ;; record the bucket count:
  (let ((find-stats *find-stats*))
    (when find-stats 
      (incf (find-stats.bucket-count find-stats)))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage unstructured-storage)
                                            disjunctive-dimensional-extents
                                            verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (when (mbr-instance-mark-set-p instance)
             (funcall (the function fn) instance))))
    (declare (dynamic-extent #'do-fn))
    (do-unstructured-map-actions 
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage unstructured-storage)
                                         disjunctive-dimensional-extents
					 verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (funcall (the function fn) instance)))
    (declare (dynamic-extent #'do-fn))
    (do-unstructured-map-actions 
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

