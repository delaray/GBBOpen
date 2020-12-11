;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/boolean-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri May 28 15:50:00 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Boolean Space-Instance Storage
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
;;;  Boolean storage
;;;
;;;  Still to do: 
;;;    * add support for composite dimensions
;;;    * handle a fully infeasible extent (without a full sweep!)

(define-class boolean-storage (storage)
  ((true-instances 
    :initform (make-keys-only-hash-table-if-supported :test 'eq))
   (false-instances 
    :initform (make-keys-only-hash-table-if-supported :test 'eq))
   (unbound-value-instances
    :initform (make-keys-only-hash-table-if-supported :test 'eq)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defun print-boolean-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (format *trace-output* 
          "~&;; - ~s: Using ~s boolean storage (~s true instance~:p, ~
           ~s false instance~:p)~&"
          't 
          (dimension-names-of storage)
	  (hash-table-count (true-instances-of storage))
	  (hash-table-count (false-instances-of storage))))

;;; ---------------------------------------------------------------------------

(defun do-boolean-add/remove-action (instance storage verbose
				     unbound-value-action
				     true-value-action
				     false-value-action
                                     &optional 
                                     old-dimension-values
                                     dimensions-being-changed)
  (declare (type function unbound-value-action true-value-action
		 false-value-action))
  (when verbose (print-boolean-storage-usage-message storage))
  (dolist (dimension-name (dimension-names-of storage))
    (multiple-value-bind (dimension-value dimension-type 
                          comparison-type
                          composite-type composite-dimension-name)
        (instance-dimension-value instance dimension-name)
      (declare (ignore dimension-type comparison-type composite-dimension-name))
      (when dimensions-being-changed
        (let ((old-dimension-value-acons 
               (assq dimension-name old-dimension-values)))
          ;; Updating a dimension value--process the old dimension value
          ;; instead of the current one:
          (when old-dimension-value-acons
            (setf dimension-value (cdr old-dimension-value-acons)))))
      (flet ((do-a-value (dimension-value)
               (cond
                ;; true value:
                (dimension-value 
                 (funcall true-value-action instance storage))
                ;; false value:
                (t (funcall false-value-action instance storage)))))
        (declare (dynamic-extent #'do-a-value))
        (cond 
         ;; unbound value:
         ((eq dimension-value unbound-value-indicator)
          (funcall unbound-value-action instance storage))
         ;; set composite dimension value:
         ((eq composite-type ':set)
          (map nil #'do-a-value dimension-value))
         ;; sequence composite dimension value:
         ((eq composite-type ':sequence)
          (nyi))
         ;; series composite dimension value:
         ((eq composite-type ':series)
          (nyi))
         ;; incomposite dimension value:
         (t (do-a-value dimension-value)))))))
  
;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
                                    (storage boolean-storage)
                                    verbose)
  (flet ((unbound-value-action (instance storage)
           (setf (gethash instance (unbound-value-instances-of storage)) 't))
         (true-value-action (instance storage)
           (setf (gethash instance (true-instances-of storage)) 't))
         (false-value-action (instance storage)
           (setf (gethash instance (false-instances-of storage)) 't)))
    (declare (dynamic-extent 
              #'unbound-value-action #'true-value-action #'false-value-action))
    (do-boolean-add/remove-action 
        instance storage verbose
        #'unbound-value-action
        #'true-value-action
        #'false-value-action)))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage boolean-storage)
                                         old-dimension-values 
                                         dimensions-being-changed 
                                         verbose)
  (flet ((unbound-value-action (instance storage)
           (remhash instance (unbound-value-instances-of storage)))
         (true-value-action  (instance storage)
           (remhash instance (true-instances-of storage)))
         (false-value-action (instance storage)
           (remhash instance (false-instances-of storage))))
    (declare (dynamic-extent 
              #'unbound-value-action #'true-value-action #'false-value-action))
    (do-boolean-add/remove-action 
        instance storage verbose
        #'unbound-value-action
        #'true-value-action
        #'false-value-action
        old-dimension-values
        dimensions-being-changed)))

;;; ---------------------------------------------------------------------------

(defmethod check-instance-storage-locators ((instance standard-unit-instance)
                                            (storage boolean-storage))
  ;; Assumes that an instance can only have a dimension value that is one of
  ;; true/false/unbound-value at a time:
  (flet
      ((unbound-value-action (instance storage)
         (unless (gethash instance (unbound-value-instances-of storage))
           (inconsistent-instance-locators-error
            instance storage "missing from unbound-value instances"))
         (when (gethash instance (true-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "present in true instances"))
         (when (gethash instance (false-instances-of storage))
           (inconsistent-instance-locators-error
            instance storage "present in false instances")))
       (true-value-action (instance storage)
         (unless (gethash instance (true-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "missing from true instances"))
         (when (gethash instance (unbound-value-instances-of storage))
           (inconsistent-instance-locators-error
            instance storage "present in unbound-value instances"))
         (when (gethash instance (false-instances-of storage))
           (inconsistent-instance-locators-error
            instance storage "present in false instances")))
       (false-value-action (instance storage)
         (unless (gethash instance (false-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "missing from false instances"))
         (when (gethash instance (unbound-value-instances-of storage))
           (inconsistent-instance-locators-error
            instance storage "present in unbound-value instances"))
         (when (gethash instance (true-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "present in true instances"))))
    (declare (dynamic-extent 
              #'unbound-value-action #'true-value-action #'false-value-action))
    (do-boolean-add/remove-action 
        instance storage nil
        #'unbound-value-action
        #'true-value-action
        #'false-value-action)))

;;; ---------------------------------------------------------------------------

(defun determine-boolean-storage-extents (storage 
					  disjunctive-dimensional-extents)
  (let ((extents nil))
    (unless (eq disjunctive-dimensional-extents 't)
      (let ((dimension-names (dimension-names-of storage)))
        (dolist (dimensional-extents disjunctive-dimensional-extents)
          (dolist (dimension-name dimension-names)
            (let ((dimensional-extent 
                   (assq dimension-name dimensional-extents)))
              (when dimensional-extent
                (destructuring-bind (extent-dimension-name
                                     dimension-type . new-extents)
                    dimensional-extent
                  (declare (ignore extent-dimension-name dimension-type))
                  ;; This isn't quite right, we need to remove the clause
                  ;; *AND* not do a full sweep if it is the only one...
                  (unless (equal new-extents '(:infeasible))
                    (dolist (new-extent new-extents)
                      (pushnew new-extent extents :test #'eq))))))))))
    (or extents (values (list 'true 'false) 't))))

;;; ---------------------------------------------------------------------------

(defun do-boolean-map-actions (action storage disjunctive-dimensional-extents
			       verbose)
  ;; disjunctive-dimensional-extents for a boolean can contain unbound-value,
  ;; true, or false:
  (declare (type function action))
  (when verbose (print-boolean-storage-usage-message storage))
  (let ((bucket-count 0))
    (multiple-value-bind (storage-extents full-map-p)
	(determine-boolean-storage-extents 
	 storage disjunctive-dimensional-extents)
      (declare (type list storage-extents))
      (when (or full-map-p
		(memq 'true storage-extents))
	(incf& bucket-count)
	(maphash action (true-instances-of storage)))
      (when (or full-map-p
		(memq 'false storage-extents))
	(incf& bucket-count)
	(maphash action (false-instances-of storage)))
      (when (or full-map-p
		(memq unbound-value-indicator storage-extents))
	(incf& bucket-count)
	(maphash action (unbound-value-instances-of storage)))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage boolean-storage)
                                            disjunctive-dimensional-extents
                                            verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (when (mbr-instance-mark-set-p instance)
             (funcall (the function fn) instance))))
    (declare (dynamic-extent #'do-fn))
    (do-boolean-map-actions
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage boolean-storage)
                                         disjunctive-dimensional-extents
					 verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (funcall (the function fn) instance)))
    (declare (dynamic-extent #'do-fn))
    (do-boolean-map-actions 
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

