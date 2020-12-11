;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/hashed-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Feb 26 07:04:18 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Hashed Space-Instance Storage
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
;;;  06-12-08 Add :size option.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::flag-set-p)))

;;; ---------------------------------------------------------------------------

(defparameter *hashed-test-functions* 
    ;; This order is important, as it is used in determining the most
    ;; liberal test:
    '(eq eql equal equalp))

;;; ===========================================================================
;;;  Hashed storage
;;;
;;;  Still to do: 
;;;    * add support for composite dimensions

(define-class hashed-storage (storage)
  ((bound-instances)                    ; initialized in shared-initialize
   (unbound-value-instances
    :initform (make-keys-only-hash-table-if-supported  :test 'eq)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defun most-general-hash-table-test (test1 test2)
  (cond 
   ;; The tests are the same:
   ((eq test1 test2) test1)
   ;; Test1 is more liberal:
   ((memq test1 (memq test2 *hashed-test-functions*))
    test1)
   ;; Test2 is more liberal:
   (t test2)))

;;; ---------------------------------------------------------------------------

(defun determine-hash-table-test (storage)
  ;; Determines the hash-table test for `storage' by using the stores-classes
  ;; and dimension-names provided in the storage description to look up the
  ;; information from the unit-class object.
  (let ((test nil))
    (dolist (class-spec (stores-classes-of storage))
      (destructuring-bind (stores-class . plus-subclasses)
          class-spec
        (flet
            ((do-class (stores-class plus-subclasses)
               (declare (ignore plus-subclasses))
               (let ((cdvs (standard-unit-class.effective-dimensional-values 
                            stores-class)))
                 (dolist (dimension-name (dimension-names-of storage))
                   (let ((cdv
                          ;; (car (member ...)) with :test & :key often
                          ;; optimizes better than (find ...):
                          (car (member dimension-name cdvs
                                       :test #'eq
                                       :key #'cdv.dimension-name))))
                     (when cdv
                       (let ((dimension-value-type (cdv.dimension-value-type cdv))
                             (comparison-type (cdv.comparison-type cdv)))
                         (unless (eq ':element dimension-value-type)
                           (error "Dimension ~s is not an :enumerated ~
                                   dimension."
                                  dimension-name))
                         (cond 
                          ;; We have a test already, determine the most
                          ;; liberal test:
                          (test
                           (setf test (most-general-hash-table-test
                                       test comparison-type)))
                          ;; Set the test:
                          (t (setf test comparison-type))))))))))
          (declare (dynamic-extent #'do-class))
          (if plus-subclasses
              (map-unit-classes #'do-class stores-class)
              (do-class stores-class nil)))))
    test))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((storage hashed-storage)
                                     slot-names
                                     &key (size 0 size-specified-p))
  (declare (ignore slot-names))
  (let ((determined-test (determine-hash-table-test storage)))
    (unless determined-test
      (internal-error "Unable to determine the hash-table test for ~s"
                      storage))
    (setf (bound-instances-of storage)
          (if size-specified-p
              (make-hash-table :size size
                               :test determined-test)
              (make-hash-table :test determined-test)))))
  
;;; ---------------------------------------------------------------------------

(defun print-hashed-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (let ((hash-table (bound-instances-of storage)))
    (format *trace-output* 
	    "~&;; - ~s: Using ~s ~s hashed storage (~s instance~:p)~&"
	    't 
            (hash-table-test hash-table)
	    (dimension-names-of storage)
	    (hash-table-count hash-table))))

;;; ---------------------------------------------------------------------------

(defun do-hashed-add/remove-action (instance storage verbose
				    unbound-value-action
				    bound-value-action
                                    &optional 
                                    old-dimension-values
                                    dimensions-being-changed)
  (declare (type function unbound-value-action bound-value-action))
  (when verbose (print-hashed-storage-usage-message storage))
  (dolist (dimension-name (dimension-names-of storage))
    (multiple-value-bind (dimension-value dimension-type 
                          comparison-type
                          composite-type composite-dimension-name)
        (instance-dimension-value instance dimension-name)
      (declare (ignore dimension-type comparison-type 
                       composite-dimension-name))
      (when dimensions-being-changed
        (let ((old-dimension-value-acons 
               (assq dimension-name old-dimension-values)))
          ;; Updating a dimension value--process the old dimension value
          ;; instead of the current one:
          (when old-dimension-value-acons
            (setf dimension-value (cdr old-dimension-value-acons)))))
      (flet ((do-a-value (dimension-value)
               (funcall bound-value-action instance storage dimension-value)))
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
                                    (storage hashed-storage)
                                    verbose)
  (flet
      ((unbound-value-action (instance storage)
         (setf (gethash instance (unbound-value-instances-of storage)) 't))
       (bound-value-action (instance storage dimension-value)
         (pushnew instance
                  (gethash dimension-value (bound-instances-of storage))
                  :test #'eq)))
    (declare (dynamic-extent #'unbound-value-action #'bound-value-action))
    (do-hashed-add/remove-action 
        instance storage verbose #'unbound-value-action #'bound-value-action)))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage hashed-storage)
                                         old-dimension-values 
                                         dimensions-being-changed
                                         verbose)
  (flet 
      ((unbound-value-action (instance storage)
         (remhash instance (unbound-value-instances-of storage)))
       (bound-value-action (instance storage dimension-value)
         (let ((storage (bound-instances-of storage)))
           (setf (gethash dimension-value storage)
                 (delq-one instance (gethash dimension-value storage))))))
    (declare (dynamic-extent #'unbound-value-action #'bound-value-action))    
    (do-hashed-add/remove-action
        instance storage verbose #'unbound-value-action #'bound-value-action
        old-dimension-values dimensions-being-changed)))

;;; ---------------------------------------------------------------------------

(defmethod check-instance-storage-locators ((instance standard-unit-instance)
                                            (storage hashed-storage))
  (flet 
      ((unbound-value-action (instance storage)
         (unless (gethash instance (unbound-value-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "missing from unbound-value instances"))
         ;; Check that it's not in any bound-value bucket:
         (flet ((check-for-it (the-dimension-value instances)
                  (when (memq instance instances)
                    (inconsistent-instance-locators-error 
                     instance storage 
                     (format nil "present in ~s bound-value instances"
                             the-dimension-value)))))
           (declare (dynamic-extent #'check-for-it))
           (maphash #'check-for-it (bound-instances-of storage))))
       (bound-value-action (instance storage dimension-value)
         (unless (memq instance
                       (gethash dimension-value (bound-instances-of storage)))
           (inconsistent-instance-locators-error
            instance storage "missing from bound-value instances"))
         ;; Check that it's not in any other bound-value bucket:
         ;; NOTE: Support for composite extents will require changes here...
         (let* ((bound-instances-hash-table (bound-instances-of storage))
                (hash-table-test (hash-table-test bound-instances-hash-table)))
           (flet ((check-for-it (the-dimension-value instances)
                    ;; skip the correct dimension-value entry:
                    (unless (funcall hash-table-test 
                                     dimension-value the-dimension-value)
                      (when (memq instance instances)
                        (inconsistent-instance-locators-error 
                         instance storage 
                         (format nil "present in ~s bound-value instances"
                                 the-dimension-value))))))
             (declare (dynamic-extent #'check-for-it))
             (maphash #'check-for-it bound-instances-hash-table)))
         ;; Check that it's not in the unbound-value instances:
         (when (gethash instance (unbound-value-instances-of storage))
           (inconsistent-instance-locators-error 
            instance storage "present in unbound-value instances"))))
    (declare (dynamic-extent #'unbound-value-action #'bound-value-action))
    (do-hashed-add/remove-action 
        instance storage nil #'unbound-value-action #'bound-value-action)))

;;; ---------------------------------------------------------------------------

(defun determine-hashed-storage-extents (storage 
					 disjunctive-dimensional-extents)
  (let ((extents nil))
    (cond 
     ((eq disjunctive-dimensional-extents 't)
      (values nil 't))     
     (t (let ((dimension-names (dimension-names-of storage)))
          (dolist (dimensional-extents disjunctive-dimensional-extents)
            (dolist (dimension-name dimension-names)
              (let ((dimensional-extent
                     (assq dimension-name dimensional-extents)))
                (when dimensional-extent
                  (destructuring-bind (extent-dimension-name
                                       dimension-type . new-extents)
                      dimensional-extent
                    (declare (ignore extent-dimension-name dimension-type))
                    ;; Support for composite extents (listed) is still needed:
                    (pushnew new-extents extents :test #'eq)))))))
        (or extents
	    (values nil 't))))))

;;; ---------------------------------------------------------------------------

(defun do-hashed-map-actions (action storage disjunctive-dimensional-extents
			      verbose)
  ;; disjunctive-dimensional-extents for a hashed storage can contain
  ;; unbound-value or one or more bound values:
  (declare (type function action))
  (when verbose (print-hashed-storage-usage-message storage))
  (let ((bucket-count 0)
	(bound-instances-hash-table (bound-instances-of storage)))
    (multiple-value-bind (storage-extents full-map-p)
	(determine-hashed-storage-extents 
	 storage disjunctive-dimensional-extents)
      (declare (type list storage-extents))
      (flet ((map-bucket (key bucket)
	       (declare (ignore key))
	       (incf& bucket-count)
	       (dolist (instance bucket)
		 (funcall action instance 't))))
        (declare (dynamic-extent #'map-bucket))
	(cond (full-map-p
	       (maphash #'map-bucket bound-instances-hash-table))		
	      (t (dolist (storage-extent storage-extents)
		   (map-bucket storage-extent 
			       (gethash storage-extent 
					bound-instances-hash-table))))))
      (when (or full-map-p
		(memq unbound-value-indicator storage-extents))
	(incf& bucket-count)
	(maphash action (unbound-value-instances-of storage)))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage hashed-storage)
                                            disjunctive-dimensional-extents
                                            verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (when (mbr-instance-mark-set-p instance)
             (funcall (the function fn) instance))))
    (declare (dynamic-extent #'do-fn))
    (do-hashed-map-actions
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage hashed-storage)
                                         disjunctive-dimensional-extents
					 verbose)
  (flet ((do-fn (instance value)
           (declare (ignore value))
           (funcall (the function fn) instance)))
    (declare (dynamic-extent #'do-fn))
    (do-hashed-map-actions 
        #'do-fn storage disjunctive-dimensional-extents verbose)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

