;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:08:09 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   Space-Instance Storage Management
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
;;;  11-18-03 File created.  (Corkill)
;;;  03-17-04 Fix applicable-storage-object-p.  (Corkill)
;;;  05-04-04 Converted unstructured storage to use an eq hash table rather
;;;           than a simple list.  (Corkill)
;;;  06-08-04 Add excess-locator counting.  (Corkill)
;;;  06-08-04 Add unit-classes-spec support.  (Corkill)
;;;  05-27-05 Change storage-objects-for-retrieval to allow space-instance-path
;;;           regexps. (Corkill)
;;;  02-20-06 Add hashed storage support.  (Corkill)
;;;  04-23-06 Split out individual storage mechanisms.  (Corkill)
;;;  06-11-07 Converted storage accessors from :prefix to modern "-of"
;;;           format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(boolean
	    describe-space-instance-storage
            describe-storage                ; not yet documented
	    hashed
	    quadtree
	    uniform-buckets
	    unstructured)))

;;; ===========================================================================
;;;   Storage objects
;;;

(define-class storage ()
  ((space-instance)
   (stores-classes)
   (dimension-names :initform 't)
   (stores-all-instances :initform 't)   
   (excess-locators :initform 0)
   (instance-counts :initform nil)))

;;; ---------------------------------------------------------------------------

(defun pretty-unit-classes-spec (unit-classes-spec)
  ;;; Return a pretty (legal) list representation for `unit-classes-spec' with 
  ;;; class-names rather than class objects and :plus-subclasses only when 
  ;;; needed.  Often used with the format-control-string "~{~{~s~^~@[+~*~]~}~}".
  (flet ((fn (class-spec)
           (destructuring-bind (unit-class . plus-subclasses)
               class-spec
             (let ((class-name (class-name unit-class)))
               (if (and (eq class-name 'standard-unit-instance)
                        plus-subclasses)
                   '(t)
                   `(,class-name ,@(when plus-subclasses '(:plus-subclasses))))))))
    (declare (dynamic-extent #'fn))
    (mapcar #'fn unit-classes-spec)))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((storage storage) stream)
  (cond 
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (storage stream :type nil)
	(let ((classes (when (slot-boundp storage 'stores-classes)
			 (stores-classes-of storage))))
	  (format stream "~:(~s~) ~s ~:[???~;(~:*~{~{~s~^~@[+~*~]~}~})~] ~s ~s"
		  (type-of storage)
                  (let ((space-instance (space-instance-of storage)))
                    (if space-instance 
                        (instance-name-of space-instance) 
                        "???"))
		  (pretty-unit-classes-spec classes)
		  (and (slot-boundp storage 'dimension-names)
		       (dimension-names-of storage))
                  (if (slot-boundp storage 'instance-counts)
                      (let ((instance-counts (instance-counts-of storage)))
                        (if instance-counts
                            (flet ((fn (a b) (+& a b)))
                              (declare (dynamic-extent #'fn))
                              (reduce #'fn instance-counts :key #'cdr))
                            0))
                      0))))
      ;; Print-object must return object:
      storage)))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage :after ((instance standard-unit-instance)
					   storage
					   verbose)
  (declare (ignore verbose))
  (let ((unit-class-name (type-of instance)))
    (pushnew/incf-acons unit-class-name 1 (instance-counts-of storage)
                        :test #'eq)))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage :after 
	   ((instance standard-unit-instance)
	    storage old-dimension-values dimensions-being-changed verbose)
  (declare (ignore old-dimension-values dimensions-being-changed verbose))
  ;; Decf/delete the instance-count:
  (decf/delete-acons (type-of instance) 1 (instance-counts-of storage)
                     :test #'eq))

;;; ---------------------------------------------------------------------------

(defun storage-objects-for-add/move/remove (unit-class space-instance)
  ;;; Finds all storage objects for instances of `unit-class' on 
  ;;; `space-instance'.  Used for add/move/remove unit-instance operations.
  (let ((result nil)
	(plus-subclasses-storage nil)
        (unit-class-name (type-of unit-class)))
    (dolist (storage (standard-space-instance.%%storage%% space-instance))
      (dolist (class-spec (stores-classes-of storage))
	(destructuring-bind (stores-class . plus-subclasses)
	    class-spec
	  (cond 
	   ;; always include exact class matches:
	   ((eq unit-class stores-class)
	    (push storage result))
	   ;; also include first ("best") plus-subclasses match:
	   ((and (not plus-subclasses-storage)
		 plus-subclasses
                 (subtypep unit-class-name (type-of stores-class)))
	    (push storage plus-subclasses-storage))))))
    (or result
	plus-subclasses-storage
	(internal-error "Unable to find storage for ~s on ~s."
			unit-class space-instance))))

;;; ---------------------------------------------------------------------------
;;;   Storage object lookup

(defun determine-retrieval-dimensions (space-instance 
				       disjunctive-dimensional-extents)
  ;;; Returns the dimensional intersection of `space-instance' and
  ;;; `disjunctive-dimensional-extents'
  (let ((space-instance-dimensions (dimensions-of space-instance)))
    (declare (type list space-instance-dimensions))
    (if (eq disjunctive-dimensional-extents 't)
	space-instance-dimensions 
	(let ((result nil))
	  (dolist (dimensional-extents disjunctive-dimensional-extents)
	    (dolist (extent dimensional-extents)
	      (let ((dimension (car extent)))
		(when (member dimension space-instance-dimensions 
			      :key #'car :test #'eq)
		  (pushnew dimension result)))))
	  result))))
    
;;; ---------------------------------------------------------------------------

(defun applicable-storage-object-p (storage unit-class plus-subclasses)
  ;;; Returns true if the storage object `storage' is relevant given
  ;;; `unit-class' and `plus-subclasses'
  (declare (inline class-name))
  (let* ((unit-class-name (class-name unit-class))
         (stores-classes (stores-classes-of storage)))
    ;; `storage' is a candidate if it:
    (flet ((fn (class-spec)
             (declare (inline class-name))
             (destructuring-bind (stores-class . stores-subclasses-p)
                 class-spec
               (or
                 ;; * stores the unit class:
                 (eq unit-class stores-class)
                 ;; * stores a parent class plus subclasses:
                 (and stores-subclasses-p
                      (subtypep unit-class-name (class-name stores-class)))
                 ;; * subclasses are desired (:plus-subclasses was specified for
                 ;;   retrieval) and `storage' stores a subclasses of
                 ;;   `unit-class':
                 (and plus-subclasses 
                      (subtypep (class-name stores-class) unit-class-name))))))
      (declare (dynamic-extent #'fn))
      (member-if #'fn stores-classes))))

;;; ---------------------------------------------------------------------------

(defun best-retrieval-storage (space-instance unit-class-specs
                               retrieval-dimensions result-so-far)
  (declare (ignore retrieval-dimensions))
  (dolist (storage (standard-space-instance.%%storage%% space-instance))
    (dolist (unit-class-spec unit-class-specs)
      (destructuring-bind (unit-class . plus-subclasses)
          unit-class-spec
        (when (applicable-storage-object-p 
               storage unit-class plus-subclasses)
          (pushnew storage result-so-far)))))
  result-so-far)
	
;;; ---------------------------------------------------------------------------

(defun storage-objects-for-mapping (unit-classes-spec space-instances
                                    invoking-fn-name)
  ;;; Returns the best storage objects for mapping instances of
  ;;; `unit-classes-spec' on `space-instances'
  (when space-instances
    (let ((result nil)
          (unit-class-specs (parse-unit-classes-specifier unit-classes-spec)))
      (flet ((do-si (space-instance)
               (when (allowed-unit-classes-of space-instance)
                 (setf result (best-retrieval-storage
                               space-instance unit-class-specs
                               nil result)))))
        (declare (dynamic-extent #'do-si))
        (cond ((eq space-instances 't)
               (map-space-instances #'do-si '(*) invoking-fn-name))
              (t (map-space-instances
                  #'do-si
                  (ensure-list space-instances)
                  invoking-fn-name))))
      result)))

;;; ---------------------------------------------------------------------------

(defun storage-objects-for-retrieval (unit-classes-spec space-instances
				      disjunctive-dimensional-extents
				      pattern invoking-fn-name)
  ;;; Returns the best storage objects for retrieving instances of 
  ;;; `unit-classes-spec' on `space-instances' given 
  ;;; `disjunctive-dimensional-extents'
  (when space-instances
    (let ((result nil)
          (unit-class-specs (parse-unit-classes-specifier unit-classes-spec)))
      (flet ((do-si (space-instance)
               (when (allowed-unit-classes-of space-instance)
                 (let ((retrieval-dimensions
                        (determine-retrieval-dimensions 
                         space-instance 
                         disjunctive-dimensional-extents)))
                   (when retrieval-dimensions
                     (setf result 
                           (best-retrieval-storage
                            space-instance unit-class-specs
                            retrieval-dimensions result)))))))
        (declare (dynamic-extent #'do-si))
        (cond ((eq space-instances 't)
               (map-space-instances #'do-si '(*) invoking-fn-name))
              (t (map-space-instances
                  #'do-si
                  (ensure-list space-instances)
                  invoking-fn-name))))
      (when (and (null result)
                 *warn-about-unusual-requests*)
	(warn "None of the retrieval dimensions of pattern ~w ~_overlap with ~
               those of unit ~:[class~;classes~] ~s on ~s."
	      pattern
	      (consp unit-classes-spec)
	      unit-classes-spec
	      space-instances))      
      result)))

;;; ---------------------------------------------------------------------------
;;;  Syntax Reminder --
;;;
;;;  <unit-classes-spec> :== t | <single-class-spec> | (<single-class-spec>+)
;;;  <single-class-spec> :== <class-spec> | (<class-spec> <subclass-indicator>)
;;;  <class-spec> :==  <class> | <class-name>
;;;  <subclass-indicator> :==  :plus-subclasses | :no-subclasses | + | =
;;;
;;; ---------------------------------------------------------------------------

(defun ill-formed-unit-classes-spec (unit-classes-spec)
  (error "Ill-formed unit-classes specification: ~s"
         unit-classes-spec))

;;; ---------------------------------------------------------------------------

(defun set-all-mbr-instance-marks (storage disjunctive-dimensional-extents)
  (map-all-instances-on-storage #'set-mbr-instance-mark
                                storage disjunctive-dimensional-extents nil))

;;; ---------------------------------------------------------------------------

(defun print-storage-usage-message (storage)
  (format *trace-output* 
          "~&;; * Space instance: ~s~%"
          (instance-name-of (space-instance-of storage))))

;;; ---------------------------------------------------------------------------

(defun missing-bucket-option-error (storage missing-option initargs)
  ;; (used in 1d-uniform-buckets.lisp and 2d-uniform-buckets.lisp):
  (error "Required storage ~s option was not specified ~
          for dimension~p~:* ~s of unit-class ~s on ~s."
         missing-option
	 (dimension-names-of storage)
	 (getf initargs ':stores-classes)
	 (instance-name-of (space-instance-of storage))))

;;; ---------------------------------------------------------------------------

(defun check-storage-dimensions/layout-lengths (storage layout initargs)
  (let* ((dimension-names (dimension-names-of storage))
	 (storage-dimension-length (length dimension-names))
	 (layout-length (length layout)))
    (flet ((the-error (many/few-string)
	     (error "Too ~a layout specifications supplied in ~w ~
                     for dimension~p~:* ~s of unit-class ~s on ~s."
		    many/few-string
		    layout
		    dimension-names
		    (getf initargs ':stores-classes)
		    (instance-name-of (space-instance-of storage)))))
      (when (>& layout-length storage-dimension-length)
	(the-error "many"))
      (when (<& layout-length storage-dimension-length)
	(the-error "few")))))

;;; ===========================================================================
;;;   Storage Initialization

(defmethod setup-instance-storage ((space-instance standard-space-instance)
                                   (storage-spec null))
  ;;; Sets up the default storage for `space-instance', when none was
  ;;; specified.  No storage is created for space instances that
  ;;; cannot store any unit instances.
  (setf (standard-space-instance.%%storage-spec%% space-instance) storage-spec)
  (setf (standard-space-instance.%%storage%% space-instance)
	(if (allowed-unit-classes-of space-instance)
	    (list (do-storage space-instance '(t t unstructured)))
	    nil)))

;;; ---------------------------------------------------------------------------

(defmethod setup-instance-storage ((space-instance standard-space-instance)
                                   (storage-spec cons))
  ;;; Sets up the specified storage for `space-instance'
  (unless (allowed-unit-classes-of space-instance)
    (error "Space instance ~s does not allow any unit instances."
	   space-instance))	 
  (setf (standard-space-instance.%%storage-spec%% space-instance) storage-spec)
  (let ((result nil))
    (cond
     ;; single storage specification:
     ((and (cddr storage-spec)
	   (atom (third storage-spec)))
      (push (do-storage space-instance storage-spec) result))
     ;; multiple storage specifications:
     (t (dolist (single-storage-spec storage-spec)
	  (push (do-storage space-instance single-storage-spec)
		result))))
    ;; be sure to include the full-coverage storage, just in case --
    ;; we could be a bit smarter about determining if we have full coverage
    ;; (checking against allowed-unit-classes/dimensions), but we simply
    ;; play it very safe for now:
    (flet ((fn (storage)
             (and (member
                   (load-time-value 
                    `(,(find-class 'standard-unit-instance) . t))
                   (the list (stores-classes-of storage))
                   :test #'equal)
                  (eq (dimension-names-of storage) 't))))
      (declare (dynamic-extent #'fn))
      (unless (member-if #'fn result)
        (push (do-storage space-instance '(t t unstructured))
              result)))
    (setf (standard-space-instance.%%storage%% space-instance)
          ;; Maintain in original specification order:
	  (nreverse result))))

;;; ---------------------------------------------------------------------------

(defun do-storage (space-instance storage-spec)
  ;;; Creates all of the storage objects for `space-instance' given
  ;;; `storage-spec'
  (destructuring-bind (unit-classes-spec dimension-names storage-class
		       &rest args)
      storage-spec
    (unless (eq dimension-names 't)
      (setf dimension-names (ensure-list dimension-names)))
    (let ((stores-classes (parse-unit-classes-specifier unit-classes-spec)))
      (apply #'make-instance
	     (case storage-class
	       (unstructured 'unstructured-storage)
	       (boolean 'boolean-storage)
	       (uniform-buckets
		(case (length dimension-names)
		  (1 '1d-uniform-buckets)
		  (2 '2d-uniform-buckets)
		  (otherwise 'uniform-buckets)))
	       (hashed 'hashed-storage)
	       (quadtree 
		(nyi))
	       ;; Allow for user-defined storage-classes:
	       (otherwise storage-class))
	     :space-instance space-instance
	     :stores-classes stores-classes
	     :dimension-names dimension-names
	     ;; Storage options:
	     args))))

;;; ===========================================================================
;;;   Describe Storage 

(defun describe-storage (storage)
  (let* ((instance-counts (instance-counts-of storage))
         (sorted-instance-counts 
          (and instance-counts
               (sort (copy-list instance-counts) #'string< :key #'car)))
         (total-instances (if instance-counts
                              (flet ((fn (a b) (+& a b)))
                                (declare (dynamic-extent #'fn))
                                (reduce #'fn instance-counts :key #'cdr))
                              0))
         (total-locators (+ total-instances (excess-locators-of storage))))
    (format t "~&~:(~s~) ~{~{~s~^~@[+~*~]~}~} ~s ~:[N/A~;~:*~,1f (~s/~s)~]~%"
            (type-of storage)
            (pretty-unit-classes-spec (stores-classes-of storage))
            (dimension-names-of storage)
            ;; Excess-locator ratio:
            (unless (zerop total-instances) 
              (/ (float total-locators) (float total-instances)))
            total-locators
            total-instances)
    (dolist (instance-count sorted-instance-counts)
      (format t "~3t~s~14t~6d~%"
              (car instance-count)
              (cdr instance-count))))
  (values))

;;; ===========================================================================
;;;  Space-Instance Storage Description

(defmethod describe-space-instance-storage
    ((space-instance standard-space-instance))
  (format t "~&~@(~s~) ~s~%"
          (type-of space-instance)
          space-instance)
  (dolist (storage (standard-space-instance.%%storage%% space-instance))
    (describe-storage storage))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod describe-space-instance-storage ((space-instance cons))
  (describe-space-instance-storage
   (find-space-instance-by-path space-instance ':with-error)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
