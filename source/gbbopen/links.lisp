;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/links.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 25 02:32:55 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            Link Functions
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
;;; ===========================================================================
;;;  Important Note:
;;;
;;;  In GBBopen, link-slot pointer lists are destructively modified by
;;;  linkf/unlinkf operations for top performance.
;;; ===========================================================================
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-16-02 File created.  (Corkill)
;;;  03-02-04 Ensure that LINKF! updates the link slot when no new links are 
;;;           added.  (Corkill)
;;;  03-09-04 Fix incorrect delete-all incoming pointers code.  (Corkill)
;;;  07-08-04 Fix CHECK-LINK-DEFINITIONS singular-link comparison.  (Corkill)
;;;  08-16-04 Allow %DO-IUNLINKS to delete links even when the link
;;;           singularity has been redefined.  (Corkill)
;;;  12-04-04 Fix to handle some link/non-link shadowing (not a general
;;;           solution, however).  (Corkill)
;;;  12-10-04 Updated for Lispworks 4.4. (Corkill)
;;;  01-06-05 Fix FIND-CLASS failure in set-inverse-link-definition for
;;;           inverse links in the class that is being defined (thanks to 
;;;           Earl Wagner for finding this problem!).  (Corkill)
;;;  03-23-05 Fix incorrect mapping class in CHECK-LINK-DEFINITIONS.  (Corkill)
;;;  07-30-05 Rename LINKF! to LINK-SETF.  (Corkill)
;;;  03-11-06 Add ECL support.  (Corkill)
;;;  06-24-06 Finally rewrote get-dlslotd-from-reader to handle object-specific
;;;           lookups.  (Corkill)
;;;  06-19-09 Add link-pointer-object support.  (Corkill)
;;;  05-06-10 Memoize dlslotd lookups.  (Corkill)
;;;  04-01-11 Add CHECK-ALL-INSTANCE-LINKS.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(check-all-instance-links    ; not documented yet
            check-link-definitions
            link-instance               ; STANDARD-LINK-POINTER slot
	    link-setf
	    linkf
            standard-link-pointer
	    unlinkf
	    unlinkf-all)))

;;; ===========================================================================
;;;  Standard-link-pointer (base class for link-pointer object customization)

(define-class standard-link-pointer (standard-gbbopen-instance)
  (link-instance))

(defmethod print-instance-slots ((obj standard-link-pointer) stream)
  (call-next-method)
  (print-instance-slot-value obj 'link-instance stream))

;;; ===========================================================================

(defun map-direct-link-slots (fn class)
  (declare (type function fn))
  (dolist (slot (class-direct-slots class))
    (when (typep slot 'direct-link-definition)
      (funcall fn slot))))

;;; ---------------------------------------------------------------------------
;;; Set all inverse-link-definition information for each direct link slot in
;;; the unit class.  This is done in initialize-instance &
;;; reinitialize-instance :after methods (rather than a shared-initialize
;;; :after method) to give ECL a chance to create-the direct-slot-definitions.

(defun set-all-inverse-link-definitions (class)
  (flet ((do-a-link (slot) 
	   (set-inverse-link-definition class slot)))
    (declare (dynamic-extent #'do-a-link))
    (map-direct-link-slots #'do-a-link class)))

(defmethod initialize-instance :after ((class standard-unit-class) 
				       &key)
  (set-all-inverse-link-definitions class))

(defmethod reinitialize-instance :after ((class standard-unit-class) 
					 &key)
  (set-all-inverse-link-definitions class))

;;; ===========================================================================
;;;   Link-slot setf sentinel

(defvar *%%allow-setf-on-link%%* nil)

;;; Catch unauthorized setf's of link slots.  This doesn't work on Lisps that
;;; optimize defclass link-writer methods rather than calling the (setf
;;; slot-value-using-class) method.  With those Lisps, we could attach to all
;;; the writer methods, but we haven't tried doing that yet.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to control
;;; the use of slot reader/writer methods.

(defmethod (setf slot-value-using-class) 
    :before (nv
             (class standard-unit-class)
             instance
             ;; instead of the effective-slot-definition, Lispworks
             ;; provides the slot name:
             (link-slot #+lispworks symbol
                        #-lispworks effective-link-definition))
  (declare (ignore nv))
  #+ecl (declare (ignore class))
  (unless (or *%%allow-setf-on-link%%*
              ;; determine if the slot is a link slot (Lispworks):
              #+lispworks
              (let ((link-slot 
                     ;; (car (member ...)) with :test & :key often optimizes
                     ;; better than (find ...):
                     (car (member link-slot (class-slots class)
                                  :test #'eq
                                  :key 'slot-definition-name))))
                (not (typep link-slot 'effective-link-definition))))
    (error "~s attempted on a link slot ~s of unit class ~s"
           'setf 
           #+(or ecl lispworks)
           link-slot
           #-(or ecl lispworks)
           (slot-definition-name link-slot)
           (type-of instance))))

;;; ---------------------------------------------------------------------------

(defun set-inverse-link-definition (class dlinkd)
  (let* ((inverse-link (direct-link-definition.inverse-link dlinkd))
         (ilinkd
          (if (eq inverse-link ':reflexive)
              dlinkd
	      (let* ((other-unit-class-name (first inverse-link))
		     (other-unit-class 
		      ;; find-class does not find the class currently
		      ;; being defined in some CLs (thanks to Earl Wagner
		      ;; for catching this!):
		      (if (eq other-unit-class-name (class-name class))
			  class
			  (find-class other-unit-class-name nil))))
		(when other-unit-class
		  (car (member (second inverse-link)
                               (class-direct-slots other-unit-class)
                               :test #'eq
                               :key #'slot-definition-name)))))))
    (when ilinkd
      ;; when possible, set the inverse-link's inverse-link-definition:
      (unless (eq inverse-link ':reflexive)
	(let ((idlinkd (direct-link-definition.inverse-link ilinkd)))
	  (when (and (eq (class-name class)
			 (first idlinkd))
		     (eq (slot-definition-name dlinkd)
			 (second idlinkd)))
	    (setf (direct-link-definition.inverse-link-definition ilinkd)
		  dlinkd))))
      ;; set the inverse-link-definition (set to nil if inverse is forward 
      ;; referenced):
      (setf (direct-link-definition.inverse-link-definition dlinkd)
	    ilinkd))))
                
;;; ===========================================================================
;;;   Link/unlink support functions

(defun build-link-macro-code (fn event-fn place env 
                              &optional 
                              (other-instances nil other-instances-p)
                              (forced-unlink-event-fn nil forced))
  ;;; Generates expansion code for all link macros (linkf & friends)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(let* (,.(mapcar #'list vars vals)
            ,@store-vars
            (.new. ,other-instances) 
            .change.
	    .dslotd.
            ,@(when forced '(.forced-unlinked.)))
       (multiple-value-setq (,(first store-vars) 
                             .change.
                             ,@(when forced '(.forced-unlinked.)))
         (,fn 
	  ;; determine the direct-link-definition:
	  (setf .dslotd.
	    ,(if (and (consp reader-form)
		      (eq (first reader-form) 'slot-value))
		 `(get-slot-value-dlslotd 
		   ,(second reader-form) 
		   ,(third reader-form))
		 `(get-dlslotd-from-reader 
		   ,(first vars)
		   ',(first reader-form))))
	  ,(first vars) ,reader-form
	  ,@(when other-instances-p '(.new.))
	  ,@(when forced `(',forced-unlink-event-fn))))
       ;; always set the slot on a LINK-SETF; otherwise only on a change:
       (when ,(if forced t '.change.)   
         (let ((*%%allow-setf-on-link%%* t))
           ,writer-form)
         ;; signal any forced direct unlinks by a LINK-SETF:
         ,@(when forced
             `((when .forced-unlinked.
                 (,forced-unlink-event-fn ,(second reader-form)
                                          .dslotd.
                                          ,(first store-vars)
                                          .forced-unlinked.))))
         ;; always signal the link-event on a LINK-SETF; otherwise only on a
         ;; change:
         (when ,(if forced 't '.change.)
           (,event-fn ,(second reader-form)
                      .dslotd.
                      ,(first store-vars)
                      .change.)))
       .new.)))

;;; ---------------------------------------------------------------------------

(defun non-empty-singular-link-cerror (dslotd instance existing-value)
  (cerror "Unlink instance ~1@*~s."
          "Singular link ~s in instance ~s is not empty. ~
           It contains ~s."
          (slot-definition-name dslotd)
          instance 
          existing-value))

;;; ---------------------------------------------------------------------------

(defun link-instance-of-or-nil (x)
  ;; Only calls LINK-INSTANCE-OF if `x' is non-nil:
  (when x
    (link-instance-of x)))

(defcm link-instance-of-or-nil (x)
  (with-once-only-bindings (x)
    `(when ,x
       (link-instance-of ,x))))

;;; ---------------------------------------------------------------------------

(defun %do-ilinks (dslotd instance-ptr-obj other-instance-ptr-objs force)
  ;;; Set the inverse pointers to instance in each other-instance, if it is
  ;;; not present already
  (let* ((instance (link-instance-of instance-ptr-obj))
         (idslotd (direct-link-definition.inverse-link-definition dslotd))
         (singular (direct-link-definition.singular idslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
	 (added-instances (list instance)))
    (dolist (other-instance-ptr-obj other-instance-ptr-objs)
      (let ((other-instance (link-instance-of other-instance-ptr-obj))
            (pointer-added? nil)
            new-value)
        (cond
         ;; ilink is singular:
         (singular 
          (let* ((existing-ptr-obj (slot-value other-instance slot-name))
                 (existing (link-instance-of-or-nil existing-ptr-obj)))
            ;; Do nothing, if the inverse pointer is present already:
            (unless (eq instance existing)
              ;; Handle another inverse pointer present:
              (when existing 
                (unless force
                  (non-empty-singular-link-cerror idslotd other-instance existing))
                ;; unlink the existing value
                (%do-iunlink idslotd other-instance existing))
              ;; Note the addition and set the value:
              (setf pointer-added? 't)
              (setf new-value instance)
              (setf (slot-value other-instance slot-name) instance-ptr-obj))))
         ;; multi-link ilink:
         (t 
          (let ((sort-function (direct-link-definition.sort-function idslotd))
                (sort-key (or (direct-link-definition.sort-key idslotd)
                              #'identity))
                (slot-value (slot-value other-instance slot-name)))
            ;; Do nothing, if the inverse pointer is present already:
            (unless (member instance slot-value
                            :key #'link-instance-of
                            :test #'eq)
              ;; Note the addition and set the value:
              (setf pointer-added? 't)
              (cond
               (sort-function
                (setf new-value
                      (flet ((fn (x)
                               (funcall sort-key (link-instance-of x))))
                        (declare (dynamic-extent #'fn))
                        (nsorted-insert 
                         instance-ptr-obj slot-value
                         sort-function 
                         (when sort-key #'fn))))
                (setf (slot-value other-instance slot-name) new-value))
               (t 
                (setf new-value (cons instance-ptr-obj slot-value))
                (setf (slot-value other-instance slot-name) new-value)))
              (setf new-value (mapcar #'link-instance-of new-value))))))
        ;; signal the indirect link event, if the inverse pointer was added:
        (when pointer-added?
          (%signal-indirect-link-event 
           other-instance idslotd 
           new-value
           added-instances))))))

;;; ---------------------------------------------------------------------------

(defun %do-iunlinks (dslotd instance other-instance-ptr-objs)
  ;;; Remove the inverse pointers to instance in each other-instance
  (let* ((idslotd (direct-link-definition.inverse-link-definition dslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
         (removed-instances (list instance)))
    (dolist (other-instance-ptr-obj other-instance-ptr-objs)
      (let* ((other-instance (link-instance-of other-instance-ptr-obj))
              (previous-value (slot-value other-instance slot-name))
	     (current-value
	      (if (consp previous-value)
		  (setf (slot-value other-instance slot-name) 
			(delete instance previous-value
                                :key #'link-instance-of
                                :test #'eq))
		  (setf (slot-value other-instance slot-name) nil))))
        (%signal-indirect-unlink-event 
         other-instance idslotd current-value removed-instances)))))

;;; ---------------------------------------------------------------------------

(defun %do-iunlink (dslotd instance other-instance-ptr-objs)
  ;;; Single value version of %do-iunlinks (above) --
  ;;; removes the inverse pointer to instance in other-instance
  (let* ((idslotd (direct-link-definition.inverse-link-definition dslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
         (removed-instances (list instance)))
    (let* ((other-instance (link-instance-of other-instance-ptr-objs))
           (previous-value (slot-value other-instance slot-name))
           (current-value
            (if (consp previous-value)
                (setf (slot-value other-instance slot-name) 
                      (delete instance previous-value
                              :key #'link-instance-of
                              :test #'eq))
                (setf (slot-value other-instance slot-name) nil))))
      (%signal-indirect-unlink-event
       other-instance idslotd current-value removed-instances))))

;;; ---------------------------------------------------------------------------

(defun %do-linkf (dslotd instance existing-ptr-objs new-ptr-objs &optional force)
  ;;; Determine the new links to add to `instance' and perform setting of
  ;;; inverse pointers.  Ensure that a link is never added more than once, no
  ;;; matter how many times it appears in `new-ptr-objs'.  Link-setf behavior
  ;;; is specified via `force'.
  ;;;
  ;;; Return the new link value, list of added instances, an indicator of
  ;;; forced removal, and the list of forced unlinked instances.
  (let ((forced-unlinked-instances nil)
        #+check-for-deleted-instances
        (operation (if force 'link-setf 'linkf)))
    #+check-for-deleted-instances
    (check-for-deleted-instance instance operation)
    (cond
     ;; nothing new to add:
     ((and (null new-ptr-objs) (not force))
      (values existing-ptr-objs nil nil))
     ;; singular link
     ((direct-link-definition.singular dslotd)
      ;; ensure atomic new-ptr-ojbs value:
      (when (consp new-ptr-objs)
        (setf new-ptr-objs (sole-element new-ptr-objs)))
      (let ((new (link-instance-of-or-nil new-ptr-objs)))
        #+check-for-deleted-instances
        (when new 
          (check-for-deleted-instance new operation))
        (cond
         ;; no-op if already present:
         ((eq (link-instance-of-or-nil existing-ptr-objs) new) 
          (values existing-ptr-objs nil nil))
         (t 
          (let ((existing (link-instance-of-or-nil existing-ptr-objs)))
            ;; non-empty existing value:
            (when existing
              (unless force
                (non-empty-singular-link-cerror dslotd instance existing-ptr-objs))
              ;; unlink the existing value
              (%do-iunlink dslotd instance existing)
              (push existing forced-unlinked-instances)))
          (let ((change (ensure-list new)))
            (%do-ilinks dslotd instance change force)
            ;; return the result values:
            (values new-ptr-objs change forced-unlinked-instances))))))
     ;; multi-link:
     (t (let ((change nil)
              (sort-function (direct-link-definition.sort-function dslotd))
              (sort-key (or (direct-link-definition.sort-key dslotd)
                            #'identity)))
          ;; ensure new-ptr-objs is a list:
          (unless (listp new-ptr-objs) 
            (setf new-ptr-objs (list new-ptr-objs)))
          ;; unlink any extra links:
          (when force
            (dolist (existing-ptr-obj existing-ptr-objs)
              (let ((existing (link-instance-of existing-ptr-obj)))
                (unless (member existing new-ptr-objs
                                :key #'link-instance-of
                                :test #'eq)
                  (setf existing-ptr-objs
                      (delete existing existing-ptr-objs
                              :key #'link-instance-of
                              :test #'eq))
                  (%do-iunlink dslotd instance existing)
                  (push existing forced-unlinked-instances)))))
          ;; add in new links:
          (dolist (new-ptr-obj new-ptr-objs)
            (let ((new (link-instance-of new-ptr-obj)))
              (unless (member new existing-ptr-objs
                              :key #'link-instance-of
                              :test #'eq)
                #+check-for-deleted-instances
                (check-for-deleted-instance new operation)
                (if sort-function
                    (setf existing-ptr-objs
                          (flet ((fn (x)
                                   (funcall sort-key (link-instance-of x))))
                            (declare (dynamic-extent #'fn))
                            (nsorted-insert 
                             new-ptr-obj existing-ptr-objs
                             sort-function
                             (when sort-key #'fn))))
                    (push new-ptr-obj existing-ptr-objs))
                (push new change))))
          (%do-ilinks dslotd instance change force)
          (values 
           ;; Use new-ptr-objs as the value for link-setf (unless a
           ;; sort-function was used); otherwise the updated existing value:
           (if (and force (not sort-function)) 
               (delete-duplicates new-ptr-objs
                                  :key #'link-instance-of
                                  :test #'eq 
                                  :from-end t)
               existing-ptr-objs)
           change
           forced-unlinked-instances))))))

;;; ---------------------------------------------------------------------------

(defun %do-unlinkf (dslotd instance existing-ptr-objs remove-ptr-objs)
  ;;; Unlink the specified instances from `dslotd' in `instance' and
  ;;; unlink the inverse pointers.
  ;;;
  ;;; Return the new link value and list of removed instances.
  #+check-for-deleted-instances
  (check-for-deleted-instance instance 'unlinkf)
  (cond
   ;; nothing to remove
   ((null remove-ptr-objs) (values existing-ptr-objs nil))
   ;; singular link
   ((direct-link-definition.singular dslotd)
    (let ((existing (link-instance-of-or-nil existing-ptr-objs))
          (remove (link-instance-of (if (consp remove-ptr-objs)
                                        (sole-element remove-ptr-objs)
                                        remove-ptr-objs))))
      (cond ((or (eq existing remove)
                 (and (consp remove)
                      (memq existing remove)))
             #+check-for-deleted-instances
             (check-for-deleted-instance existing 'unlinkf)
             ;; unlink the inverse pointer
             (%do-iunlink dslotd instance existing)
             ;; return the new link value and changes
             (values nil (list existing)))
            (t (values existing-ptr-objs nil)))))
   ;; multi-link
   (t (let ((change nil))
        ;; ensure remove-ptr-objs is a list
        (unless (listp remove-ptr-objs) 
          (setf remove-ptr-objs (list remove-ptr-objs)))
        (flet ((when-eq-push (a b)
                 (when (eq a b)
                   (push a change))))
          (declare (dynamic-extent #'when-eq-push))
          (dolist (remove-ptr-obj remove-ptr-objs)
            (let ((remove (link-instance-of remove-ptr-obj)))
              #+check-for-deleted-instances
              (check-for-deleted-instance remove 'unlinkf)
              (setf existing-ptr-objs (delete remove existing-ptr-objs 
                                              :key #'link-instance-of
                                              :test #'when-eq-push))))
          (when change
            ;; unlink the inverses
            (%do-iunlinks dslotd instance change))
          ;; return the new link value and changes
          (values existing-ptr-objs change))))))
  
;;; ---------------------------------------------------------------------------

(defun %do-unlinkf-all (dslotd instance existing)
  ;;; Unlink all links from `dslotd' in `instance' and unlink the inverse 
  ;;; pointers.
  ;;;
  ;;; Return the new link value (nil) and list of removed instances.
  #+check-for-deleted-instances
  (check-for-deleted-instance instance 'unlinkf-all)
  (cond
   ;; singular link:
   ((direct-link-definition.singular dslotd)
    (when existing
      ;; unlink the inverse pointer of the existing value:
      (%do-iunlink dslotd instance existing)
      ;; return the new link value and changes:
      (values nil (list existing))))
   ;; multi-link:
   (t
    ;; unlink the inverse pointers:
    (%do-iunlinks dslotd instance existing)
    ;; return the new link value and changes:
    (values nil existing))))
  
;;; ---------------------------------------------------------------------------

;;; This approach is temporary and nasty!  Need to do a much better job of
;;; accessing the effective & definition objects directly within the accessor
;;; code...

(defun find-eslotd-given-dslotd (instance dslotd)
  ;; (car (member ...)) with :test & :key often optimizes better than (find
  ;; ...):
  (car (member (slot-definition-name dslotd) 
               (class-slots (class-of instance))
               :key #'slot-definition-name
               :test #'eq)))

;;; ---------------------------------------------------------------------------

(defun %signal-direct-link-event (instance dslotd current-value 
				  added-instances)
  (signal-event-using-class (load-time-value (find-class 'link-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :added-instances added-instances
                            :directp 't
                            :initialization *%%doing-initialize-instance%%*))

;;; ---------------------------------------------------------------------------

(defun %signal-indirect-link-event (instance dslotd current-value
				    added-instances)
  (signal-event-using-class (load-time-value (find-class 'link-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :added-instances added-instances
                            :directp nil
                            :initialization *%%doing-initialize-instance%%*))

;;; ---------------------------------------------------------------------------

(defun %signal-direct-unlink-event (instance dslotd current-value
				    removed-instances)
  (signal-event-using-class (load-time-value (find-class 'unlink-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :removed-instances removed-instances
                            :directp 't))

;;; ---------------------------------------------------------------------------

(defun %signal-indirect-unlink-event (instance dslotd current-value
				      removed-instances)
  (signal-event-using-class (load-time-value (find-class 'unlink-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :removed-instances removed-instances
                            :directp nil))

;;; ---------------------------------------------------------------------------

(defun delete-incoming-link-pointer (instance link-slot)
  ;;; Internal function to delete the incoming pointers to `instance'
  ;;; associated with `link-slot'
  (declare (inline class-of))
  (let ((dslotd (effective-link-definition.direct-slot-definition link-slot))
	(inverse-instances
	 (slot-value-using-class (class-of instance) instance link-slot)))
    (when inverse-instances
      (if (direct-link-definition.singular dslotd)
          (%do-iunlink dslotd instance inverse-instances)
          (%do-iunlinks dslotd instance inverse-instances))
      (%signal-direct-unlink-event 
       instance link-slot nil
       (ensure-list inverse-instances)))))

;;; ---------------------------------------------------------------------------

(defun delete-all-incoming-link-pointers (instance) 
  ;;; Internal function to delete all incoming pointers to `instance'
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (dolist (slot (class-slots unit-class))
      (when (typep slot 'effective-link-definition)
	(delete-incoming-link-pointer instance slot)))))

;;; ===========================================================================
;;;   Direct-link definition lookups
;;;
;;; The following two function peform direct-link-definition lookups on every
;;; link-slot operation, so they always reflect the latest definitions.  For
;;; performance, the lookups are memoized, with the caches reset by
;;; FINISH-UNIT-CLASS-LOADING to reflect redefined direct-link-definitions.

;;; ---------------------------------------------------------------------------

(defun clear-memoized-dlslotd-caches (unit-class)
  (setf (standard-unit-class.memoized-slot-value-dlslotds unit-class)
        (make-et))
  (setf (standard-unit-class.memoized-dlslotds-from-reader unit-class)
        (make-et)))

;;; ---------------------------------------------------------------------------

(defun get-slot-value-dlslotd (object slot-name)
  ;;; Return the direct-link definition for `slot-name' in `object' by looking
  ;;; for the closest (CPL) definition
  (declare (inline class-of))
  (let* ((unit-class (class-of object))
         (memoized-slot-value-dlslotds
          (standard-unit-class.memoized-slot-value-dlslotds unit-class)))
    (or (get-et slot-name memoized-slot-value-dlslotds)
        (setf (get-et slot-name memoized-slot-value-dlslotds)
              (flet
                  ((fn (class)
                     (flet
                         ((slot-fn (dslotd)
                            (when (and (eq slot-name 
                                           (slot-definition-name dslotd))
                                       (typep dslotd 'direct-link-definition)) 
                              dslotd)))
                       (declare (dynamic-extent #'slot-fn))
                       ;; (car (member-if ...)) often optimizes better than
                       ;; (find-if ...):
                       (car (member-if #'slot-fn
                                       (class-direct-slots class))))))
                (declare (dynamic-extent #'fn))
                (some #'fn (class-precedence-list unit-class)))))))
    
;;; ---------------------------------------------------------------------------

(defun get-dlslotd-from-reader (object reader-method-name)
  ;;; Return the direct-link definition associated with `reader-method-name'
  ;;; and `object'
  (declare (inline class-of))
  (let* ((unit-class (class-of object))
         (memoized-dlslotds-from-reader
          (standard-unit-class.memoized-dlslotds-from-reader unit-class)))
    (or (get-et reader-method-name memoized-dlslotds-from-reader)
        (setf (get-et reader-method-name memoized-dlslotds-from-reader)
              (let* ((reader-methods
                      (compute-applicable-methods
                       (symbol-function reader-method-name) (list object)))
                     (reader-method
                      #+ecl (first (last reader-methods))
                      #-ecl (first reader-methods)))
                (or (when (typep reader-method 'standard-reader-method)
                      (let ((dslotd
                             (accessor-method-slot-definition reader-method)))
                        (when (typep dslotd 'direct-link-definition)
                          dslotd)))
                    (error "Unable to determine the link slot of place ~s ~
                              for ~s" 
                           reader-method-name
                           object)))))))
  
;;; ===========================================================================
;;;   Linkf & friends

(defmacro linkf (place other-instances &environment env)
  (build-link-macro-code '%do-linkf '%signal-direct-link-event
                         place env other-instances))

(defmacro link-setf (place other-instances &environment env)
  (build-link-macro-code '%do-linkf '%signal-direct-link-event 
                         place env other-instances
                         '%signal-direct-unlink-event ))

(defmacro unlinkf (place other-instances &environment env)
  (build-link-macro-code '%do-unlinkf '%signal-direct-unlink-event 
                         place env other-instances))

(defmacro unlinkf-all (place &environment env)
  (build-link-macro-code '%do-unlinkf-all '%signal-direct-unlink-event 
                         place env))

;;; ===========================================================================
;;;   Link consistency checking

(defun %check-link-definition (class linkd silent errorp)
  (let ((inverse-link (direct-link-definition.inverse-link linkd)))
    (cond 
     ;; reflexive links are OK:
     ((eq inverse-link ':reflexive) t)
     ;; compare the inverse-link specification in `linkd' with the actual
     ;; ilinkd values:
     (t (let* ((iunit-class-name (first inverse-link))
               (iunit-class (find-class iunit-class-name nil)))
          (cond 
           ;; missing class for inverse link:
           ((not iunit-class)
            (unless silent
              (funcall (if errorp #'error #'warn)
                       "The inverse of link slot ~a in unit-class ~s refers ~
                        to unit class ~s, which is not defined."
                       (slot-definition-name linkd)
                       (class-name class)
                       iunit-class-name)))
           (t (let* ((islot-name (second inverse-link))
                     (ilinkd 
                      ;; (car (member ...)) with :test & :key often optimizes
                      ;; better than (find ...):
                      (car (member islot-name 
                                   (class-direct-slots iunit-class)
                                   :test #'eq
                                   :key #'slot-definition-name)))
                     (singular-inverse
                      (getf inverse-link :singular nil)))
                (cond
                 ;; missing inverse link slot:
                 ((or (not ilinkd)
                      (not (typep ilinkd 'direct-link-definition)))
                  (unless silent
                    (funcall (if errorp #'error #'warn)
                             "The inverse of link slot ~a in unit-class ~s ~
                              refers to link slot ~s which is not present ~
                              in unit class ~s."
                             (slot-definition-name linkd)
                             (class-name class)
                             islot-name
                             iunit-class-name)))
                 ;; mismatched singularity:
                 ((xor singular-inverse
		       (direct-link-definition.singular ilinkd))
                  (unless silent
                    (funcall (if errorp #'error #'warn)
                             "Link slot ~a in unit class ~s incorrectly ~
                              declares its inverse link slot ~s in unit ~
                              class ~s as ~:[not ~;~]singular."
                             (slot-definition-name linkd)
                             (class-name class)
                             (slot-definition-name ilinkd)
                             iunit-class-name
                             singular-inverse)))
                 (t t))))))))))

;;; ---------------------------------------------------------------------------

(defun check-link-definitions (&optional silentp errorp)
  (let ((result 't))
    (flet ((fn (class plus-subclasses)
             (declare (ignore plus-subclasses))
             (flet ((do-link (link) 
                      (unless (%check-link-definition
                               class link silentp errorp)
                        (if silentp
                            (setf result nil)
                            (return-from check-link-definitions nil)))))
               (declare (dynamic-extent #'do-link))
               (map-direct-link-slots 
                #'do-link (ensure-finalized-class class)))))
      (declare (dynamic-extent #'fn))
      (map-unit-classes
       #'fn
       ;; Note: standard-unit-instance isn't defined until after this file is
       ;;       loaded, so we can't use load-time-value on this class
       ;;       "constant"
       (find-class 'standard-unit-instance)))
    (when (and result (not silentp))
      (format t "~&;; All link definitions are consistent.~%"))
    result))

;;; ---------------------------------------------------------------------------

(defun %check-instance-inverse-link (instance dslotd inverse-instance 
                                     idslotd warn-fn errorp)
  (declare (function warn-fn))
  (cond
   ;; Pointer to a deleted instance:
   ((instance-deleted-p (link-instance-of inverse-instance))
    (let ((slot-name (slot-definition-name dslotd)))
      (flet ((missing-link ()
               (funcall warn-fn
                        "Deleted instance ~s is referenced in link-slot ~
                         ~s in ~s"
                        inverse-instance
                        slot-name
                        instance))
             (reference-removed ()
               (format t "~&;; Reference to ~s removed." inverse-instance)))
        (cond
         ;; Singular link:
         ((direct-link-definition.singular dslotd)
          (missing-link)
          (when errorp
            (let ((*%%allow-setf-on-link%%* 't))
              (setf (slot-value instance slot-name) nil))
            (reference-removed)))
         ;; Non-singular link:
         (t (let ((link-slot-value (slot-value instance slot-name)))
              (missing-link)
              (when errorp
                (let ((*%%allow-setf-on-link%%* 't))
                  (setf (slot-value instance slot-name)
                        (delete inverse-instance link-slot-value
                                :key #'link-instance-of
                                :test #'eq)))
                (reference-removed))))))))
  ;;; Check that the inverse-pointer back to `instance' is present in
  ;;; `inverse-instance'
   (t (let* ((slot-name (slot-definition-name idslotd))
             (link-slot-value (slot-value inverse-instance slot-name)))
        (flet ((missing-link ()
                 (funcall warn-fn
                          "Inverse link back to ~s (link-slot ~s) from ~s ~
                           is missing in link-slot ~s"
                          instance
                          (slot-definition-name dslotd)
                          inverse-instance
                          slot-name
                          link-slot-value))
               (link-added ()
                 (format t "~&;; Link to ~s added." instance)))
          (cond
           ;; Singular link:
           ((direct-link-definition.singular idslotd)
            (unless (eq instance (link-instance-of link-slot-value))
              (missing-link)
              (when errorp
                (let ((*%%allow-setf-on-link%%* 't))
                  (setf (slot-value inverse-instance slot-name) instance))
                (link-added))))
           ;; Non-singular link:
           (t (unless (and (consp link-slot-value)
                           (member instance link-slot-value
                                   :key #'link-instance-of
                                   :test #'eq))
                (missing-link)
                (when errorp
                  (let ((sort-function
                         (direct-link-definition.sort-function idslotd))
                        (sort-key 
                         (or (direct-link-definition.sort-key idslotd)
                             #'identity))
                        (*%%allow-setf-on-link%%* 't)
                        (slot-value (slot-value inverse-instance slot-name)))
                    (setf (slot-value inverse-instance slot-name)
                          (if sort-function
                              ;; sorted, put the pointer in the proper location:
                              (flet ((fn (x)
                                       (funcall sort-key (link-instance-of x))))
                                (declare (dynamic-extent #'fn))
                                (nsorted-insert 
                                 instance 
                                 slot-value
                                 sort-function 
                                 (when sort-key #'fn)))
                              ;; unsorted, just push the pointer:
                              (cons instance slot-value))))
                  (link-added))))))))))
   
;;; ---------------------------------------------------------------------------
;;;   STILL TO DO: Fixup singular/non-singular errors, check & fixup sort
;;;                order errors

(defun check-all-instance-links (&optional silentp errorp 
                                 &aux (problem-counter 0)
                                      (repairs-counter 0))
  (flet 
      ((check-one (instance)
         (let ((class (class-of instance)))
           (flet ((warn-fn (&rest args)
                    (declare (dynamic-extent args))
                    (incf& problem-counter)
                    (cond 
                     ;; Error (with possible fixup):
                     (errorp 
                      (apply #'cerror "Fix the inconsistency." args)
                      (incf& repairs-counter))
                     ;; Non-silent warn:
                     ((not silentp)
                      (apply #'warn args)))))
             (declare (dynamic-extent #'warn-fn))
             (dolist (slot (class-slots class))
               (when (typep slot 'effective-link-definition)
                 (let* ((dslotd 
                         (effective-link-definition.direct-slot-definition slot))
                        (islotd
                         (direct-link-definition.inverse-link-definition dslotd))
                        (link-slot-value
                         (slot-value-using-class class instance slot)))
                   (when link-slot-value
                     (cond
                      ;; Singular link:
                      ((direct-link-definition.singular dslotd)
                       (if (consp link-slot-value)
                           (funcall #'warn-fn
                                    "Non-atomic link-slot value ~s in ~
                                     singular link slot ~s of ~s"
                                    link-slot-value
                                    (slot-definition-name slot)
                                    instance)
                           (%check-instance-inverse-link                            
                            instance dslotd 
                            (link-instance-of link-slot-value)
                            islotd #'warn-fn errorp)))
                      ;; Non-singular link:
                      (t (unless (consp link-slot-value)
                           (funcall #'warn-fn
                                    "Atomic link-slot value ~s in ~
                                     link slot ~s of ~s"
                                    link-slot-value
                                    (slot-definition-name slot)
                                    instance)
                           ;; Allow checking to continue:
                           (setf link-slot-value (list link-slot-value)))
                         (dolist (linked-instance link-slot-value)
                           (%check-instance-inverse-link
                            instance dslotd 
                            (link-instance-of linked-instance)
                            islotd #'warn-fn errorp))))))))))))
    (declare (dynamic-extent #'check-one))
    (map-instances-of-class #'check-one 't)
    (unless silentp
      (if (zerop& problem-counter)
          (format t "~&;; All instance links are consistent.~%")
          (format t "~&;; ~s ~:[problems were~;problem was~] found.~%"
                  problem-counter
                  (=& 1 problem-counter)))
      (when (plusp& repairs-counter)
        (format t "~&;; ~s ~:[repairs were~;repair was~] made.~%"
                repairs-counter
                (=& 1 repairs-counter))))
    problem-counter))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
