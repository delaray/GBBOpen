;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/save-restore.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Mar 24 04:18:19 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *             GBBopen Repository Save/Restore/Reset Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-19-11 File split out from epilogue.lisp.  (Corkill)
;;;  03-18-11 Add *REPOSITORY-LOAD-PERCENTAGE-HOOK-FUNCTIONS*.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*recorded-class-descriptions-ht*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*repository-load-percentage-hook-functions* ; not yet documented
            *repository-load-percentage-reads-per-update* ; not yet documented
            confirm-if-blackboard-repository-not-empty-p
            delete-blackboard-repository
            empty-blackboard-repository-p
            load-blackboard-repository
            save-blackboard-repository
            reset-gbbopen)))

;;; ---------------------------------------------------------------------------
;;;  Save-blackboard-repository format version

(defparameter *save-blackboard-repository-format-version* 5)

;;; ===========================================================================
;;;  Miscellaneous Entities

(defun empty-blackboard-repository-p ()
  ;; Returns t if there are no unit instances in the blackboard repository
  (flet ((fn (class plus-subclasses-p)
           (declare (ignore plus-subclasses-p))
           (when (plusp& (class-instances-count class))
             (return-from empty-blackboard-repository-p nil))))
    (declare (dynamic-extent #'fn))
    (map-unit-classes 
     #'fn (load-time-value (find-class 'standard-unit-instance))))
  ;; The repository is empty:
  't)

;;; ---------------------------------------------------------------------------

(defun delete-blackboard-repository (&key (all-classes nil)
                                          (disable-events t)
                                          (retain-classes nil))
  (when (and all-classes retain-classes)
    (warn "~s is being overridden by ~s ~s."
          ':all-classes ':retain-classes retain-classes))
  ;;; Deletes all unit and space instances; resets instance counters to 1.
  (with-blackboard-repository-locked ()
    (let ((*%%events-enabled%%* (not disable-events)))
      (flet ((fn (unit-class plus-subclasses)
               (declare (ignore plus-subclasses))
               (unless (or 
                         ;; Explicitly retained:
                         (and retain-classes
                              (unit-class-in-specifier-p
                               unit-class retain-classes))
                         ;; :all-classes specified or a retained unit class:
                         (not (or all-classes
                                  (not (or (standard-unit-class.retain unit-class))))))
                 ;; We must practice safe delete-instance:
                 (let ((instances nil))
                   (flet ((do-instance (instance)
                            (push instance instances)))
                     (declare (dynamic-extent #'do-instance))
                     (map-instances-given-class #'do-instance unit-class))
                   (dolist (instance instances)
                     ;; Child space instances might already be deleted due to
                     ;; parent space-instance deletion, so we check for
                     ;; deleted-p status to avoid the
                     ;; operation-on-deleted-instance error:
                     (unless (instance-deleted-p instance)
                       (delete-instance instance)))
                   (reset-unit-class unit-class)))))
        (declare (dynamic-extent #'fn))
        (map-extended-unit-classes #'fn 't)))
    ;; Reset the global instance-name counter, if possible:
    (when (empty-blackboard-repository-p)
      (setf *global-instance-name-counter* 0)))
  ;; Return a polite success:
  't)
  
;;; ---------------------------------------------------------------------------

(defun reset-gbbopen (&key (disable-events t))
  (with-blackboard-repository-locked ()
    (delete-blackboard-repository :all-classes 't
                                  :disable-events disable-events)
    (setf *top-level-space-instances* nil))
  (disable-event-printing)
  (remove-all-event-functions)
  ;; Return a polite success:
  't)

;;; ===========================================================================
;;;  Save & restore repository

(defun make-bb-pathname (pathname) 
  ;; Adds type "bb", if not supplied ; then adds defaults from
  ;; (user-homedir-pathname), as needed:
  (merge-pathnames 
   pathname
   (make-pathname :type "bb"
                  :defaults (user-homedir-pathname))))

;;; ---------------------------------------------------------------------------

(defun make-unit-instance-count-alist ()
  (let ((result nil))
    (flet ((fn (unit-class plus-subclasses)
             (declare (ignore plus-subclasses))
             (let ((count (class-instances-count unit-class)))
               (when (plusp& count)
                 (push `(,(class-name unit-class) . ,count) result)))))
      (declare (dynamic-extent #'fn))
      (map-extended-unit-classes #'fn 't))
    result))

;;; ---------------------------------------------------------------------------

(defun save-blackboard-repository (pathname
                                   &key (package ':common-lisp)
                                        (read-default-float-format
                                         'single-float)
                                        (external-format ':default)
                                        (value))
  (with-open-file (stream (make-bb-pathname pathname)
                   :direction ':output
                   :if-exists ':supersede
                   :external-format external-format)
    (format stream ";;; GBBopen Blackboard Repository (saved ~a)~%"
            (internet-text-date-and-time))
    (with-saving/sending-block (stream :package package
                                       :read-default-float-format 
                                       read-default-float-format
                                       :value value)
      (with-blackboard-repository-locked ()
        ;; Save repository-format version:
        (format stream "~&;;; Saved repository format version:~%~s~%"
                *save-blackboard-repository-format-version*)
        ;; Save important values:
        (format stream "~&;;; Important values:~%~s~%"
                (list *global-instance-name-counter* (make-unit-instance-count-alist)))
        ;; Save space instances:
        (let ((top-level-space-instances *top-level-space-instances*))
          (format stream "~&;;; Space instances:~%")
          ;; Save top-level (root children) space-instance references:
          (let ((*save/send-references-only* 't))
            (print-object-for-saving/sending top-level-space-instances stream))
          ;; Now save the space-instances in the repository forest:
          (let ((*save/send-references-only* nil))
            (dolist (child top-level-space-instances)
              (flet ((do-si (space-instance)
                       (print-object-for-saving/sending space-instance stream)))
                (declare (dynamic-extent #'do-si))
                (traverse-space-instance-tree #'do-si child)))
            ;; Save non-space unit instances:
            (format stream "~&;;; Other unit instances:~%")
            (do-instances-of-class (instance t)
              ;; Skip space instances (standard-space-instance :plus-subclasses):
              (unless (typep instance 'standard-space-instance)
                (print-object-for-saving/sending instance stream)))
            ;; Save unit-class states:
            (format stream "~&;;; Unit-class states:~%")
            (loop for class-name being the hash-keys
                of *recorded-class-descriptions-ht* do
                  (let ((class (find-class class-name)))
                    (when (typep class 'standard-unit-class)
                      (print-unit-class-state-for-saving/sending
                       class stream))))))))
    (format stream "~&;;; End of file~%")
    (pathname stream)))

;;; ---------------------------------------------------------------------------

(defun confirm-if-blackboard-repository-not-empty-p
    (&key describe-non-empty-repository
          (pending-action "deleted"))
  ;; Returns true unless the blackboard-repository is not empty and the user
  ;; doesn't confirm continuing:
  (cond
   ((empty-blackboard-repository-p) 't)
   (t (when describe-non-empty-repository
        (describe-blackboard-repository))
      (nicer-y-or-n-p 
       "The~@[ above~*~] blackboard repository is not empty.~%Continue anyway ~
        (the current contents will be ~a)? "
       describe-non-empty-repository
       pending-action))))

;;; ---------------------------------------------------------------------------

(defvar *repository-load-percentage-reads-per-update* 1000)
(defvar *repository-load-percentage-hook-functions* nil)

;;; ---------------------------------------------------------------------------

(defun load-blackboard-repository
    (pathname 
     &rest reset-gbbopen-args
     &key (class-name-translations nil)
          (coalesce-strings nil)
          (confirm-if-not-empty 't)
          (disable-events 't)
          (estimated-peak-forward-references 
           *default-estimated-peak-forward-references*)
          (external-format ':default)
          (readtable *reading-saved/sent-objects-readtable*)
          (read-eval nil))
  (declare (dynamic-extent reset-gbbopen-args))
  (when confirm-if-not-empty
    (unless (confirm-if-blackboard-repository-not-empty-p)
      (return-from load-blackboard-repository nil)))
  (with-open-file (stream (make-bb-pathname pathname)
                   :direction ':input
                   :external-format external-format)
    (with-blackboard-repository-locked ()
      (apply 'delete-blackboard-repository
             :all-classes 't
             (remove-properties reset-gbbopen-args 
                                '(:class-name-translations
                                  :coalesce-strings
                                  :confirm-if-not-empty
                                  :estimated-peak-forward-references
                                  :external-format
                                  :readtable
                                  :read-eval)))
      (let ((*%%events-enabled%%* (not disable-events))
            (repository-file-length/100 (round (file-length stream) 100)))
        (with-reading-saved/sent-objects-block 
            (stream :class-name-translations class-name-translations
                    :coalesce-strings coalesce-strings
                    :estimated-peak-forward-references 
                      estimated-peak-forward-references
                    :readtable readtable
                    :read-eval read-eval)
          (let ((format-version (read stream)))
            (when (>& format-version *save-blackboard-repository-format-version*)
              (error "Incompatible ~s format version ~a ~
                      (the current version is ~a, generated by a newer ~
                      version of GBBopen)"
                     'save-blackboard-repository
                     format-version
                     *save-blackboard-repository-format-version*))
            (unless (>=& format-version 2)
              (error "Incompatible ~s format version ~a (the current version is ~a)"
                     'save-blackboard-repository
                     format-version
                     *save-blackboard-repository-format-version*))
            (when (>=& format-version 3)
              (destructuring-bind (global-instance-name-counter 
                                   &optional unit-instance-count-alist)
                  ;; Read important values:
                  (read stream)
                (setf *global-instance-name-counter* global-instance-name-counter)
                ;; Resize instance hash-tables:
                (dolist (acons unit-instance-count-alist)
                  (destructuring-bind (class-name . count)
                      acons
                    (let ((unit-class 
                           (find-unit-class 
                            (possibly-translate-class-name class-name))))
                      (when unit-class
                        (resize-instance-hash-table unit-class count)))))))
            ;; Skip after-loading-function, if an old format version:
            (when (<& format-version 4)
              (read stream)))
          ;; Read top-level (root children) space-instance references:
          (let ((root-children (read stream))
                (*%%loading-complete-repository%%* 't)
                (*%%allow-setf-on-link%%* 't))
            (setf *top-level-space-instances* root-children)
            ;; Now read everything else:
            (let ((eof-marker '#:eof)
                  (counter 0))
              (until (eq eof-marker (read stream nil eof-marker))
                ;; Load percentage hooks:
                (when *repository-load-percentage-hook-functions*
                  (unless (plusp& (decf& counter))
                    (setf counter *repository-load-percentage-reads-per-update*)
                    (let ((load-percentage 
                           (round (file-position stream)
                                  repository-file-length/100)))
                      (dolist (fn *repository-load-percentage-hook-functions*)
                        (funcall fn stream load-percentage)))))))
            ;; Process all unit instances, in case any link-slot arity or
            ;; sorting has changed (FUTURE ENHANCEMENT: It would be nice to
            ;; skip this unless such has truly happened):
            (map-instances-of-class #'reconcile-direct-link-values 't))
          ;; Return the pathname, saved/sent-time, and saved/sent-value:
          (values (pathname stream)
                  *block-saved/sent-time* 
                  *block-saved/sent-value*))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


