;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/epilogue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Feb 12 02:44:11 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                GBBopen Miscellaneous Entities & Epilogue
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
;;;  01-16-04 File created.  (Corkill)
;;;  05-03-04 Added reset-gbbopen.  (Corkill)
;;;  01-28-08 Added load-blackboard-repository and save-blackboard-repository.
;;;           (Corkill)
;;;  05-02-08 Added :di and :dsi REPL commands.  (Corkill)
;;;  05-25-08 Added :dsis REPL command.  (Corkill)
;;;  04-25-09 Added :pic REPL command.  (Corkill)
;;;  01-19-11 Split out load/save/reset repository entities to 
;;;           save-restore.lisp.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command)))

;;; ---------------------------------------------------------------------------
;;;  Result-passing variable (used by :fi REPL command), with a bit of
;;;  effort to make use of the = symbol where necessary...

#-(or lispworks sbcl)
(defvar = nil)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (special =))
  (unless (boundp '=)
    (setf = nil)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext::without-package-locks
   (defvar = nil)))

;;; ---------------------------------------------------------------------------
;;;  Add :di, :dsbb, :dsi, :dsis, :fi, :fsi, and :pic REPL commands (available
;;;  if using GBBopen's initiate.lisp)

(defun do-di/dsi/dsis/fi/fsi-repl-command (type find-fn describe-fn args)
  (let ((maybe-instance 
         ;; Handle evaluating REPLs:
         (if (typep (first args) type)
             ;; Already evaluated:
             (first args)
             ;; Try evaluating:
             (ignore-errors (eval (first args))))))
    (cond 
     ;; We're given a unit instance:
     ((typep maybe-instance type)
      (setf = maybe-instance)
      (funcall describe-fn maybe-instance))
     ;; Look it up
     (t (let ((instance (apply find-fn args)))
          (cond
           (instance 
            (setf = instance)
            (cond
             (describe-fn (funcall describe-fn instance))
             (t (format t "~&Found ~s (assigned to =)~%" instance)
                (force-output)
                instance)))
           (t (case type
                (standard-unit-instance
                 (format t "~&No unit instance named ~s~@[ of class ~s~] ~
                             was found.~%"
                         (first args)
                         (second args)))
                (standard-space-instance
                 (format t "~&No space instance named ~s was found.~%"
                         (first args))))
              (force-output))))))))

;;; ===========================================================================
;;;  If DEFINE-REPL-COMMAND is available, define these GBBopen commands:

(when (fboundp 'define-repl-command)
  (eval `(let ((*current-system-name* ':gbbopen))
           (declare (special *current-system-name*))
           
           (define-repl-command :di (&rest args)
             "Describe instance"
             (do-di/dsi/dsis/fi/fsi-repl-command 'standard-unit-instance 
               'find-instance-by-name
               'describe-instance args))
           
           (define-repl-command (:dsbb :add-to-native-help) ()
             "Describe blackboard repository"
             (describe-blackboard-repository))
           
           (define-repl-command :dsi (&rest args)
             "Describe space instance"
             (do-di/dsi/dsis/fi/fsi-repl-command 'standard-space-instance 
               'find-space-instance-by-path
               'describe-space-instance args))
           
           (define-repl-command :dsis (&rest args)
             "Describe space instance storage"
             (do-di/dsi/dsis/fi/fsi-repl-command 'standard-space-instance 
               'find-space-instance-by-path 
               'describe-space-instance-storage args))
           
           (define-repl-command :fi (&rest args)
             "Find instance by name"
             (do-di/dsi/dsis/fi/fsi-repl-command 'standard-unit-instance
               'find-instance-by-name nil args))
           
           (define-repl-command :fsi (&rest args)
             "Find space instance by name"
             (do-di/dsi/dsis/fi/fsi-repl-command 'standard-unit-instance
               'find-space-instance-by-path nil args))
           
           (define-repl-command :pic (&rest args)
             "Print instances of class"
             (apply 'map-instances-of-class 'print (or args '(t)))))))


;;; ===========================================================================
;;;  GBBopen is fully loaded

(pushnew ':gbbopen *features*)
(pushnew *gbbopen-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


