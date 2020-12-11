;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/defflags.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:55:39 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *          Bitwise Flag Manipulation (within a fixnum flag field)
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2005, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-26-02 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clear-flag 
            #+unused defflags 
            flag-set-p 
            set-flag)))

;;; ===========================================================================
;;;
;;; DEFFLAGS accessor prefix flags {option}*
;;;
;;;   accessor        -- a symbol namimg a function to access the place for
;;;                      the flags value.
;;;   prefix          -- a symbol or string to be prepended to the flag names
;;;                      to define flag accessors.
;;;   flags           -- a list of symbols naming individual flags
;;;
;;; options:
;;;   (:start n)      -- start flags at bit n rather than 0.
;;;   (:max-index n)  -- maximum flag index (default 15)
;;;   (:constructor name)
;;;                   -- symbol naming a function to be generated to make a
;;;                      set of flags
;;;
;;; Example:
;;;
;;;   (defflags my-rec.slot "MY-REC."
;;;     (a b)
;;;     (:start 3))
;;;   (my-rec.a my-rec)             ; t if flag A is set; nil otherwise
;;;   (setf (my-rec.a my-rec) t)    ; sets flag A
;;;   (setf (my-rec.a my-rec) nil)  ; clears flag A
;;;
;;; Common Lisp's setf machinery does not provide access to the newvalue-form,
;;; eliminating the potential for optimizing the typical set/clear forms
;;; containing constant flag values.  Too bad, since with access to the form,
;;; we could eliminate the flag-value if check for boolean constants.

;;; ===========================================================================
;;;  Low-Level Flag Operation Macros

(defmacro flag-set-p (flag index)
  ;; The obvious fixnum logbitp form `(logbitp (& ,index) (& ,flag)) is not 
  ;; optimized in Allegro (6.2), so we do it ourself as follows:
  `(not (zerop& (logand
                 (& ,flag) 
                 (& ,(if (constantp index)
                         (ash 1 index)
                         `(ash 1 (& ,index))))))))

(defmacro set-flag (flag index)
  `(logior (& ,flag) 
           (& ,(if (constantp index)
                   (ash 1 index)
                   `(ash 1 (& ,index))))))

(defmacro clear-flag (flag index)
  `(logandc2 (& ,flag)
             (& ,(if (constantp index)
                     (ash 1 index)
                     `(ash 1 (& ,index))))))

;;; ===========================================================================
;;;   Defflags
;;;
;;; Not currently used in GBBopen, but retained just in case...

#+unused
(defmacro defflags (accessor prefix flags &rest options)
  (let ((start 0)
        (max-index 15) ;; minimum legal fixnum size (very conservative)
        (constructor nil)
        (forms nil))
    (when (symbolp prefix) (setq prefix (symbol-name prefix)))
    (dolist (option options)
      (ecase (first option)
        (:start (setq start (second option)))
        (:max-index (setq max-index (second option)))
        (:constructor (setq constructor (second option)))))
    (let ((index start))
      (dolist (flag flags)
        (when (symbolp flag) (setq flag (symbol-name flag)))
        (let ((reader (intern (concatenate 'string prefix flag))))
          (push `(defsetf ,reader (place) (nv)
                   (with-gensyms (flag-var)
                     `(let ((,flag-var (,',accessor ,place)))
                        (progn
                          (setf (,',accessor ,place)
                                (if ,nv
                                    (set-flag ,flag-var ,',index)
                                    (clear-flag ,flag-var ,',index)))
                          ,nv))))
                forms)
          (push `(defun ,reader (place) 
                   (flag-set-p (,accessor place) ,index))
                forms)
          ;; The inline declaration must come before the function definition,
          ;; so we push it after in the forms:
          (push `(declaim (inline ,reader)) forms)
          (incf& index)))
      (when (>& index max-index)
        (error "Flag index exceeded.")))
    `(progn
       ,@forms
       ,@(when constructor
           (let ((index (1-& start)))
             `((defun ,constructor (&key ,@flags)
                 (let ((value 0))
                   ,.(flet ((fn (flag)
                              `(when ,flag
                                 (setq value (set-flag value ,(incf& index))))))
                       (declare (dynamic-extent #'fn))
                       (mapcar #'fn flags))
                   value))))))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


