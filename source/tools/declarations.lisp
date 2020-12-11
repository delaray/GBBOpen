;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/declarations.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Oct  1 04:45:47 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Optimization Declarations
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-02 File created.  (Corkill)
;;;  01-18-04 Added NYI error signalling.  (Corkill)
;;;  06-22-04 Added (debug 0) to WITH-FULL-OPTIMIZATION.  (Corkill)
;;;  01-26-08 Added MAKE-KEYS-ONLY-HASH-TABLE-IF-SUPPORTED.  (Corkill)
;;;  06-12-08 Added DEFCM.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*generate-nyi-errors*       ; not documented
            allow-redefinition          ; in module-manager, but part of tools
            defcm
            feature-present-p           ; in module-manager, but part of tools;
                                        ; not documented
            make-keys-only-hash-table-if-supported ; not documented
            need-to-port                ; in module-manager-loader, not documented
            nyi                         ; not documented
            unbound-value-indicator
            without-cmucl/sbcl/scl-optimization-warnings ; not documented
            with-full-optimization)))

;;; ---------------------------------------------------------------------------
;;;  NOTE: Copy any changes to WITH-FULL-OPTIMIZATION to the copy in
;;;  ../module-manager/module-manager.lisp 
;;;  ---------------------------------------------------------------------------

(defmacro with-full-optimization ((&key) &body body)
  ;;  The feature :full-safety disables with-full-optimization optimizations:
  `(locally #+full-safety ()
            #-full-safety 
            (declare (optimize (speed 3) 
                               (safety 0)
                               (space 0)
                               (debug 0)
                               (compilation-speed 0)
                               #+lispworks
                               (system:interruptable 0)))
            ,@body))

;;; ---------------------------------------------------------------------------

(defmacro without-cmucl/sbcl/scl-optimization-warnings (&body body)
  ;;  Suppress CMUCL, SBCL, and SCL compilation notes on failed optimizations:
  `(locally 
     ;; Inhibit CMUCL and SCL optimization warnings:
     #+(or cmu scl) (declare (optimize (extensions:inhibit-warnings 3)))
     ;; Eliminate SBCL optimization warnings by lowering the speed setting.
     ;; (It would be better to find a direct way to suppress these.)
     #+sbcl (declare (optimize (speed 1)))
     ,@body))

;;; ---------------------------------------------------------------------------
;;;  Defcm (also copied in portable-threads.lisp--duplicate any changes there)

(defmacro defcm (&body body)
  ;;; Shorthand conditional compiler-macro:
  (unless (or (feature-present-p ':full-safety)
              (feature-present-p ':disable-compiler-macros))
    `(define-compiler-macro ,@body)))

;;; ---------------------------------------------------------------------------
;;;   NYI wrapper (for use with code that is not yet ready for prime time)

(defvar *generate-nyi-errors* 't)

(defmacro nyi (&body body)
  `(progn
     (when *generate-nyi-errors*
       (error "Not yet implemented."))
     ,@body))

;;; ---------------------------------------------------------------------------

(defconstant unbound-value-indicator
    ;; We use Allegro's keyword symbol (as good as any choice...)
    ':---unbound---)

;;; ---------------------------------------------------------------------------
;;;  Keys-only hash tables

(defun make-keys-only-hash-table-if-supported (&rest args)
  ;; Return a keys only hash table, if supported by the CL implementation;
  ;; otherwise return a regular hash table:
  (declare (dynamic-extent args))
  (apply #'make-hash-table 
         ;; Use Allegro's sans-value hash tables:
         #+allegro :values #+allegro nil
         args))

;;; Add the keys-only-hash-table feature, if supported:
#+allegro
(pushnew ':has-keys-only-hash-tables *features*)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
