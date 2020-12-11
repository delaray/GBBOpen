;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/test-harness.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:03:06 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Entities for Testing Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-15-07 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(portable-threads:with-timeout)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(test)))

;;; ---------------------------------------------------------------------------

(defvar *timeout-indicator* '#:timeout)

;;; ---------------------------------------------------------------------------

(defun check-test-result (test label-or-form expected-result result)
  (unless (funcall test expected-result result)
    (let* ((timeout-occurred? 
            (and (consp result) (eq (first result) *timeout-indicator*)))
           (seconds
            (when timeout-occurred? (second result))))
      (cerror "Continue testing"
              "Test Failed~:[~*~; (timed out after ~s second~:p)~]~
               ~%~7t~:[Form: ~w~;Label: ~a~]~
               ~%~7tExpected: ~s~
               ~%~7tActual: ~:[~s~;~*N/A~]~%"
              timeout-occurred?
              seconds            
              (stringp label-or-form)
              label-or-form
              expected-result 
              timeout-occurred?
              result))))

;;; ---------------------------------------------------------------------------

(defmacro test ((expected-result &key label 
                                      seconds (test 'eql))
                &body body)
  `(check-test-result ',test ',(or label 
                                   (if (list-length-1-p body)
                                       (car body)
                                       `(progn ,@body)))
                      ',expected-result 
                      (#-with-timeout-not-available
                       ,@(if seconds
                             `(with-timeout
                                  (,seconds (list *timeout-indicator* 
                                                  ,seconds)))
                             '(progn))
                       #+with-timeout-not-available
                       progn 
                       ;; Evaluate the body:
                       ,@body)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


