;;;; -*- Mode:Common-Lisp; Package:OS-INTERFACE-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/os-interface-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:02:37 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Basic OS Interface Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-13-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':os-interface-user) 
    (make-package ':os-interface-user
      :use '(:common-lisp :gbbopen-tools))))

(in-package :os-interface-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*)))

;;; ---------------------------------------------------------------------------

(defun log-error (control-string &rest args)
  (declare (dynamic-extent args))
  (format t "~&;; ~73,,,'*<*~>~
             ~%;; *** ~?~
             ~&;; ~73,,,'*<*~>"
          control-string
          args))

;;; ---------------------------------------------------------------------------

(defun forced-format (&rest args)
  (declare (dynamic-extent args))
  (apply #'format t args)
  (force-output))

;;; ---------------------------------------------------------------------------

(defun ps-test ()
   (let ((stream (run-external-program "ps" '("a")))
        (line))
    (while (setf line (read-line stream nil nil))
      (print line))
    (close-external-program-stream stream)))

;;; ---------------------------------------------------------------------------

(defun kill-test ()
  (let ((os-process (nth-value 1 (run-external-program "sleep" '("10")))))
    (sleep 1)
    (ps-test)
    (kill-external-program os-process)
    (sleep 1)
    (ps-test)))

;;; ---------------------------------------------------------------------------

(defun os-interface-test ()
  (ps-test)
  #+ignore
  (kill-test))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (os-interface-test))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


