;;;; -*- Mode:Emacs-Lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-indent.el *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jun  1 21:08:41 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen ELI Indentations 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-02 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(defun set-indent-hook (symbol value)
  (put symbol 'lisp-indent-function value)
  (put symbol 'fi:lisp-indent-hook value))

(defun gbbopen:add-indentation ()
  (interactive)
  ;; "Improve" some CL indentations:
  (set-indent-hook 'if 3)
  (set-indent-hook 'setf  0)
  (set-indent-hook 'and  0)
  (set-indent-hook 'or  0)
  ;; GBBopen-tools entity indentations:
  (set-indent-hook 'case-using 2)
  (set-indent-hook 'ccase-using 2)
  (set-indent-hook 'ecase-using 2)
  ;; GBBopen entity indentations:
  (set-indent-hook 'define-module 1)
  (set-indent-hook 'destructure-extent 2)
  (set-indent-hook 'dosequence 1)
  (set-indent-hook 'dosublists 1)
  (set-indent-hook 'make-space-instance 2)
  (set-indent-hook 'patch 2)
  (set-indent-hook 'start-patch 2)
  (set-indent-hook 'with-error-handling 1)
  (set-indent-hook 'without-lock-held 2))

(add-hook 'lisp-mode-hook (function gbbopen:add-indentation))
(add-hook 'fi:lisp-mode-hook (function gbbopen:add-indentation))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************

