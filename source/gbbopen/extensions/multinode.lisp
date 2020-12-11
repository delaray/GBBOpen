;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/multinode.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:10:05 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        GBBopen Multi-Node Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-28-06 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; None of these are documented yet!
  (export '(add-gbbopen-node
	    all-gbbopen-nodes
	    current-gbbopen-node
	    delete-gbbopen-node
	    switch-gbbopen-node)))

;;; ---------------------------------------------------------------------------
;;;   Multinode state variables (not intended for direct user manipulation)

(defvar *%gbbopen-nodes%* nil)
(defvar *%remaining-gbbopen-nodes%* nil)
(defvar *%current-gbbopen-node%* nil)

;;; ---------------------------------------------------------------------------

(defun all-gbbopen-nodes ()
  *%gbbopen-nodes%*)
  
(defun current-gbbopen-node ()
  *%current-gbbopen-node%*)
  
;;; ---------------------------------------------------------------------------

(defun add-gbbopen-node (node-name 
			 &optional (node-state 'gbbopen-node-state))
  (if (member node-name *%gbbopen-nodes%* :key #'node-name-of)
      (warn "Node ~s is already present." node-name)
      (let ((node (make-instance node-state :node-name node-name)))
	(save-gbbopen-node-state node)
	(unless *%current-gbbopen-node%*
	  (setq *%current-gbbopen-node%* node))
	(push node *%gbbopen-nodes%*)
	(push node *%remaining-gbbopen-nodes%*))))

;;; ---------------------------------------------------------------------------

(defun delete-gbbopen-node (node-name)
  (if (member node-name *%gbbopen-nodes%* :key #'node-name-of)
      (setq *%gbbopen-nodes%* (delete node-name *%gbbopen-nodes%*
				      :key #'node-name-of))
      (warn "Node ~s was not found." node-name)))

;;; ---------------------------------------------------------------------------

(defun switch-gbbopen-node ()
  (unless *%remaining-gbbopen-nodes%*
    (setq *%remaining-gbbopen-nodes%* (shuffle-list *%gbbopen-nodes%*)))
  (when *%current-gbbopen-node%*
    (save-gbbopen-node-state *%current-gbbopen-node%*))
  (setq *%current-gbbopen-node%* (pop *%remaining-gbbopen-nodes%*))
  (when *%current-gbbopen-node%*
    (restore-gbbopen-node-state *%current-gbbopen-node%*))
  *%current-gbbopen-node%*) 

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================

