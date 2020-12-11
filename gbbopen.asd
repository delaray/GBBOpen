;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jan  5 15:16:46 2013 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                           GBBopen ASDF Interface
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2013, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;;  Interfaces GBBopen's Module Manager facility, a lightweight and easy to use
;;;  mechanism for maintaining (compiling and loading) modules, with ASDF.
;;;
;;;  After ASDF loading, via (asdf:operate 'asdf:load-op :gbbbopen) or
;;;  (require :gbbopen), GBBopen REPL commands, such as :gbbopen-user,
;;;  :gbbopen-test, :agenda-shell-test, :tutorial-example, ... and their
;;;  COMMON-LISP-USER functional equivalents, are available.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-12-05 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

(require :asdf)

;;; ---------------------------------------------------------------------------

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "initiate"
	 :type "lisp"
	 :defaults truename))
  (defparameter *gbbopen-version*
      (with-open-file (version-file 
                       (make-pathname :name "VERSION"
                                      :type nil
                                      :defaults truename))
        (read version-file))))

;;; ===========================================================================
;;;  Compile/load GBBopen's Module Manager

(module-manager-user :propagate :create-dirs)

(in-package :module-manager)

;;; ===========================================================================
;;;  Utilities

(defun do-mm-component (mm-component &rest options)
  (declare (dynamic-extent options))
  (apply 'compile-module
         ;; Support cross-case mode CLs:
         (read-from-string (format nil ":~a" (asdf:component-name mm-component)))
         :propagate
         options))

;;; ---------------------------------------------------------------------------

(defun mm-component-done-p (mm-component)
  (module-loaded-p
   ;; Support cross-case mode CLs:
   (read-from-string (format nil ":~a" (asdf:component-name mm-component)))))

;;; ---------------------------------------------------------------------------

(defun mm-component-defsystem (module-name &optional no-components-p)
  (eval `(asdf:defsystem ,module-name
             :description 
               ,(if (eq ':gbbopen module-name)
                    "The GBBopen blackboard-system framework"
                    (format nil
                            "GBBopen ~s module; part of the GBBopen blackboard-system framework"
                            module-name))
	     :author "The GBBopen Project <gbbopen@GBBopen.org>"             
	     :maintainer "Dan Corkill <corkill@GBBopen.org>"
             :version ,common-lisp-user::*gbbopen-version*
             :licence "Licensed under Apache License 2.0 (see LICENSE for license information)."
	     ,@(unless no-components-p
                 `(:components ((:mm-component ,module-name)))))))

;;; ---------------------------------------------------------------------------

(defun mm-component-undefsystem (module-name)
  (asdf:clear-system module-name))

;;; ===========================================================================
;;;  Module-Manager ASDF component

(defclass mm-component (asdf:component)
  ())

(defmethod asdf:component-pathname ((component mm-component))
  nil)

(defmethod asdf:operation-done-p ((op asdf:compile-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-source-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:perform ((op asdf:compile-op) (component mm-component))
  (do-mm-component component))

(defmethod asdf:perform ((op asdf:load-op) (component mm-component))
  (do-mm-component component))

(defmethod asdf:perform ((op asdf:load-source-op) (component mm-component))
  (do-mm-component component :source))

;;; ===========================================================================
;;;  Generate an asdf:defsystem for each defined module

(format t "~&;; Defining an ASDF defsystem for each Module Manager module...~%")

(dolist (module-name (list-modules 't))
  (mm-component-defsystem module-name))

;;; ---------------------------------------------------------------------------
;;;  We have trivially completed all of the :gbbopen system operations by
;;;  loading this file:

#|
(defclass gbbopen (asdf:component)
  ())

(defmethod asdf:component-pathname ((component gbbopen))
  nil)

(defmethod asdf:operation-done-p ((op asdf:compile-op) (component gbbopen))
  t)

(defmethod asdf:operation-done-p ((op asdf:load-op) (component gbbopen))
  t)

(defmethod asdf:operation-done-p ((op asdf:load-source-op) (component gbbopen))
  t)

(defmethod asdf:perform ((op asdf:compile-op) (component gbbopen)))

(defmethod asdf:perform ((op asdf:load-op) (component gbbopen)))

(defmethod asdf:perform ((op asdf:load-source-op) (component gbbopen)))
|#

(mm-component-defsystem :gbbopen 't)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
