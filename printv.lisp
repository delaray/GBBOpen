;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/printv.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Nov 16 13:50:22 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            Handy PRINTV Macro
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-05-09 Copied from module-manager.lisp for stand-alone use.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------
;;;  NOTE: Keep synchronized with the original PRINTV definitions in
;;;  module-manager.lisp
;;;  ---------------------------------------------------------------------------

(defun printv-separator ()
  (format *trace-output* "~&;; ~60,,,'-<-~>~%")
  (force-output *trace-output*))

;;; ---------------------------------------------------------------------------

(defun printv-form-printer (form)
  (typecase form
    ;; String (label):
    (string (format *trace-output* "~&;; ~a~%" form))
    ;; Evaluated form:
    ((or cons
         (and symbol (not keyword)))
     (format *trace-output* "~&;;   ~w =>" form))
    ;; Self-evaluating form:
    (t (format *trace-output* "~&;;  ~s~%" form)))
  (force-output *trace-output*))

;;; ---------------------------------------------------------------------------

(defun printv-values-printer (values-list)
  (format *trace-output* 
          "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%" 
          values-list)
  (force-output *trace-output*))

;;; ---------------------------------------------------------------------------

(defun printv-expander (forms  
                        ;; Allow for customized printv-style printv'ers:
                        &optional values-trans-fn)
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil)
           ,result-sym)
       ,@(loop for form in forms 
             nconcing
               (cond
                ;; Separator requested?
                ((eq form ':hr)
                 ;; list used for splicing protection...
                 (list '(printv-separator)))
                ;; Evaluated form:
                ((or (consp form)
                     (and (symbolp form)
                          (not (keywordp form))))
                 `((printv-form-printer ',form)
                   (printv-values-printer
                    (setf ,result-sym
                          ,(if values-trans-fn
                               `(funcall ,values-trans-fn
                                         (multiple-value-list ,form))
                               `(multiple-value-list ,form))))))
                ;; Self-evaluating form:
                (t `((printv-form-printer 
                      (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))

;;; ---------------------------------------------------------------------------

(defmacro printv (&rest forms)
  (printv-expander forms))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
