;;;; -*- Mode:Common-Lisp; Package:SWANK; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/slime-extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Sep  3 13:16:11 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                Extended SLIME REPL Command Processing
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill 
;;;
;;; Copyright (C) 2005-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-17-08 Separated out from extended-repl.lisp and rewritten for
;;;           the latest Swank mechanisms.  (Corkill)
;;;  08-24-10 Redefine Swank's SIMPLE-REPL to provide command processing for
;;;           nil communication style.  (Corkill)
;;;  05-29-12 Remove support for contrib/swank-listener-hooks (no longer
;;;           set *listener-eval-function* binding).  (Corkill)
;;;  09-03-12 Conditionally support contrib/swank-listener-hooks.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	 
(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-user::compile-if-advantageous)))

;;; ---------------------------------------------------------------------------

(defun get-extended-repl-command-with-help (command)
  ;; Used in extended-repl-swank-eval-hook to add SLIME support for :help on
  ;; CLs that already provide their own REPL help command (and is therefore
  ;; not in *extended-repl-commands*)
  (or (assoc command cl-user::*extended-repl-commands* :test #'eq)
      #+(or allegro ecl)
      (and (member command '(:help :h))
	   #+allegro
	   '(:help tpl::tpl-help-command)
	   #+ecl
	   '(:help si::tpl-help-command))))

(compile-if-advantageous 'get-extended-repl-command-with-help)

;;; ---------------------------------------------------------------------------

(defun repl-command-form (string)
  ;; Performs REPL command processing, if `string' contains a command.
  ;; Returns true, if a command was executed and normal REPL processing should
  ;; be skipped; nil if normal REPL evaluation should be performed on `string'.
  (setf string (string-left-trim '(#\space #\tab) string))
  (when (or 
         ;; Check for 'spread' command syntax:
         (eql (char string 0) #\:)
         ;; Quick check for 'list' command syntax::
         (and (eql (char string 0) #\()
              (eql (char string 1) #\:)))
    (with-input-from-string (stream string)
      (let ((form (read stream nil stream)))
        (flet ((do-command (symbol-or-fn args)
                 (apply (the function (if (symbolp symbol-or-fn) 
                                          (fdefinition symbol-or-fn)
                                          symbol-or-fn))
                        args)
                 (values)))
          (cond 
           ;; No form was read:
           ((eq form stream) nil)
           ;; A keyword symbol (possible command) was read:
           ((keywordp form)
            (let ((repl-command (get-extended-repl-command-with-help form)))
              (when repl-command
                (do-command (second repl-command) 
                  (loop 
                      for form = (read stream nil stream)
                      until (eq form stream)
                      collect form))
                ;; bypass normal REPL processing:
                't)))
           ;; Support (<command> <arg>*) syntax as well:
           ((consp form)
            (let ((repl-command
                   (get-extended-repl-command-with-help (car form))))
              (when repl-command
                (do-command (second repl-command) (cdr form))
                ;; bypass normal REPL processing:
                't)))))))))

(compile-if-advantageous 'repl-command-form)

;;; ---------------------------------------------------------------------------
;;;  Conditional support for SLIME's contrib/slime-repl:

(defun extended-repl-eval (string)
  (unless (repl-command-form string)
    ;; Normal REPL processing:
    (funcall 'repl-eval string)))

(compile-if-advantageous 'extended-repl-eval)

(when (boundp '*listener-eval-function*)
  (format t "~&;; Interfacing with SLIME contrib/slime-repl...~%")
  (setf *listener-eval-function* 'extended-repl-eval))

;;; ---------------------------------------------------------------------------
;;;  Extended redefinition of SWANK's SIMPLE-REPL that adds command processsing 
;;;  for nil communiation style (the default style with ECL & CLISP):

(defun simple-repl (&aux (buffer "") prompt-issued?)
  (flet ((issue-prompt () 
           (format t "~a> " (package-string-for-prompt *package*))
           (setf prompt-issued? 't)
           (force-output)))
    (loop
      (issue-prompt)
      (let ((line (handler-case (read-line)
                    (end-of-repl-input () (return)))))
        (when (plusp (length line))
          (unless (repl-command-form line)
            (setf buffer (concatenate 'simple-string
                           buffer #.(string #\newline) line))
            (let* ((eof '#:eof))
              (loop
                (multiple-value-bind (form pos)
                    (handler-case (read-from-string buffer nil eof)
                      (error () eof))
                  (when (eq form eof) (return))
                  (setf buffer (subseq buffer pos))
                  (unless prompt-issued?
                    (issue-prompt))
                  (let ((- form)
                        (values (multiple-value-list (eval form))))
                    (setf *** **  ** *  * (car values)
                          /// //  // /  / values
                          +++ ++  ++ +  + form)
                    (cond ((null values) (format t "; No values~&"))
                          (t (flet ((print-it (v)
                                      (format t "~s~&" v)))
                               (declare (dynamic-extent #'print-it))
                               (mapc #'print-it values))))
                    (setf prompt-issued? nil)))))))))))

(compile-if-advantageous 'simple-repl)

;;; ---------------------------------------------------------------------------

(defun set-slime-repl-package (package-specifier)
  (when *emacs-connection*
    (let ((package-name 
           (if (packagep package-specifier)
               (package-name package-specifier)
               package-specifier))
          ;; Don't return the results:
          (*send-repl-results-function* #'identity))
      (declare (ignorable *send-repl-results-function*))
      (when *communication-style*       ; skip emacs-side setting in nil
                                        ; communication style
        (funcall 'repl-eval (format nil "(in-package ~s)" package-name)))
      (let ((package (find-package package-name)))
        (when package
          (setf *package* package)))
      ;; Return success:
      't)))

(compile-if-advantageous 'set-slime-repl-package)

;;; ---------------------------------------------------------------------------

(set-slime-repl-package *package*)      ; initialize to the current package

(format t "~&;; Finished loading extended REPL command processing for SLIME.~%")

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

