;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon May 28 17:38:56 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Extended REPL Command Processing
;;;; * 
;;;; *  for ABCL, CLISP, Closure CL, CMUCL, ECL, Lispworks, SBCL, SCL, and 
;;;; *  XCL REPL and for SLIME (Emacs->Swank)
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
;;; Porting Notice:
;;;
;;;   These extensions add:
;;;    - keyword-command capabilities to the REPL in CLISP, CMUCL, SBCL, 
;;;      and SCL
;;;    - interface into CLISP's *user-commands* facility
;;;    - extend ECL's command repertoire
;;;    - add keyword-command support to SLIME's Emacs->Swank interface  
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-04-05 File created.  (Corkill)
;;;  02-02-06 Added ECL support.  (Corkill)
;;;  02-04-06 Added SLIME (Emacs->Swank) support.  (Corkill)
;;;  04-17-08 Reworked SLIME mechanism.  (Corkill)
;;;  10-15-09 Added XCL support.  (Corkill)
;;;  01-04-11 Added partial ABCL support.  (Corkill)
;;;  09-13-11 Completed ABCL support.  (Corkill)
;;;  05-28-12 SBCL now prefers EXIT over QUIT.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	 
(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------
;;;  In ECL, setup an Extended REPL command topic (must be done destructively,
;;;  as si::*tpl-commands* is already lexically bound in the default TLP)

#+ecl
(setf (rest si::*tpl-commands*)
      (adjoin (list "Extended REPL Commands") (cdr si::*tpl-commands*)
	      :key #'car :test #'equal))

;;; ---------------------------------------------------------------------------
;;;  In Allegro, we hide non-native help commands by saving the command-name
;;;  strings in *non-native-help-commands* and fwrapping
;;;  TPL::GET-COMMANDS-LIST to remove the commands from its returned value.

#+allegro
(defvar *non-native-help-commands* nil)

#+allegro
(def-fwrapper filtered-get-commands-list-wrap ()
  ;; Remove non-native extended-REPL commands:
  (remove-if #'(lambda (command)
                 (member (first command) *non-native-help-commands*
                         :test #'equal))
             (call-next-fwrapper)))

#+allegro
(fwrap 'top-level::get-commands-list 'extended-repl 
       'filtered-get-commands-list-wrap)

;;; ---------------------------------------------------------------------------
;;;  With-system-name (copied in module-manager/module-manager.lisp for
;;;                    startup.lisp only invocation)

(defvar *current-system-name* nil)

(defmacro with-system-name ((&optional system-name) &body body)
  (unless (keywordp system-name)
    (error "System name, ~s, must be a keyword." system-name))
  `(let ((*current-system-name* ',system-name))
     ,@body))

;;; ---------------------------------------------------------------------------

(defvar *extended-repl-commands* nil)

(defun get-extended-repl-command (command)
  (assoc command *extended-repl-commands* :test #'eq))

(compile-if-advantageous 'get-extended-repl-command)

;;; ===========================================================================
;;;   Define-repl-command

(defun redefining-cl-user-repl-command-warning (command fn)
  ;; avoid SBCL optimization warning: 
  (declare (optimize (speed 1)))
  (let ((*package* (find-package ':common-lisp)))
    (format t "~&;; Redefining ~s function for REPL-command ~s~%"
            fn command)))

(compile-if-advantageous 'redefining-cl-user-repl-command-warning)

;;; ---------------------------------------------------------------------------

(defun add-repl-command-spec (command-spec)
  (setf *extended-repl-commands*
        (cons command-spec (delete (first command-spec)
                                   (the list *extended-repl-commands*)
                                   :test #'eq
                                   :key #'car))))

(compile-if-advantageous 'add-repl-command-spec)

;;; ---------------------------------------------------------------------------

(defun eval-special-repl-variable (var)
  (if (member var '(* ** *** =) :test #'eq)
      (eval var)
      var))

(compile-if-advantageous 'eval-special-repl-variable)

;;; ---------------------------------------------------------------------------

#+(or abcl
      xcl)
(defun read-args-from-string (string)
  (when string
    (ignore-errors
     (read-from-string 
      (concatenate 'string "(" string ")")))))

;;; ---------------------------------------------------------------------------

(defmacro define-repl-command (command-name lambda-list &rest body)
  (let ((.options. nil))
    ;; Handle (<command> <option>*) syntax:
    (when (consp command-name)
      (setf .options. (rest command-name))
      (setf command-name (first command-name))
      (let ((bad-options (set-difference .options. '(:add-to-native-help 
                                                     :no-cl-user-function
                                                     :no-help))))
        (dolist (bad-option bad-options)
          (warn "Illegal command option ~s specified for REPL-command ~s"
                bad-option command-name))))
    (let ((cl-user-fn-name
           ;; Check if a :cl-user function is to be created:
           (unless (member ':no-cl-user-function .options.)
             (intern (symbol-name command-name) ':common-lisp-user)))
          (tlc-sym (gensym))
          #+abcl
          (command-name-string (string-downcase (symbol-name command-name)))
	  #+xcl
	  (command-fn-sym
	   (intern (concatenate 'string
				"%"
				(symbol-name command-name)
				"-CMD%")
		   ':common-lisp-user))
          (maybe-doc (first body)))
      `(progn 
         ;; Define the :cl-user function, if desired:
         ,@(when cl-user-fn-name
             `((when (fboundp ',cl-user-fn-name)
                 (redefining-cl-user-repl-command-warning 
                  ',command-name ',cl-user-fn-name))
               (defun ,cl-user-fn-name ,lambda-list 
                 ,@body)))
	 ;; Define the command (with line parsing) for XCL:
	 #+xcl
	 (defun ,command-fn-sym (line)
	   (flet ((command ,lambda-list ,@body))
	      (apply #'command (read-args-from-string line))))
          ;; Now do the REPL-command definition:
         #+allegro
         ,@(unless (member ':add-to-native-help .options.)
             `((pushnew ,(string-downcase (symbol-name command-name))
                        *non-native-help-commands* :test #'equal)))
         ;; Define the command function:
         #+abcl
         (defun ,tlc-sym (line)
           (flet ((do-it ,lambda-list ,@body))
             (apply #'do-it (read-args-from-string line))))
         #-abcl
         (defun ,tlc-sym ,lambda-list 
             ,@body)
         ;; Always add command to *extended-repl-commands* (for SLIME
         ;; interface and more):
         (add-repl-command-spec
          `(,',command-name
            ,',tlc-sym 
            ;; documentation string:
            ,',(when (stringp maybe-doc) maybe-doc)
            ;; Help control:
            ,',(or
                ;; no help:
                (when (member ':no-help .options.) 
                  ':no-help)
                ;; add to native help:
                (when (member ':add-to-native-help .options.) 
                  ':add-to-native-help))
            ,',cl-user-fn-name
            ,*current-system-name*))
         ;; Add to the CL implemention's top-level, where possible:
         #+abcl
         (pushnew '(,command-name-string ,command-name-string ,tlc-sym ,maybe-doc)
                  top-level::*command-table*
                  :test #'string= :key #'first)
         #+allegro
         (top-level:alias ,(string-downcase (symbol-name command-name))
             ,lambda-list
           ,@body)
         ;; NOTE: Clozure CL evaluates spread command arguments (but not ones
         ;; in list command-form syntax). Our normal semantics are for
         ;; unevaluated command arguments always.  Most of the time it doesn't
         ;; matter, but some users might be surprized by the difference when
         ;; using Clozure CL (but we keep consistent with Clozure CL's other
         ;; toplevel commands).
         #+clozure
         (ccl::define-toplevel-command :global ,command-name ,lambda-list
                                       ,@body)
         ;; NOTE: ECL evaluates spread command arguments.  Most of the
         ;; time it doesn't matter, but some users might be surprized by
         ;; the difference when using ECL (but we keep consistent with
         ;; ECL's other toplevel commands).
         #+ecl
         (progn
           ;; Extend ECL's commands for extended-REPL command use:
           (pushnew '((,command-name) ,tlc-sym :eval
                      ,@(if (stringp maybe-doc)
                            (list (format nil "~s~16,8t~a"
                                   command-name
                                   maybe-doc)
                             maybe-doc)
                            '("" "")))
                    ;; Place in the extended REPL topic area:
                    (cdr (second si::*tpl-commands*))
                    :test #'equal
                    :key #'car))
         #+lispworks
         (system:define-top-loop-command ,command-name ,lambda-list
           (progn ,@body))
         #+xcl
         (progn
           (pushnew
            '(,(string-downcase (symbol-name command-name))
              nil
	      ,command-fn-sym
              ,(when (stringp maybe-doc) 
                 ;; Match XCL's non-captialization:
                 (string-downcase maybe-doc :end 1)))
            top-level::*command-table*
            :test #'string=
	    :key #'first)
           (sort top-level::*command-table* #'string< :key #'first))
         ',command-name))))

(compile-if-advantageous 'define-repl-command)

;;; ---------------------------------------------------------------------------

(defun list-all-extended-repl-systems ()
  ;;; Return a list of all extended-REPL systems, as computed from REPL
  ;;; commands and Module Manager definitions.
  (let ((result nil))
    (dolist (command-spec *extended-repl-commands*)
      (destructuring-bind (command function doc help-control 
                           cl-user-fn-name command-system-name)
          command-spec
        (declare (ignore command function doc help-control cl-user-fn-name))
        (declare (symbol command-system-name))
        (when command-system-name
          (pushnew command-system-name result))))
    (when (find-package ':module-manager)
      (setf result 
            (nunion result
                    (funcall 
                     (fdefinition (intern (symbol-name '#:list-all-systems)
                                          ':module-manager))))))
    result))

(compile-if-advantageous 'list-all-extended-repl-systems)

;;; ---------------------------------------------------------------------------

(defun sorted/filtered-extended-repl-commands ()
  (sort (remove-if
         #'(lambda (command-spec)
             (destructuring-bind (command function doc help-control
                                  cl-user-fn-name system-name)
                 command-spec
               (declare (ignore command function doc 
                                cl-user-fn-name system-name))
               (eq help-control ':no-help)))
         *extended-repl-commands*)
        #'string<
        :key #'first))

(compile-if-advantageous 'sorted/filtered-extended-repl-commands)

;;; ---------------------------------------------------------------------------

(defun show-all-extended-repl-commands (&optional system-name)
  ;;; Display all extended-REPL commands that are not marked with :no-help
  ;;; control.  Use `system-name' as a filter (:none matches commands without
  ;;; a specified system name).
  #+sbcl (declare (optimize (speed 1))) ; prevent SBCL generic-+ warnings:
  (cond 
   ((and system-name
         (not (member system-name (list-all-extended-repl-systems)))
         (not (eq system-name ':none)))
    (format t "~&;; System ~s was not found.~%"
            system-name))
   (t (let ((2nd-column 28))
        (if system-name
            (format t "~&System ~s Commands:~%" system-name)
            (format t "~&All Commands:~%"))
        (format t "~&Command~v,4tDescription~:*~
                   ~%-------~v,4t-----------~%"
                2nd-column)
        (dolist (command-spec (sorted/filtered-extended-repl-commands))
          (destructuring-bind (command function doc help-control 
                               cl-user-fn-name command-system-name)
              command-spec
            (declare (ignore function help-control cl-user-fn-name))
            (when (or (not system-name)
                      (eq system-name command-system-name)
                      (and (eq system-name ':none)
                           (not command-system-name)))
              (format t "~&~s~v,4t~@[~a~]~%"
                      command
                      2nd-column
                      doc))))))))

(compile-if-advantageous 'show-all-extended-repl-commands)

;;; ---------------------------------------------------------------------------

(defun show-all-extended-repl-systems ()
  ;;; Display all extended-REPL systems, as computed from REPL commands
  ;;; and Module Manager definitions.
  (dolist (system-name (sort (list-all-extended-repl-systems) #'string<))
    (format t "~&~s~%" system-name))
  (values))

(compile-if-advantageous 'show-all-extended-repl-systems)

;;; ---------------------------------------------------------------------------

(defun undefine-system-repl-commands (system-name)
  ;;; Removes all extended-REPL commands tagged with `system-name'.
  (setf *extended-repl-commands*
        (delete-if 
         #'(lambda (command-spec)
             (destructuring-bind (command-name function doc help-control
                                  cl-user-fn-name this-system-name)
                 command-spec
               (declare (ignore #-(or allegro clozure lispworks xcl)
                                command-name
                                function doc help-control))
               ;; Deleting?
               (when (eq this-system-name system-name)
                 #+allegro
                 (let ((*terminal-io* excl::*null-stream*))
                   (top-level:remove-alias
                    (string-downcase (symbol-name command-name))))
                 #+clozure
                 (let ((global-toplevel-commands-acons
                        (assoc ':global ccl::*defined-toplevel-commands*)))
                   (setf (cdr global-toplevel-commands-acons)
                         (delete command-name 
                                 (cdr global-toplevel-commands-acons)
                                 :test #'eq
                                 :key #'car)))
                 #+ecl
                 (let ((extended-tpl-commands (second si::*tpl-commands*)))
                   (setf (cdr extended-tpl-commands)
                         (delete command-name 
                                 (cdr extended-tpl-commands)
                                 :test #'eq
                                 :key #'caar)))                 
                 #+lispworks
                 (system::%put command-name 'system::top-loop-handler nil)
                 #+xcl
                 (setf top-level::*command-table*
                       (delete (string-downcase (symbol-name command-name))
                               top-level::*command-table*
                               :test #'string=
                               :key #'first))
                 ;; Also delete the :cl-user function?
                 (when cl-user-fn-name
                   (fmakunbound cl-user-fn-name))
                 ;; please do the delete:
                 't)))
         (the list *extended-repl-commands*))))

(compile-if-advantageous 'undefine-system-repl-commands)

;;; ---------------------------------------------------------------------------

(defun do-undefine-system-repl-command (system-name)
  ;;; Implements the :undefine-system <system-name> REPL command:
  (cond
   ((and system-name
         (cond 
          ((not (member (the symbol system-name)
                        (list-all-extended-repl-systems)))
           (format t "~&;; System ~s was not found.~%"
                   system-name)
           ;; don't proceed further:
           nil)
          ;; confirm with the user:
          ((y-or-n-p "Really undefine commands, directories, & modules ~
                          of ~s?  "
                     system-name))))
    (undefine-system-repl-commands system-name)
    (when (find-package ':module-manager)
      (funcall 
       (fdefinition
        (intern (symbol-name '#:undefine-system-directories-and-modules)
                ':module-manager))
       system-name))
    (format t "~&;; System ~s undefined.~%" system-name))
   (t (format t "~&;; Nothing was undefined.~%"))))

(compile-if-advantageous 'do-undefine-system-repl-command)

;;; ---------------------------------------------------------------------------
;;;  Interface into CLISP's *user-commands* facility
;;;
;;; Currently there is no way to read remaining values from the RELP read-line
;;; string in CLISP (but maybe in 2.45!)

#+clisp
(defun user-commands ()
  (mapcan 
   #'(lambda (command-spec)
       (destructuring-bind (command function doc help-control 
                            cl-user-fn-name system-name)
	   command-spec
         (declare (ignore cl-user-fn-name system-name))
	 (let* ((command-name (concatenate 'simple-string 
                                ":" 
                                (string-downcase (symbol-name command))))
                (command-fn-pair
                 `(,command-name 
                   . ,#'(lambda (&optional .string.)
                          ;; Pre-2.45 CLISP's will not have a
                          ;; .string. argument:
                          (if .string.
                              (apply function
                                     (with-input-from-string (stream .string.)
                                       (loop
                                           for form = (read stream nil stream)
                                           until (eq form stream)
                                           collect form)))
                              (funcall function))))))
           `(,command-fn-pair
             ;; native-help command documentation:
             ,@(when (eq help-control ':add-to-native-help)
                 ;; Keep the 2nd column at 24 to be consistent with CLISP's
                 ;; own help format:
                 (list (format nil "~%~a~24,4t~a" command-name doc)))))))
   (sort (copy-list *extended-repl-commands*) #'string< :key #'first)))

#+clisp
(compile-if-advantageous 'user-commands)

#+clisp
(pushnew #'user-commands custom:*user-commands*)

;;; ---------------------------------------------------------------------------
;;; CMUCL doesn't provide an extension hook in either %top-level or
;;; interactive-eval.  So, we resort to shadowing the original
;;; interactive-eval function:

#+cmu
(setf (fdefinition 'original-interactive-eval)
      (fdefinition 'ext:interactive-eval))
 
#+cmu
(lisp::without-package-locks  
 (defun ext:interactive-eval (form)
   (let ((in *standard-input*)
	 (repl-command (get-extended-repl-command form)))
     (flet ((do-command (symbol-or-fn args)
	      (apply (the function (if (symbolp symbol-or-fn) 
				       (fdefinition symbol-or-fn)
				       symbol-or-fn))
		     args)
	      ;; always return nil
	      nil))
       (cond (repl-command
              (do-command (second repl-command)
		(loop 
                  while (ext:listen-skip-whitespace in)
                    collect (read in nil)))
	      (values))
	     ;; Support (<command> <arg>*) syntax as well:
	     ((and (consp form)
		   (setf repl-command (get-extended-repl-command (car form))))
	      (do-command (second repl-command) (cdr form))
	      (values))
	     (t (original-interactive-eval form))))))
 
 (compile-if-advantageous 'ext:interactive-eval))

;;; ---------------------------------------------------------------------------
;;; The Scieneer CL 1.3 doesn't provide an extension hook in either %top-level
;;; or interactive-eval.  So, we resort to shadowing the original
;;; interactive-eval function:

#+scl
(setf (fdefinition 'original-interactive-eval)
      (fdefinition 'ext:interactive-eval))
 
#+scl
(defun ext:interactive-eval (form)
  (let ((in *standard-input*)
	(repl-command (get-extended-repl-command form)))
    (flet ((do-command (symbol-or-fn args)
	     (apply (the function (if (symbolp symbol-or-fn) 
				      (fdefinition symbol-or-fn)
				      symbol-or-fn))
		    args)
	     ;; always return nil
	     nil))
      (cond (repl-command
             (do-command (second repl-command)
               (loop 
                   while (ext:listen-skip-whitespace in)
                   collect (read in nil)))
	     (values))
	    ;; Support (<command> <arg>*) syntax as well:
	    ((and (consp form)
		  (setf repl-command (get-extended-repl-command (car form))))
	     (do-command (second repl-command) (cdr form))
	     (values))
	    (t (original-interactive-eval form))))))

#+scl
(compile-if-advantageous 'ext:interactive-eval)

;;; ---------------------------------------------------------------------------
;;; Extend SBCL's default form reader, SB-IMPL::REPL-READ-FORM-FUN, with a
;;; richer version that provides keyword-command capabilities:

#+sbcl
(progn
  (defun extended-repl-read-form-fun (in out)
  (declare (type stream in out) (ignore out))
  (let* ((eof-marker (cons nil nil))
	 (form (read in nil eof-marker)))
    (cond 
     ((eq form eof-marker) 
      (exit))
     (t (let ((repl-command (get-extended-repl-command form)))
	  (flet ((do-command (symbol-or-fn args)
		   (apply (the function (if (symbolp symbol-or-fn) 
					    (fdefinition symbol-or-fn)
					    symbol-or-fn))
			  args)
                   ;; always return nil
		   nil))
	    (cond
             (repl-command
              (do-command (second repl-command)
                (loop 
                  while (sb-int:listen-skip-whitespace in)
                    collect (read in nil)))
              '(values))
             ;; Support (<command> <arg>*) syntax as well:
             ((and (consp form)
                   (setf repl-command (get-extended-repl-command (car form))))
              (do-command (second repl-command) (cdr form))
              '(values))
             (t form))))))))

  (compile-if-advantageous 'extended-repl-read-form-fun)
  (setf sb-int:*repl-read-form-fun* #'extended-repl-read-form-fun))

;;; ---------------------------------------------------------------------------
;;;  Provide a standard way of quitting CL (for use in extended-REPL commands)

(defun extended-repl-quit-lisp (&rest args)
  #+sbcl
  (let ((swank-package (find-package :swank)))
    (when swank-package
      (let ((quit-lisp-fn (intern (symbol-name 'quit-lisp) swank-package)))
        (when (fboundp quit-lisp-fn)
          (funcall (fdefinition quit-lisp-fn))))))
  (apply
   ;; Avoid infinite #'quit recursion in Allegro:
   #+allegro #'exit
   ;; SBCL prefers EXIT:
   #+sbcl #'exit
   #-(or allegro sbcl) #'quit
   args))
  
(compile-if-advantageous 'extended-repl-quit-lisp)

;;; ===========================================================================
;;;  SLIME REPL Interface Setup

(defun load-slime-extended-repl ()
  (format t "~&;; Loading extended REPL command processing for SLIME...~%")
  (load (make-pathname :name "slime-extended-repl"
                       :type "lisp"
                       :defaults *gbbopen-install-root*)))

(compile-if-advantageous 'load-slime-extended-repl)

;;;  *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;;;  SLIME doesn't provide an easy means of using its *after-init-hook*
;;;  mechanism in advance of loading Swank.  The :swank package cannot be
;;;  created in advance, as the existence of the :swank package is used as an
;;;  indicator that Swank has been initialized.
;;;
;;;  We work around this by creating the :swank-backend package in advance of
;;;  swank loading (a package that will be used by the :swank package, once
;;;  the :swank package is created by defpackage), setting
;;;  swank-backend::*after-init-hook*, and exporting
;;;  swank-backend::*after-init-hook* so that it will become Swank's
;;;  *after-init-hook*.  This exporting strategy doesn't work with LispWorks
;;;  (defpackage :swank ...) handling of swank-backend::*after-init-hook*, so
;;;  on LispWorks we instead pre-create the :swank-loader package and then add
;;;  :after advice to a placeholder swank-loader::load-swank function that
;;;  acts as our hook.  This :after advice will remain once the actual
;;;  swank-loader::load-swank function is loaded and it will be invoked when
;;;  swank-loader::load-swank is called.
;;;
;;;  It would have been *SO* much easier if Swank simply imported and used a
;;;  better-named variable (such as *swank-after-init-hook*) from the :cl-user
;;;  package, allowing straightforward specification of after-init hooks in
;;;  advance of Swank loading.
;;;  *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

#-lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Package :swank is already present:
  (when (find-package ':swank)
    ;; If there is not a :swank-backend package, assume an outdated SLIME
    ;; installation and delete the :swank package to force a reload:
    (unless (find-package ':swank-backend)
      (format t "~&;; Deleting outdated ~s package for SLIME...~%" 
              ':swank)
      (delete-package ':swank)))
  ;; Now define the :swank-backend package, if needed:
  (unless (find-package ':swank-backend)
    (format t "~&;; Predefining ~s package for SLIME...~%" 
            ':swank-backend)
    (make-package ':swank-backend :use '(:common-lisp))))

#-lispworks
(cond
 ;; Swank is already present:
 ((find-package ':swank)
  (load-slime-extended-repl))
 ;; The :swank package doesn't exist yet:
 (t (export '(swank-backend::*after-init-hook*) ':swank-backend)
    (locally (declare (special swank-backend::*after-init-hook*))
      (if (boundp 'swank-backend::*after-init-hook*)
          (pushnew 'load-slime-extended-repl swank-backend::*after-init-hook*)
          (setf swank-backend::*after-init-hook* 
                '(load-slime-extended-repl))))))

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Define the :swank-loader package, if needed:
  (unless (find-package ':swank-loader)
    (format t "~&;; Predefining ~s package for SLIME...~%" 
            ':swank-loader)
    (make-package :swank-loader :use '(:common-lisp))))

#+lispworks
(progn
  ;; Predefine a placeholder swank-loader::load-swank function to add
  ;; advice to:
  (unless (fboundp 'swank-loader::load-swank)
    (defun swank-loader::load-swank (&rest args) args))
  ;; Add the advice:
  (defadvice (swank-loader::load-swank extended-repl :after)
      ()
    (load-slime-extended-repl)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

