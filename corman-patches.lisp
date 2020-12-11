;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/corman-patches.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:52:20 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Required Patches for Corman Common Lisp
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;;  This file is used as source only (Corman Common Lisp compiles on load)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-26-05 File Created.  (Corkill)
;;;  09-18-06 Updated for Corman Common Lisp 3.0.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp)

;;; ---------------------------------------------------------------------------
;;;  Redefine machine-type to return a useful value:

(defun machine-type () "x86")

;;; ---------------------------------------------------------------------------
;;;  From Sys\structures.lisp
;;;
;;;  Support :conc-name string designators in defstruct (see patch below):
(defmacro defstruct (name-and-options &rest doc-and-slots)
  (let (name 
	options 
	doc-string 
	slot-descriptors
	(slot-count 0)
	struct-template-info
	constructor-name
	(boa-constructor-info nil)
	(conc-name nil)
	copier-name
	predicate-name
	accessor-name
	(print-function nil)
	(expressions nil)
	(struct-template-expressions nil)
	(struct-type nil)
	(named nil)
	(initial-offset 0)
	(base nil)
	(base-list nil)
	(included-options nil)
	struct-template
	(struct-template-sym (gensym))
	(num-included-slots 0)) 
    
    (if (symbolp name-and-options)
	(setq name name-and-options)
      (progn
	(if (or (not (consp name-and-options))
		(not (symbolp (car name-and-options))))
	    (error "Invalid syntax for defstruct name: ~A" name-and-options))
	(setq name (car name-and-options))
	(setq options (cdr name-and-options))))
    
    ;;(format t "Parsing structure: ~A~%" name)
    (setq conc-name (concatenate 'string (symbol-name name) "-"))
    (dolist (opt options)
      ;;(format t "option: ~A~%" opt)
      (cond
       ((keywordp opt)(if (eq opt ':named) (setf named t)))
       ((and (listp opt) (keywordp (car opt)))
	(case (car opt)
	      (:conc-name 
	       (if (cdr opt)
		   (setq conc-name 
			 (if (cadr opt) 
			     ;; Patched DDC -- Allow conc-name string designators:
			     #-original-version
 			     (if (symbolp (cadr opt)) 
				 (symbol-name (cadr opt))
			         (cadr opt))
			     #+original-version
			     (symbol-name (cadr opt))
			     ""))
		 (setq conc-name "")))
	      (:constructor 
	       (if (cdr opt) 
		   (if (cddr opt)
		       (push (list (cadr opt) (caddr opt)) boa-constructor-info)
		     (setq constructor-name (cadr opt)))))
	      (:copier (if (cdr opt) (setq copier-name (cadr opt))))
	      (:predicate (if (cdr opt) (setq predicate-name (cadr opt))))
	      (:include (setf base (cadr opt) included-options (cddr opt)))
	      (:print-function (if (cdr opt) (setq print-function (cadr opt))))
	      (:type (setf struct-type (cadr opt)))
	      (:initial-offset (setf initial-offset (cadr opt)))
	      (otherwise (error "Unknown defstruct option: ~A~%" (car opt)))))
       (t (error "Invalid defstruct option: ~A~%" opt))))	
    
    (if (and (null struct-type) (/= initial-offset 0))
	(error "If :INITIAL-OFFSET is specified in DEFSTRUCT, then :TYPE must also be specified."))
    
    (if (stringp (car doc-and-slots))
	(progn
	  (setq doc-string (car doc-and-slots))
	  (setq slot-descriptors (cdr doc-and-slots)))
      (setq slot-descriptors doc-and-slots))
    
    ;; add the doc string with structure attribute	
    (if doc-string
	(push 
	 `(setf (documentation ',name 'structure) ,doc-string) 
	 expressions))
    
    ;; if :include specified, add those slots now
    (if base
	(let* ((included-struct-template (get base ':struct-template)))
	  (unless included-struct-template
	    (error "Cannot :INCLUDE struct type ~S. No slot information was found." base))
	  (setf base-list (cons base (struct-template-base included-struct-template)))
	  (setf num-included-slots (struct-template-num-slots included-struct-template))
	  (dotimes (i num-included-slots)
	    (let* ((name (struct-template-slot-name included-struct-template i))
		   (default (struct-template-slot-default included-struct-template i))
		   (type (struct-template-slot-type included-struct-template i))
		   (ro-p (struct-template-slot-ro-p included-struct-template i))
		   (inline-p (struct-template-slot-inline-p included-struct-template i))
		   (override (member name included-options 
				     :key #'(lambda (obj) (if (symbolp obj) obj (car obj))))))
	      (if override
		  (setf default 
			(if (and (consp (car override))(consp (cdar override)))
			    (cadar override))))
	      (push name struct-template-info) 
	      (push default struct-template-info)
	      (push type struct-template-info)
	      (push ro-p struct-template-info)
	      (push inline-p struct-template-info)
	      (incf slot-count)))))
    
    ;; process slot options
    (dolist (opt slot-descriptors)
      ;;(format t "slot: ~A~%" opt)
      (incf slot-count)
      (cond
       ((symbolp opt)  
	(push opt struct-template-info)
	(push nil struct-template-info)
	(push t struct-template-info)
	(push nil struct-template-info)
	(push nil struct-template-info)
	;;(format t "parsed-slot: ~A ~A ~A ~A ~A~%"
	;;opt nil t nil nil)
	)
       ((consp opt)
	(let ((sym (car opt))
	      (slot-initializer (cadr opt))
	      (options (cddr opt))
	      (type t)
	      (read-only nil)
	      (inline nil))
	  (if (not (symbolp sym))
	      (error "Invalid slot descriptor: ~A~%" sym))
	  (if (or (not (constantp slot-initializer)) 
		  (functionp slot-initializer))
	      (setq slot-initializer (compile-form slot-initializer)))
	  (do ((o options (cddr o)))
	      ((null o) nil)
	      (case (car o)
		    (:type (setf type (cadr o)))
		    (:read-only (setf read-only (cadr o)))
		    (:inline (setf inline (cadr o)))
		    (otherwise (error "Unknown slot option: ~A" (car o)))))
	  ;;(format t "parsed-slot: ~A ~A ~A ~A ~A~%"
	  ;;sym slot-initializer type read-only inline)
	  (push sym struct-template-info)
	  (push slot-initializer struct-template-info)
	  (push type struct-template-info)
	  (push read-only struct-template-info)
	  (push inline struct-template-info)))
       (t (error "Invalid slot option: ~A~%" opt))))
    
    (setq struct-template 
	  (apply #'define-struct-template name (create-named-class name (mapcar 'find-class base-list)) struct-type 
		 base-list initial-offset slot-count (reverse struct-template-info)))
    
    ;; install template		
    (push
     `(setf (get ',name :struct-template) ,struct-template-sym)
     struct-template-expressions)
    
    ;; install print function		
    (when print-function
      (if (and (consp print-function) (eq (car print-function) 'lambda))
	  (setq print-function `(function ,print-function))
	(setq print-function `(quote ,print-function)))
      (push
       `(setf (get ',name :struct-print) 
	      ,print-function)
       expressions))
    
    ;; install constructor function
    (setq constructor-name
	  (if constructor-name 
	      (intern (symbol-name constructor-name))
	    (if boa-constructor-info			
		(make-symbol (concatenate 'string "MAKE-" (symbol-name name))) ;; invisible
	      (intern (concatenate 'string "MAKE-" (symbol-name name))))))
    
    (push
     `(create-keyword-constructor 
       ,constructor-name
       ,struct-template
       ,struct-template-sym
       ,name
       ,struct-type
       ,named)
     struct-template-expressions)
    
    (push
     `(setf (get ',name ':struct-constructor) ',constructor-name)
     expressions) 
    
    ;; install BOA constructor
    (dolist (boa-info boa-constructor-info)
      (push
       `(create-boa-constructor 
	 ,(car boa-info) 
	 ,(cadr boa-info)
	 ,struct-template
	 ,struct-template-sym
	 ,name
	 ,struct-type
	 ,named)
       struct-template-expressions))
    
    ;; install copier function			
    (setq copier-name
	  (if copier-name 
	      (intern (symbol-name copier-name))
	    (intern (concatenate 'string "COPY-" (symbol-name name)))))
    
    (push
     `(defun ,copier-name (arg) (clone-struct arg))
     expressions)
    
    ;; install predicate function			
    (setq predicate-name
	  (if predicate-name 
	      (intern (symbol-name predicate-name))
	    (intern (concatenate 'string (symbol-name name) "-P"))))
    
    (push
     (cond ((and named struct-type)
	    `(defun ,predicate-name (arg) 
	       (and (typep arg ',struct-type)
		    (eq (elt arg 0) ',name))))
	   (struct-type `(fmakunbound ',predicate-name))   ;; don't define a predicate
	   (t `(defun ,predicate-name (arg) (struct-type-p arg ',name))))
     expressions)
    
    ;; install type specifier
    (push
     `(cl::declare-type-specifier ,name (x specifier)
				  (declare (ignore specifier))
				  (struct-type-p x ',name))
     expressions)
    
    ;; install accessor functions
    (do* ((num-slots (struct-template-num-slots struct-template))
	  (i 0 (+ i 1)))  ;; skip over any included slots--they already have accessors
	 ((= i num-slots))
	 (setq accessor-name 
	       (intern 
		(concatenate 'string conc-name 
			     (symbol-name (struct-template-slot-name struct-template i)))))
	 (if (struct-template-slot-inline-p struct-template i)
	     (push `(proclaim '(inline ,accessor-name)) expressions))
	 (push (build-accessor name struct-type named initial-offset
			       accessor-name i (< i num-included-slots))
	       expressions)
	 (when (not (struct-template-slot-ro-p struct-template i))
	   (if (struct-template-slot-inline-p struct-template i)
	       (push 
		`(proclaim '(inline ,(setf-function-symbol (list 'setf accessor-name)))) 
		expressions))
	   (push (build-mutator name struct-type named initial-offset
				accessor-name i (struct-template-slot-type struct-template i) (< i num-included-slots))
		 expressions)))
    
    (push `',name expressions)	
    `(progn
       (let* ((,struct-template-sym
	       (apply 'define-struct-template ',name 
		      (create-named-class ',name 
					  (list ,.(mapcar 
						   #'(lambda (class-name) 
						       `(find-class ',class-name))
						   base-list))) 
		      ',struct-type 
		      ',base-list ,initial-offset ,slot-count ',(reverse struct-template-info)))) 
	 ,.(reverse struct-template-expressions))
       ,@(nreverse expressions))))

;;; ===========================================================================
;;;  Add :up & :back support to directory paths

(in-package :win32)

(defun hack-back/up (dir-list)
  (let ((result nil))
    (dolist (dir-item dir-list)
      (case dir-item
	;; treat :back & :up the same on Windows
	((:up :back)
	 (unless result (error "Cannot go ~s any further" dir-item))
	 (pop result))
	(otherwise (push dir-item result))))
    (nreverse result)))

;;; ---------------------------------------------------------------------------
;;;  From Sys\directory.lisp

(defun subdirectory-pathname 
    (directory-pathname subdir-name &key (name nil) (type nil) (version nil))
  (let ((p1 (parse-namestring subdir-name)))
    (truename
     (make-pathname
      :host (pathname-host directory-pathname)
      :device (pathname-device directory-pathname)
      :directory 
      (append (hack-back/up		; patched DDC
	       (pathname-directory directory-pathname))
	      (list (if (pathname-type p1)
			(format nil "~A.~A" (pathname-name p1) (pathname-type p1))
			(pathname-name p1))))
      :name name
      :type type
      :version version
      ))))

(defun file-pathname (directory-pathname file-name &key (version nil))
  (let ((p1 (parse-namestring file-name)))
    (truename
     (make-pathname
      :host (pathname-host directory-pathname)
      :device (pathname-device directory-pathname)
      :directory (hack-back/up 		; patched DDC
		  (pathname-directory directory-pathname))
      :name (pathname-name p1)
      :type (pathname-type p1)
      :version version
      ))))

;;; ---------------------------------------------------------------------------
;;;  From Sys\filenames.lisp

(in-package :pathnames)

(defun convert-pathname-to-namestring (pathname)
  (let ((device (pathname-internal-device pathname))
	(directory (win32::hack-back/up	; patched DDC
		    (pathname-internal-directory pathname)))
	(type (pathname-internal-type pathname))
	(name (pathname-internal-name pathname)))
    (format nil "~{~A:~}~{~A~}~{~A\\~}~{~A~}~{.~A~}"
	    (if device (list device) nil)
	    (if (eq (car directory) :absolute)
		'(#\\) nil)
	    (cdr directory)
	    (if name (list name) nil)
	    (if type (list type) nil))))

(defun convert-pathname-to-directory-namestring (pathname)
  (let ((device (pathname-internal-device pathname))
	(directory (win32::hack-back/up	; patched DDC
		    (pathname-internal-directory pathname))))
    (format nil "~{~A:~}~{~A~}~{~A\\~}"
	    (if device (list device) nil)
	    (if (eq (car directory) :absolute)
		'(#\\) nil)
	    (cdr directory))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
