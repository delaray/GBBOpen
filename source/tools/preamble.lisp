;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/preamble.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Sep 17 03:41:24 2011 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        GBBopen-Tools Preamble
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-15-04 File created.  (Corkill)
;;;  06-15-05 Added ADD-PACKAGE-NICKNAME.  (Corkill)
;;;  09-13-05 Added HYPERDOC-FILENAME.  (Corkill)
;;;  09-28-05 Added import of *PREFERRED-BROWSER* setting.  (Corkill)
;;;  01-09-08 Added SAFELY-SET-DISPATCH-MACRO-CHARACTER.  (Corkill)
;;;  01-26-08 Added ENSURE-PACKAGE.  (Corkill)
;;;  02-24-08 Added OBJECT-ADDRESS.  (Corkill)
;;;  01-30-11 Export ENSURE-PACKAGE.  (Corkill)
;;;  02-21-11 Added APPLY-WHEN-FBOUNDP and FUNCALL-WHEN-FBOUNDP.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':gbbopen-tools)
    (make-package ':gbbopen-tools 
                  :use '(:common-lisp))))

(in-package :gbbopen-tools)

;;; We require the :module-manager package for a few entities (see
;;; ../module-manager/module-manager.lisp for details):
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((module-manager-package (find-package ':module-manager)))
    (if module-manager-package
        (use-package (list module-manager-package))
        (let ((truename *load-truename*))
          (error "The file ~s is required by ~s"
                 (make-pathname 
                  :directory (append (butlast (pathname-directory truename)) 
                                     '("module-manager"))
                  :name "module-manager"
                  :defaults truename)
                 truename)))))

;;; ---------------------------------------------------------------------------
;;;  Import user's preferred browser setting

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*gbbopen-install-root*
            common-lisp-user::*preferred-browser*
            common-lisp-user::*inf-reader-escape-hook*
            module-manager:printv
            module-manager:printv-expander)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*gbbopen-install-root*      ; re-export, not documented
            add-package-nickname        ; not documented
            apply-when-fboundp          ; not yet documented
            ensure-package              ; not documented
            funcall-when-fboundp        ; not yet documented
            gbbopen-tools-implementation-version
            hyperdoc-filename           ; not yet documented
            hyperdoc-url                ; not yet documented
            object-address
            printv                      ; in module-manager, but part of tools
            printv-expander             ; in module-manager, but part of tools
                                        ; (not documented)
            with-gensyms
            with-once-only-bindings)))  ; not yet documented

(unless (boundp '*gbbopen-install-root*)
  (error "~s was not set." '*gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Tools version (read from ../../VERSION file)

(defun gbbopen-tools-implementation-version ()
  (with-open-file (version-file 
                   (make-pathname
                    :name "VERSION"
                    :type nil
                    :defaults *gbbopen-install-root*))
    (read version-file)))

;;; Added to *features* in epilogue.lisp:
(defparameter *gbbopen-tools-version-keyword* 
    ;; Support cross-case mode CLs:
    (read-from-string (format nil ":gbbopen-tools-~a" 
                              (gbbopen-tools-implementation-version))))

;;; ---------------------------------------------------------------------------

(defun print-gbbopen-tools-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  GBBopen Tools ~a
;;;
;;;    Developed and supported by the GBBopen Project (http:/GBBopen.org/)
;;;    (See http://GBBopen.org/downloads/LICENSE for license details.)
;;; ~72,,,'-<-~>~2%"
          (gbbopen-tools-implementation-version)))
  
(eval-when (:load-toplevel)
  (print-gbbopen-tools-herald))

;;; ===========================================================================
;;;  Ensure package (find-package with error check)

(defun ensure-package (package)
  (or (find-package package)
      (error "Package ~s does not exist" package)))

;;; ===========================================================================
;;;  Convenient package-nickname adder

(defun add-package-nickname (nickname package)
  (let ((package (ensure-package package))
        (nickname-package (find-package nickname)))
    (if nickname-package
        (unless (eq package nickname-package)
          (error "Another package is named ~s" nickname))
        (rename-package package
                        (package-name package)
                        (cons nickname (package-nicknames package))))))

;;; ===========================================================================
;;;  With-gensyms
;;;
;;; GBBopen-tools version of the widely used gensym binding macro
;;;
;;; Placed here to make this macro available ASAP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms ((&rest symbols) &body body)
    `(let ,(flet ((fn (symbol) `(,symbol (gensym))))
             (declare (dynamic-extent #'fn))
             (mapcar #'fn symbols))
       ,@body)))

;;; ===========================================================================
;;;  With-once-only-bindings  
;;;
;;; GBBopen's version of the "once-only" macro-writing macro which 
;;; ensures that the specified forms are only evaluated once and in the
;;; specified order.
;;;
;;; Placed here to make this macro available ASAP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-once-only-bindings ((&rest symbols) &body body)
    (let ((gensyms (flet ((fn (symbol)
                            (declare (ignore symbol))
                            (gensym)))
                     (declare (dynamic-extent #'fn))
                     (mapcar #'fn symbols))))
      `(let (,.(flet ((fn (gensym) `(,gensym (gensym))))
                 (declare (dynamic-extent #'fn))
                 (mapcar #'fn gensyms)))
         `(let (,,.(flet ((fn (symbol gensym) ``(,,gensym ,,symbol)))
                     (declare (dynamic-extent #'fn))
                     (mapcar #'fn symbols gensyms)))
            ,(let (,.(flet ((fn (symbol gensym) `(,symbol ,gensym)))
                       (declare (dynamic-extent #'fn))
                       (mapcar #'fn symbols gensyms)))
               ,@body))))))

;;; ---------------------------------------------------------------------------

(defmacro funcall-when-fboundp (symbol &rest args)
  (with-once-only-bindings (symbol)
    `(when (fboundp ,symbol)
       (funcall ,symbol ,@args))))

;;; ---------------------------------------------------------------------------

(defmacro apply-when-fboundp (symbol &rest args)
  (with-once-only-bindings (symbol)
    `(when (fboundp ,symbol)
       (apply ,symbol ,@args))))

;;; ===========================================================================
;;;  Object-address (can be useful in conjunction with printv)

(defun object-address (obj &optional hex-string-p)
  (let ((address #+abcl
                 (system:identity-hash-code obj)
                 #+allegro
                 (excl::pointer-to-address obj)
                 #+clisp
                 (system::address-of obj)
                 #+clozure
                 (ccl::%address-of obj)
                 #+cmu
                 (kernel:get-lisp-obj-address obj)
                 #+digitool-mcl
                 (ccl::%address-of obj)
                 #+ecl
                 (si:pointer obj)
                 #+lispworks
                 (system:object-address obj)
                 #+sbcl
                 (sb-kernel:get-lisp-obj-address obj) 
                 #+scl
                 (kernel:get-lisp-obj-address obj)
                 #+xcl
                 (system:address-of obj)
                 #-(or abcl
                       allegro
                       clisp
                       clozure
                       cmu
                       digitool-mcl
                       ecl
                       lispworks
                       sbcl
                       scl
                       xcl)
                 (need-to-port object-address)))
    (if hex-string-p
        (format nil "~x" address)
        address)))

;;; ===========================================================================
;;;   Dispatch-macro-character conflict checker

(defun safely-set-dispatch-macro-character (disp-char sub-char function
                                            &optional
                                            (readtable *readtable*))
  (declare (special *inf-reader-escape-hook*))
  (let ((existing-dispatch 
         (get-dispatch-macro-character disp-char sub-char readtable)))
    (unless (or (null existing-dispatch)
                (eq existing-dispatch function)
                (and (functionp existing-dispatch)
                     (eq (nth-value 
                          2 (function-lambda-expression existing-dispatch))
                         function))
                ;; On Corman Lisp, look if the dispatch function is the same
                ;; as the default (by checking against another unlikely macro
                ;; character):
                #+cormanlisp
                (eq existing-dispatch
                    (get-dispatch-macro-character #\# #\& readtable))
                #+cmu
                (eq existing-dispatch
                    (symbol-function 'lisp::dispatch-char-error))
                ;; On CCL, look if the dispatch function is the same as the
                ;; default (by checking against another unlikely macro
                ;; character) or the COCOA bridge Objective-C #@ reader:
                #+(or clozure digitool-mcl)
                (and (functionp existing-dispatch)
                     (member (nth-value 
                              2 (function-lambda-expression existing-dispatch))
                             '(ccl::|#@-reader|
                               ccl::|objc-#@-reader|)
                             :test #'eq))
                ;; On ECL, look if the dispatch function is the same as the
                ;; default (by checking against another unlikely macro
                ;; character):
                #+ecl
                (eq existing-dispatch
                    (get-dispatch-macro-character #\# #\& readtable))
                ;; On GCL, look if the dispatch function is the same as the
                ;; default (by checking against another unlikely macro
                ;; character):
                #+gcl
                (eq existing-dispatch
                    (get-dispatch-macro-character #\# #\& readtable)))
      (cond 
       ;; Allow an override (and warn), if the user has set up an
       ;; *inf-reader-escape-hook* and we are setting the inf-reader dispatch
       ;; (done in tools/declared-numerics.lisp):
       ((and (eql disp-char #\#)
             (eql sub-char #\@)
             *inf-reader-escape-hook* 
             (eq function 'inf-reader))
        (warn "Replacing existing dispatch-macro for ~c~c due to ~s value ~s"
              disp-char
              sub-char
              '*inf-reader-escape-hook*
              *inf-reader-escape-hook*))
       ;; Allow the user to continue:
       (t (cerror "Change and continue"
                  "An existing dispatch-macro for ~c~c is defined for ~a: ~s"
                  disp-char
                  sub-char
                  (lisp-implementation-type)
                  existing-dispatch))))
    ;; Check & warn if Allegro's read-only readtable is in play:
    #+allegro
    (if (eq *readtable* excl::std-lisp-readtable)
        (warn "Allegro's read-only restriction on the system readtable (~s) ~
               prevents setting dispatch-macro for ~c~c" 
              *readtable*
              disp-char
              sub-char)
        (set-dispatch-macro-character disp-char sub-char function readtable))
    #-allegro
    (set-dispatch-macro-character disp-char sub-char function readtable)))

;;; ===========================================================================
;;;   Hyperdoc lookup helper

(defun hyperdoc-filename (symbol)
  (namestring
   (merge-pathnames 
    (format nil "ref-~a.html"
            (let ((basename (string-downcase (symbol-name symbol))))
              (cond 
               ;; Global variables:
               ((eql #\* (aref basename 0))
                (format nil "~a-var" 
                        (subseq basename 
                                1 
                                (the fixnum
                                  (1- (the fixnum (length basename)))))))
               ;; Using ~a above handles keyword-symbol conversions
               ;; automatically...
               (t basename))))
    (load-time-value
     (compute-relative-directory :gbbopen-root '(:up "hyperdoc") nil)))))

;;; ---------------------------------------------------------------------------

(defun hyperdoc-url (symbol)
  (let ((filename (hyperdoc-filename symbol)))
    (when (probe-file filename)
      (format nil "file://~a" filename))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
