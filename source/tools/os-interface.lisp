;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/os-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul 29 17:48:38 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Operating System Extensions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-17-05 File created.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  07-10-08 Added SVN-VERSION.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*gbbopen-install-root*)))

;;; ---------------------------------------------------------------------------
;;;  Unify some implementation differences

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+abcl
   '()                                  ; nothing yet!
   #+allegro
   '(excl.osi:kill
     excl.osi:setenv
     system:getenv
     system:reap-os-subprocess)
   #+clisp
   '(ext:run-program)
   #+clozure
   '(ccl:external-process-input-stream
     ccl:external-process-output-stream
     ccl:getenv     
     ccl:run-program 
     ccl:setenv
     ccl:signal-external-process)
   #+cmu
   '(ext:run-program
     ext:process-input
     ext:process-kill
     ext:process-output)
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:getenv
     ccl:setenv)
   #+ecl
   '(si:getenv
     si:setenv)
   #+lispworks
   '(system::environment-variable)
   #+sbcl
   '(sb-ext:run-program 
     sb-ext:posix-getenv
     sb-ext:process-input 
     sb-ext:process-kill 
     sb-ext:process-output)
   #+scl
   '(ext:run-program
     ext:process-input
     ext:process-kill
     ext:process-output)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(browse-hyperdoc
            close-external-program-stream
            getenv                      ; not yet documented
            kill-external-program       ; not yet documented
            run-external-program
            svn-version)))

;;; ---------------------------------------------------------------------------

#+(or cmu sbcl scl)
(progn
  (defun external-process-input-stream (process)
    (process-input process))
 
  (defun external-process-output-stream (process)
    (process-output process)))

;;; ===========================================================================
;;;  Environment variables

#-(or allegro clozure ecl digitool-mcl lispworks sbcl)
(defun getenv (string)
  (declare (ignore string))
  (need-to-port getenv))

#+lispworks
(progn
  (defun getenv (string)
    (environment-variable string))
  
  (defcm getenv (string)
    `(environment-variable ,string)))

#+sbcl
(progn
  (defun getenv (string)
    (posix-getenv string))

  (defcm getenv (string)
    `(posix-getenv ,string)))

;; Allegro already defines (setf getenv), but it doesn't do unset (it fakes it
;; with a null string); we could write (setf getenv) to call unsetenv on those
;; Allegro CL platforms where it is available

#+(or clozure digitool-mcl)
(progn
  (defun %setenv (string nv)
    ;; Can't unset with nil, so we fake it with a null string:
    (setenv string (or nv ""))
    string)
  (defsetf getenv %setenv))

#+ecl
(defsetf getenv setenv)

#+lispworks
(defun (setf getenv) (nv string)
  (setf (environment-variable string) nv))

#+sbcl
(defun (setf getenv) (nv string)
  (declare (ignore nv string))
  ;; SBCL doesn't provide an ENV setter
  (need-to-port (setf getenv)))

;;; ===========================================================================
;;;  Run-external-program
;;;
;;;  Returns two values:
;;;   1) the io-stream, if appropriate
;;;   2) the process identifier, if available
;;;
;;;  Note that Lispworks and SBCL do not search PATH for programs, the path 
;;;  must be explicitly included in the `program' string.
;;;
;;;  Support for Corman Common Lisp and Digitool MCL is incomplete.

(defun run-external-program (program args &key (input ':stream)
                                               (output ':stream)
                                               wait)
  #+(or cormanlisp digitool-mcl)
  (declare (ignore input output wait))  
  #+allegro
  (progn
    ;; Clean up any stray processes (up to 5 -- aggressive, but usually what
    ;; the user needs):
    #+ignore
    (dotimes (i 5)
      (declare (fixnum i))
      (unless (reap-os-subprocess)
        (return)))
    (multiple-value-bind (io-stream error-stream process)
        (excl:run-shell-command 
         #+(and x86 (not unix))
         (apply #'concatenate 'string program args)
         #-(and x86 (not unix))
         (apply #'vector program program args)
         :input input
         :output output
         :wait wait)
      (declare (ignore error-stream))
      (values io-stream process)))
  #+clisp
  (values
   (run-program program 
                :arguments args
                :input input
                :output output
                :wait (or wait (eq input ':stream) (eq output ':stream)))
   ;; no process id is available in CLISP:
   nil)
  #+cormanlisp
  (progn
    (win32:shell (format nil "~A ~{~A ~}" program args))
    nil)
  #+digitool-mcl-not-yet
  (let ((process (ccl:do-shell-script (format nil "~A ~{~A ~}" program args))))
    (values nil process))
  #+ecl
  (progn
    (when wait
      (error "~s on ECL does not support :wait ~s"
             'run-external-program 't))
    (values (si:run-program program args
                            :input input
                            :output output)
            ;; no process id is available in ECL:
            nil))
  #+(and lispworks (not win32))
  (multiple-value-bind (io-stream error-stream process)
      (system:run-shell-command 
       `(,program ,@args)
       :input input
       :output output
       :wait wait)
    (declare (ignore error-stream))
    (values io-stream process))
  #+(and lispworks win32)
  ;; 'program' is duplicated here due to bug found in LWW 4.4.6 where 'program'
  ;; was not passed to ARGV when args as non NIL. The bug may or may not be
  ;; present in unix versions. Environment variable PATH *IS* searched.
  (system:open-pipe `(,program ,program ,@args) :direction :io)
  #+(or clozure cmu sbcl scl)
  (let ((process (run-program program args
                              :input input
                              :output output
                              :wait wait)))
    (values
     (when (or input output)
       (make-two-way-stream 
        (external-process-output-stream process)              
        (external-process-input-stream process)))
     process))
  #-(or allegro clisp clozure cmu digitool-mcl ecl lispworks sbcl scl)
  (need-to-port run-external-program))

;;; ---------------------------------------------------------------------------

(defun close-external-program-stream (stream)
  ;; CLISP requires individual closing of all 3 streams:
  #+clisp
  (progn
    (close (two-way-stream-input-stream stream))
    (close (two-way-stream-output-stream stream)))
  (prog1 (close stream)
    #+allegro
    (system:reap-os-subprocess)))
   
;;; ---------------------------------------------------------------------------

(defun kill-external-program (os-process &optional (signal-number 15))
  ;;; Note: user is responsbile for closing any external program streams of
  ;;; `os-process'
  #-(or allegro clozure cmu sbcl scl)
  (declare (ignore os-process signal-number))
  #+allegro
  (progn
    ;; Not supported on Windows, but we'll let Allegro generate the error:
    (kill os-process signal-number)
    (system:reap-os-subprocess :pid os-process))
  #+clozure
  (signal-external-process os-process signal-number)
  #+(or cmu scl)
  (process-kill os-process signal-number)
  #+sbcl
  (process-kill os-process signal-number)
  ;; Return t:
  't
  #-(or allegro clozure cmu sbcl scl)
  (need-to-port kill-external-program))

;;; ---------------------------------------------------------------------------

(defun browse-hyperdoc (symbol)
  (let ((url (hyperdoc-url symbol)))
    (cond 
     (url (run-external-program *preferred-browser* (list url))
          't)
     (t (values)))))

;;; ---------------------------------------------------------------------------

(defun svn-version (&key (directory (namestring *gbbopen-install-root*))
                         (program "svnversion"))
  (let ((stream (run-external-program 
                 program 
                 (list (etypecase directory
                         (string directory)
                         (pathname (namestring directory))
                         (keyword 
                          (namestring (get-root-directory directory))))))))
    (prog1 (when stream (string-trim " " (read-line stream nil nil)))
      (close-external-program-stream stream))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
