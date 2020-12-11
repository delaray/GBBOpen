;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-modules-directory.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Aug 13 16:31:01 2011 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                GBBopen-Modules Directory Processing
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;;   This file is loaded by initiate.lisp (and optionally by startup.lisp)
;;;   to process personal gbbopen-modules directory items:
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-08-02 Split out from startup.lisp and initiate.lisp.  (Corkill)
;;;  06-20-06 Add "pseudo symbolic-link" support (for operating systems that
;;;           do not provide symbolic links).  (Corkill)
;;;  03-29-08 Add PROCESS-GBBOPEN-MODULES-DIRECTORY rescanning.  (Corkill)
;;;  06-01-09 Add *IGNORED-GBBOPEN-MODULES-DIRECTORY-SUBDIRECTORIES* variable. 
;;;           (Corkill)
;;;  03-21-10 Add *GBBOPEN-MODULES-DIRECTORY-VERBOSE* and *SYM-FILE-VERBOSE*
;;;           variables.  (Corkill) 
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

#+debugging
(load (make-pathname :name "printv" :defaults *load-truename*))

;;; ---------------------------------------------------------------------------

(defvar *gbbopen-modules-directory-verbose* t)
(defvar *sym-file-verbose* nil)

;;; ---------------------------------------------------------------------------

(defvar *ignored-gbbopen-modules-directory-subdirectories*
    '(".svn"))

(defvar *loaded-gbbopen-modules-directory-files* nil)

;;; ---------------------------------------------------------------------------

(defun read-target-directory-specification (filename)
  ;;; Return the target-directory-specification (from a *.sym file) as a
  ;;; one-element list; or nil if none is found:
  (with-open-file (file filename)
    (let (line)
      (loop
	(setf line (ignore-errors (read-line file nil nil)))
	(unless line
          (format t "~&;; WARNING: Unable to read the target-directory in file ~a~%" 
                  (namestring filename))
	  (return-from read-target-directory-specification nil))
        (when (plusp (length line))
          (flet ((trim-spec (string)
                   (string-right-trim 
                    '(#\Space #\Tab #\Return #\Linefeed #\Newline #\Backspace #\Rubout)
                    string)))
            (case (aref line 0)
              ;; Skip comment lines:
              ((#\; #\#))
              ;; The target was specified as a string:
              (#\" (let ((string
                          (ignore-errors (read-from-string line nil nil))))
                     (return-from read-target-directory-specification 
                       (typecase string
                         ;; We got the expected string:
                         (string (list (trim-spec string)))
                         (otherwise 
                          (format t "~&;; WARNING: Unable to read target-directory specification in file ~a: ~a~%" 
                                  (namestring file)
                                  line)
                          ;; return nil
                          nil)))))
              (otherwise 
               (let ((target-directory (trim-spec line)))
                 (return-from read-target-directory-specification 
                   (list (cons filename target-directory))))))))))))
  
(compile-if-advantageous 'read-target-directory-specification)
  
;;; ---------------------------------------------------------------------------
                  
(defun process-the-gbbopen-modules-directory (modules-dir filename shared?)
  ;;; Common function to process GBBopen's shared-gbbopen-modules directory
  ;;; and a user's personal gbbopen-modules directory.
  (let* ((subdirs-pathname
	  #-(or abcl clozure lispworks xcl)
          (make-pathname
	   :directory (append (pathname-directory modules-dir)
			      #-(or allegro cmu scl)
			      '(:wild))
	   :defaults modules-dir)
          #+(or abcl clozure lispworks xcl)
          (make-pathname
           :name ':wild
	   :defaults modules-dir))
	 (pseudo-sym-link-paths
          (and
            ;; XCL requires the directory to be present:
            #+xcl (probe-directory modules-dir)
            (directory 
             (make-pathname
              :name ':wild
              :type "sym"
              :defaults modules-dir))))
	 (module-dirs
	  (append
           ;; Allegro CL includes regular files in directory-pathname listing,
           ;; so we remove them:
           #+allegro
           (delete-if #'pathname-type
                      (directory subdirs-pathname :directories-are-files nil))
           #+clozure
           (directory subdirs-pathname :directories 't)
           ;; CMUCL and SCL include regular files in directory-pathname
           ;; listing, so we remove them:
           #+(or cmu scl)
           (delete-if #'pathname-type (directory subdirs-pathname))
           ;; Lispworks includes regular files in directory-pathname
           ;; listing, so we remove them:
           #+lispworks
           (flet ((fn (pathname)
                    (not (eq (pathname-type pathname) ':unspecific))))
             (delete-if #'fn (directory subdirs-pathname)))
	   #-(or allegro clozure cmu lispworks scl)
           (and
             ;; XCL requires the directory to be present:
             #+xcl (probe-directory modules-dir)
             (directory subdirs-pathname))
           ;; Add in any *.sym file "pseudo" symbolic links:
	   (mapcan #'read-target-directory-specification
                   pseudo-sym-link-paths)))
         (now (get-universal-time)))
    (let ((any-found? nil)
          (load-type (cond ((string= filename "modules")
                            "module definitions")
                           ((string= filename "commands")
                            "command definitions")
                           (t filename))))
      (dolist (dir module-dirs)
        (let ((sym-filename nil))
          (when (consp dir)
            (destructuring-bind (the-sym-filename . target-directory)
                dir
              (setf sym-filename the-sym-filename
                    dir target-directory)))
          (let* ((pathname-directory (pathname-directory dir))
                 (dir-name (first (last pathname-directory))))
            (unless (member dir-name 
                            *ignored-gbbopen-modules-directory-subdirectories*
                            :test #'string-equal)
              (let ((pathname (make-pathname 
                               :name filename
                               :type "lisp"
                               :directory pathname-directory
                               :defaults dir)))
                (let* ((previous-load-time-acons 
                        (assoc pathname *loaded-gbbopen-modules-directory-files*
                               :test #'equal))
                       (previous-load-time (cdr previous-load-time-acons)))
                  (unless (and previous-load-time
                               (let ((file-write-date (file-write-date pathname)))
                                 (and file-write-date
                                      (locally ; avoid SBCL optimization warning: 
                                          (declare (notinline >))
                                        (not (> file-write-date 
                                                previous-load-time))))))
                    (when (and sym-filename
                               *sym-file-verbose*
                               *gbbopen-modules-directory-verbose*)
                      (format t "~&;; Pseudo (*.sym) link ~a --> ~a~%"
                              sym-filename
                              dir))
                    (cond
                     ;; No load-type file in this directory:
                     ((not (probe-file pathname))
                      (when *gbbopen-modules-directory-verbose*
                        (format t "~&;; No ~:[personal~;shared~] ~a file (~a.lisp) found in ~a~%" 
                                shared?
                                load-type
                                filename
                                (namestring dir))))
                     ((not any-found?)
                      (when *gbbopen-modules-directory-verbose*
                        (format t "~&;; Loading ~:[personal~;shared~] ~a from ~a...~%" 
                                shared?
                                load-type
                                (namestring modules-dir)))
                      (setf any-found? 't)))
                    (when (load (namestring pathname)
                                :if-does-not-exist nil)
                      (cond 
                       (previous-load-time-acons
                        (setf (cdr previous-load-time-acons) now))
                       (t (setf *loaded-gbbopen-modules-directory-files*
                                (acons 
                                 pathname now
                                 *loaded-gbbopen-modules-directory-files*))))))))))
          (unless (or any-found? *gbbopen-startup-loaded*)
            (when *gbbopen-modules-directory-verbose*
              (format t "~&;; No ~:[personal~;shared~] ~a were found in ~a.~%"
                      shared?
                      load-type 
                      (namestring modules-dir)))))))))
  
(compile-if-advantageous 'process-the-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(defun process-shared-gbbopen-modules-directory (filename)
  (let ((shared-modules-dir
         (make-pathname
          :name nil
          :type nil
          :directory (append (pathname-directory *gbbopen-install-root*)
                             '("shared-gbbopen-modules"))
          :defaults *gbbopen-install-root*)))
      (process-the-gbbopen-modules-directory shared-modules-dir filename 't)))

(compile-if-advantageous 'process-shared-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(defun process-gbbopen-modules-directory (filename)
  (let* ((user-homedir-pathname
	  ;; CMUCL uses a search list in (user-homedir-pathname), so we must
	  ;; fixate it using truename before proceeding:
	  (make-pathname :defaults (truename (user-homedir-pathname))))
	 (user-modules-dir
	  (make-pathname
	   :directory (append (pathname-directory user-homedir-pathname)
			      '("gbbopen-modules"))
	   :defaults user-homedir-pathname)))
    (process-the-gbbopen-modules-directory user-modules-dir filename nil)))

(compile-if-advantageous 'process-gbbopen-modules-directory)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
