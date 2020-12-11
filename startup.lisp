;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/startup.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Feb 23 00:57:22 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         GBBopen Startup File
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;;  For GBBopen porting information, refer to the file PORTING.
;;;
;;;  This file is used as source only.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-08-02 File created.  (Corkill)
;;;  04-14-04 Added MCL feature hacking to keep things simpler 
;;;           throughout.  (Corkill)
;;;  06-09-04 Added <homedir>/gbbopen-init.lisp file loading. (Corkill)
;;;  06-10-04 Moved the :gbbopen package definition here (from
;;;           gbbopen/preamble.lisp) to facilitate setting GBBopen parameters
;;;           in a user's <homedir>/gbbopen-init.lisp file.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  09-28-05 Added *preferred-browser*.  (Corkill)
;;;  03-11-06 Added *module-manager-compile-verbose* and 
;;;           *module-manager-load-verbose*.  (Corkill)
;;;  04-07-06 Added gbbopen-modules directory support.  (Corkill)
;;;  04-27-08 Added shared-gbbopen-modules directory support.  (Corkill)
;;;  10-15-09 Added XCL support.  (Corkill)
;;;  07-14-10 Added FUNCALL-IN-PACKAGE.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;  Control compilation of old event names (for backward compatibility --
;;;  disappearing soon...)

(declaim (special *include-old-event-names*))

;; This file also can establish the GBBopen install directory:
(when (or (not (boundp '*include-old-event-names*))
          *include-old-event-names*)
  (format t "~&;; Adding :old-event-names to ~s...~%"
          '*features*)
  (pushnew ':old-event-names *features*))
  
;;; ===========================================================================
;;;  Preferred browser setting (defaults) 

(defvar *preferred-browser* 
    ;; On Mac OSX we defer to the OS default browser:
    #+(or macosx darwin)
    "open"
    ;; Lispworks (non-Windows) and SBCL do not search PATH for programs, so
    ;; the path must be explicitly included in the preferred browser setting:
    #+(or (and lispworks (not win32)) sbcl) "/usr/bin/firefox"
    #-(or macosx darwin (and lispworks (not win32)) sbcl) "firefox")

;;; ---------------------------------------------------------------------------
;;;  Set/reset GBBopen install root (defvar is in initiate.lisp)

(declaim (special *gbbopen-install-root*))

;; This file also can establish the GBBopen install directory:
(unless (boundp '*gbbopen-install-root*)
  (setf *gbbopen-install-root* 
        (make-pathname :name nil
                       :type nil
                       :defaults *load-truename*)))

;;; ---------------------------------------------------------------------------
;;;  Show directory locations (if not done previously by initiate.lisp)

(declaim (special *gbbopen-startup-loaded*)) ; a redefinition-quiet DEFVAR
(unless (boundp '*gbbopen-startup-loaded*)
  (setf *gbbopen-startup-loaded* nil))

(unless *gbbopen-startup-loaded*
  (format t "~&;; GBBopen is installed in ~a~%"
          (namestring *gbbopen-install-root*))
  (format t "~&;; Your \"home\" directory is ~a~%"
          (namestring (user-homedir-pathname))))

;;; ---------------------------------------------------------------------------
;;;  Control gbbopen-modules directory processing (defvar is in initiate.lisp)

(declaim (special *skip-gbbopen-modules-directory-processing*))
(unless (boundp '*skip-gbbopen-modules-directory-processing*)
  (setf *skip-gbbopen-modules-directory-processing* nil))

;;; ---------------------------------------------------------------------------
;;;  Load required Corman Common Lisp patches

#+cormanlisp
(load (make-pathname :name "corman-patches"
                     :type "lisp" 
                     :defaults *gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  Define COMPILE-IF-ADVANTAGEOUS if it is not already present (from loading
;;;  initiate.lisp)
;;;
;;;  NOTE: Copy all changes to COMPILE-IF-ADVANTAGEOUS to initiate.lisp:

(unless (fboundp 'compile-if-advantageous)
  (defun compile-if-advantageous (fn-name)
    ;;;  CMUCL, Lispworks, and SBCL running in :interpret *evaluator-mode* can't
    ;;;  compile interpreted closures (so we avoid having `fn-name' any
    ;;;  definitions that are closures)
    ;;;
    ;;;  CMUCL, SCL, and XCL can't compile macro definitions (so we skip
    ;;;  compiling them)
    ;;; 
    ;;;  ECL's compiler is slow and creates temporary files, so we don't bother
    #+ecl (declare (ignore fn-name))
    #-ecl (unless (or #+(or cmu scl xcl) (macro-function fn-name))
            (compile fn-name))))

;;; ---------------------------------------------------------------------------
;;;  Define FUNCALL-IN-PACKAGE if it is not already present (from loading
;;;  extended-repl.lisp)

(unless (fboundp 'funcall-in-package)
  #+allegro
  (import '(excl::funcall-in-package))
  #-allegro
  (defun funcall-in-package (symbol package &rest args)
    (declare (dynamic-extent args))
    (let ((fn-symbol (find-symbol (symbol-name symbol) package)))
      (if fn-symbol
          (apply fn-symbol args)
          (error "Symbol ~s is not present in package ~s"
                 symbol
                 package))))
  #-allegro
  (compile-if-advantageous 'funcall-in-package))

;;; ---------------------------------------------------------------------------
;;;  Define EXTENDED-REPL-QUIT-LISP if it is not already present (from loading
;;;  extended-repl.lisp)

(unless (fboundp 'extended-repl-quit-lisp)
  (defun extended-repl-quit-lisp (&rest args)
    #+sbcl
    (let ((swank-package (find-package :swank)))
      (when swank-package
        (let ((quit-lisp-fn (intern (symbol-name 'quit-lisp) swank-package)))
          (when (fboundp quit-lisp-fn)
            (funcall quit-lisp-fn)))))
    (apply
     ;; Avoid infinite #'quit recursion in Allegro:
     #+allegro #'exit
     #-allegro #'quit
     args))
  
  (compile-if-advantageous 'extended-repl-quit-lisp))

;;; ---------------------------------------------------------------------------
;;;  Bootstrap-load the module-manager system from its location relative to the
;;;  GBBopen install root:

(load (make-pathname
       :name "module-manager-loader"
       :type "lisp"
       :directory `(,@(pathname-directory *gbbopen-install-root*)
                      "source" "module-manager")
       :version :newest
       :defaults *gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  Define and load the remaining GBBopen module definitions from
;;;  modules.lisp:

(module-manager:define-module :gbbopen-modules
  (:requires :module-manager)
  (:directory nil :up)
  (:files ("modules" :source)))

(module-manager:load-module :gbbopen-modules)

;;; ---------------------------------------------------------------------------
;;;  Define the :gbbopen-tools and :gbbopen packages here, to allow a user to
;;;  set GBBopen parameters in their <homedir>/gbbopen-init.lisp file
;;;  (loaded below):

(unless (find-package :gbbopen-tools)
  (make-package ':gbbopen-tools :use '(:common-lisp :module-manager)))

(unless (find-package :gbbopen)
  (make-package ':gbbopen :use '(:common-lisp :module-manager :gbbopen-tools)))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-init.lisp file (or compiled version) in the users
;;;  "home" directory, load it now:

(let ((gbbopen-init-namestring
       (namestring
        (make-pathname :name "gbbopen-init"
                       :type nil
                       :version ':newest
                       :defaults (user-homedir-pathname)))))
  (load gbbopen-init-namestring :if-does-not-exist nil))

;;; ===========================================================================
;;;  Load gbbopen-modules-directory processing, if needed:

(unless (fboundp 'process-gbbopen-modules-directory)
  (load (make-pathname 
         :name "gbbopen-modules-directory"
         :type "lisp"
         :defaults *gbbopen-install-root*)))

;;; ---------------------------------------------------------------------------
;;;  Process the modules.lisp file (if present) from each module directory
;;;  that is linked from GBBopen's shared-gbbopen-modules directory:

(unless *skip-gbbopen-modules-directory-processing*
  (process-shared-gbbopen-modules-directory "modules"))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-modules directory in the users "home" directory,
;;;  load the modules.lisp file (if present) from each module directory that
;;;  is linked from the gbbopen-modules directory:

(unless *skip-gbbopen-modules-directory-processing*
  (process-gbbopen-modules-directory "modules"))

;;; ---------------------------------------------------------------------------
;;;  Record that GBBopen's startup file has been loaded:

(setf *gbbopen-startup-loaded* 't)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
