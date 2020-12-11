;;;; -*- Mode:Common-Lisp; Package:MODULE-MANAGER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/module-manager/module-manager-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Sep  4 12:11:37 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Module-Manager System Bootstrap Loader
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill (incorporating some original ideas by 
;;;                          Kevin Gallagher and Zack Rubinstein)
;;;
;;; Copyright (C) 2002-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Stand-alone Use:
;;;
;;;    To use the Module-Manager System separate from the GBBopen Project
;;;    software tree, do the following:
;;;      1. Create a "root" directory to contain the module-manager software tree
;;;         (e.g., $ mkdir my-tree)
;;;      2. Create the module-manager portion of the source tree in that 
;;;         "root" directory  (e.g., $ cd my-tree; mkdir -p source/module-manager)
;;;      3. Copy the module-manager-loader.lisp, module-manager.lisp, and
;;;         module-manager-user.lisp files into the source/module-manager directory
;;;      4. Start your CL and then load the module-manager-loader.lisp file:
;;;          > (load "my-tree/source/module-manager/module-manager-loader")
;;;      5. Compile the :module-manager and :module-manager-user modules:
;;;          > (module-manager:compile-module :module-manager-user
;;;                                        :create-dirs :propagate)
;;;    The Module-Manager Facility can now be used stand-alone by loading
;;;    source/module-manager-loader.lisp as part of your Common Lisp
;;;    initialization.
;;;
;;; Porting Notice:
;;;
;;;    The defparameter forms (in this file) for:
;;;       *compiled-directory-name*
;;;       *compiled-file-type*
;;;    must be extended when porting to a new CL implementation.
;;;
;;;  This file defines the :module-manager package and the global variables
;;;  *compiled-directory-name* and *compiled-file-type*.  It then loads either
;;;  the source or compiled module-manager file (whichever is more recent).
;;;
;;;  This file is used as source only.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-11-04 Split from startup.lisp to support stand-alone use.  (Corkill)
;;;  05-22-05 Added ECL support.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  09-27-06 Added Intel Mac *compiled-directory-name* features (sometimes
;;;           best guesses).  (Corkill)
;;;  11-11-08 Unified compiled-directory naming.  (Corkill)
;;;  09-04-12 Add "-smp" designation to Allegro CL's CL-implementation-string,
;;;           when appropriate.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(unless (find-package ':module-manager)
  (make-package ':module-manager 
                :use '(:common-lisp)))

(in-package :module-manager)

(export '(need-to-port                  ; not documented
          ))

;;; ===========================================================================
;;;  Need-to-port reporting

(defun need-to-port-warning/error (entity error)
  (funcall (if error 'error 'warn)
           "~s needs to be defined for ~a~@[ running on ~a~].~
            ~@[~%(Please send this error message and the result of ~
            ~% evaluating (pprint *features*) to bugs@gbbopen.org.)~]"
           entity
           (lisp-implementation-type) 
           (machine-type)
           error))

;;; ---------------------------------------------------------------------------

(defmacro need-to-port (entity &optional only-warn)
  ;; Generate compile-time warnings of needed porting:
  (need-to-port-warning/error entity nil)
  ;; Error if called at run time:
  (unless only-warn
    `(need-to-port-warning/error ',entity t)))

;;; ===========================================================================
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (both Digitool MCL and pre-1.2 Clozure CL include the
;;; feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':digitool-mcl *features*))

;;; ---------------------------------------------------------------------------
;;; Add clozure feature to legacy OpenMCL:

#+(and openmcl (not clozure))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':clozure *features*))

;;; ===========================================================================
;;;  Compiled Directory Name
;;;
;;; The module-manager system uses a separate compiled directory tree for each
;;; CL implementation and version.  The following code creates a unique name
;;; for the root of this tree for a number of CL implementations.  If you use
;;; the module-manager system with another CL implementation, you should add
;;; that implementation to the CL-IMPLEMENTATION-VALUES function and e-mail
;;; the modifications to the GBBopen Project (bugs@GBBopen.org) for inclusion
;;; in future releases.
;;;
;;; The form of the compiled directory name is:
;;;        [<OS/platform>-]<cl-implementation>-[m-]-<cl-version-number>
;;; where:
;;;  * the optional OS/platform is present unless the compiled files are
;;;    identical on all platforms (e.g., CLISP)
;;;  * the optional "modern mode" indicator (m) is present if the CL
;;;    implementation is operating in non-ANSI case mode (case-sensitive
;;;    lower)
;;;
;;; OS/platform names:
;;;
;;;   "windows"       Windows/X86 (32-bit)
;;;   "windows-64"    Windows/X86 (64-bit)
;;;
;;;   "mac86"         Macintosh/X86 (32-bit) 
;;;   "mac86-64"      Macintosh/X86 (32-bit) 
;;;   "macppc"        Macintosh/PowerPC (32-bit) 
;;;   "macppc-64"     Macintosh/PowerPC (64-bit) 
;;;
;;;   "freebsd"       FreeBSD/X86 (32-bit)
;;;   "linux86"       Linux/X86 (32-bit)
;;;   "linux86-64"    Linux/X86 (64-bit)
;;;   "linuxppc"      Linux/PowerPC (32-bit) 
;;;   "linuxppc-64"   Linux/PowerPC (64-bit) 
;;;
;;;   "alpha"         Unix 5.1/HP Tru64 (Alpha) (32-bit)
;;;   "alpha-64"      Unix 5.1/HP Tru64 (Alpha) (64-bit)
;;;   "aix"           IBM AIX 5.1/PowerPC (32-bit)
;;;   "aix-64"        IBM AIX 5.1/PowerPC (64-bit)
;;;   "hpux"          HP-UX 11.00/PA-RISC (32-bit)
;;;   "hpux-64"       HP-UX 11.00/PA-RISC (64-bit)
;;;   "solaris86"     Solaris/X86 (32-bit)
;;;   "solaris86-64"  Solaris/X86 (64-bit)
;;;   "sparc"         Solaris/Sparc (32-bit)
;;;   "sparc-64"      Solaris/Sparc (64-bit)

(defun cl-implementation-values ()
  ;;; Returns 4 values:
  ;;;   1. OS/platform (string)
  ;;;   2. CL-implementation (string)
  ;;;   3. modern-mode-p indicator (boolean)
  ;;;   4. version (string or number)
  (flet (#+(or allegro clozure cmu ecl gcl lispworks sbcl scl xcl)
         (check (&rest args)
           ;; Ensure that only one string matched:
           (when (cdr args)
             (error
              "Multiple OS/platform strings, ~s, were defined ~
               for ~a~@[ running on ~a~]."
              args
              (lisp-implementation-type) 
              (machine-type)))
           ;; Return the sole-element string:
           (first args)))
    ;; Armed Bear Common Lisp:
    #+abcl
    (values 
     ;; Compiled files in ABCL are OS/platform independent
     nil         
     "abcl"
     nil
     (let ((version (lisp-implementation-version)))
       (subseq version 0 (position #\- version))))
    ;; Franz Allegro:
    #+allegro
    (let ((platform
           (check                       ; ensure one feature match
            #+alpha "alpha"
            #+alpha-64 "alpha-64"       ; just a guess
            #+prism "hpux"
            #+prism-64 "hpux-64"        ; just a guess
            #+sparc "sparc"
            #+sparc-64 "sparc-64"       ; just a guess
            #+freebsd "freebsd"         ; just a guess
            #+linux86 "linux86"
            #+linux86-64 "linux86-64" 
            #+(and x86 macosx) "mac86"
            #+(and x86-64 macosx) "mac86-64" ; just a guess
            #+(and x86 (not macosx) (not linux) (not freebsd)) "windows"
            #+(and x86-64 (not macosx) (not linux) (not freebsd)) "windows-64"
            #+powerpc "macppc"
            #+powerpc-64 "macppc-64")))   ; just a guess
      (values #+smp (concatenate 'string platform "-smp")
              #-smp platform
              "allegro"
              (eq excl:*current-case-mode* ':case-sensitive-lower) 
              excl::*common-lisp-version-number*))
    ;; CLISP:
    #+clisp
    (values 
     ;; Compiled files in CLISP are OS/platform independent
     nil         
     "clisp"
     nil
     (let ((version (lisp-implementation-version)))
       (subseq version 0 (position #\Space version))))
    ;; Clozure Common Lisp:
    #+clozure
    (values (check                      ; ensure one feature match
             #+darwinppc-target "macppc"
             #+darwinx8632-target "mac86"
             #+darwinx8664-target "mac86-64"
             #+linuxppc-target "linuxppc"
             #+linuxx8632-target "linux86"
             #+linuxx8664-target "linux86-64" ; Thanks to Matthew Danish
             #+windows-target "windows"
             #+win64-target "windows-64")
            "clozure"
            nil
            (format nil "~a.~a"
                    ccl::*openmcl-major-version*
                    ccl::*openmcl-minor-version*))
    ;; CMUCL:
    #+cmu
    (values (check                      ; ensure one feature match
             #+(and (not x86) darwin) "macppc"
             #+(and x86 darwin) "mac86"
             #+(and x86 linux) "linux86"
             #+(and x86 (not linux) (not darwin)) "windows"
             #+sparc "sparc")
            "cmucl"
            nil
            (let ((version (lisp-implementation-version)))
              (subseq version 0 (position #\Space version))))
    ;; Corman Common Lisp:
    #+cormanlisp
    (values "windows"
            "corman"
            nil
            (lisp-implementation-version))
    ;; Digitool MCL:
    #+digitool-mcl
    (values (check                      ; ensure one feature match
             #+powerpc "macppc")
            "mcl"
            nil
            (ccl::lisp-implementation-short-version))
    ;; ECL (Embedable Common Lisp):
    #+ecl
    (values (check                      ; ensure one feature match
             #+(and (or i686 i586 pentium4 pentium3) linux) "linux86" 
             #+(and (or i686 i586 pentium4 pentium3) darwin) "mac86" 
             #+(and (or i686 i586 pentium4 pentium3) 
                    (not (or linux darwin))) "windows"
             #+(and (not (or i686 i586 pentium4 pentium3)) darwin) "macppc")
            "ecl"
            nil
            ;; Strip away any CVS info:
            (let ((full-version (lisp-implementation-version)))
              (subseq full-version 0 (position '#\space full-version))))
    ;; GCL:
    #+gcl
    (values (check                      ; ensure one feature match
             #+linux "linux86" 
             #+sparc "sparc")
            "gcl"
            nil
            (format nil "~s.~s"
                    system::*gcl-major-version*
                    system::*gcl-minor-version*))
    ;; Lispworks:
    #+lispworks
    (values (check                      ; ensure one feature match   
             ;; LispWorks for HP PA:
             #+hppa "hpux"
             ;; LispWorks (32-bit) for Solaris/Sparc:
             #+(and sparc lispworks-32bit) "sparc"
             ;; LispWorks (64-bit) for Solaris/Sparc:
             #+(and sparc lispworks-64bit) "sparc-64"
             ;; LispWorks (32-bit) for Linux:
             #+(and :linux :lispworks-32bit) "linux86"
             ;; LispWorks (64-bit) for Linux:
             #+(and :linux :lispworks-64bit) "linux86-64"
             ;; LispWorks for FreeBSD:
             #+freebsd "freebsd"
             ;; LispWorks (32-bit) for Windows:
             #+(and :mswindows :lispworks-32bit) "windows"
             ;; LispWorks (64-bit) for Windows:
             #+(and :mswindows :lispworks-64bit) "windows-64"
             ;; LispWorks (32-bit) for Macintosh (running on X86):
             #+(and :darwin :x86 :lispworks-32bit) "mac86"
             ;; LispWorks (64-bit) for Macintosh (running on X86):
             #+(and :darwin :x86 :lispworks-64bit) "mac86-64"
             ;; LispWorks (32-bit) for Macintosh (running on PowerPC):
             #+(and :darwin :powerpc :lispworks-32bit) "macppc"
             ;; LispWorks (64-bit) for Macintosh (running on PowerPC):
             #+(and :darwin :powerpc :lispworks-64bit) "macppc-64")
            "lispworks"
            nil
            (format nil "~s.~s"
                    system::*major-version-number*
                    system::*minor-version-number*))
    ;; SBCL:
    #+sbcl
    (values (check                      ; ensure one feature match   
             #+(and x86-64 darwin) "mac86-64"
             #+(and x86 darwin) "mac86"
             #+(and ppc darwin) "macppc"
             #+(and x86 linux) "linux86"
             #+(and x86-64 linux) "linux86-64" ; Thanks to Eric Menard
             #+sparc "sparc"
             #+(and x86 (not linux) (not darwin)) "windows"
             #+(and x86-64 (not linux) (not darwin)) "windows-64")
            "sbcl"
            nil
            (lisp-implementation-version))
    ;; The Scieneer CL:
    #+scl
    (values (check                      ; ensure one feature match   
             #+(and x86 linux (not 64-bit)) "linux86"
             #+(and x86 linux 64-bit) "linux86-64"
             #+(and sparc solaris (not 64-bit)) "sparc"
             #+(and sparc solaris 64-bit) "sparc-64"
             #+(and x86 solaris (not 64-bit)) "solaris86"
             #+(and x86 solaris 64-bit) "solaris86-64"
             #+(and hpux (not 64-bit)) "hpux"
             #+(and hpux 64-bit) "hpux-64")
            "scl"
            (not (eq ext::*case-mode* ':upper))
            (lisp-implementation-version))
    ;; XCL:
    #+xcl
    (values (check                      ; ensure one feature match   
             #+(and x86 linux) "linux86"
             #+(and x86 (not linux)) "windows")
            "xcl"
            nil
            (lisp-implementation-version))))
  
;;; ---------------------------------------------------------------------------

(defparameter *compiled-directory-name*
    (multiple-value-bind (os/platform cl-implementation modern-mode-p version)
        (cl-implementation-values)
      ;; ABCL & CLISP compiled files are OS/platform independent:
      #-(or abcl 
            clisp)
      (unless os/platform
        ;; Unknown CL:
        (need-to-port-warning/error '*compiled-directory-name* nil))
      (format nil "~@[~a-~]~a-~:[~;m-~]~a"
              os/platform
              cl-implementation
              modern-mode-p
              version)))

;;; ===========================================================================
;;;  Compiled File Type
;;;
;;; The module-manager system needs to know the file type of compiled files.  The
;;; following form specifies the compiled-file type for a number of CL
;;; implementations. If you use the module-manager system with another CL
;;; implementation, you should add that implementation to the
;;; *compiled-file-type* form and e-mail the modified form to the GBBopen
;;; Project (bugs@GBBopen.org) for inclusion in future releases.

(defparameter *compiled-file-type*
    (or
     ;; Armed Bear Common Lisp:
     #+abcl
     system::*compile-file-type*
     ;; Franz Allegro:
     #+allegro
     excl:*fasl-default-type* 
     ;; CLISP:
     #+clisp
     (car custom:*compiled-file-types*)
     ;; Clozure Common Lisp:
     #+clozure
     (pathname-type ccl:*.fasl-pathname*)
     ;; CMUCL:
     #+cmu
     (c:backend-fasl-file-type c:*backend*)
     ;; Corman Common Lisp:
     #+cormanlisp
     "fasl"
     ;; Digitool MCL:
     #+digitool-mcl
     (pathname-type ccl:*.fasl-pathname*)
     ;; ECL (Embedable Common Lisp):
     #+ecl
     "fas"
     ;; GCL
     #+gcl
     "o"
     ;; Lispworks:
     #+lispworks
     compiler:*fasl-extension-string*
     ;; SBCL:
     #+sbcl
     sb-fasl:*fasl-file-type*
     ;; The Scieneer CL:
     #+scl
     (c:backend-fasl-file-type c:*backend*)
     ;; XCL:
     #+xcl
     "xcl"
     ;; Unknown CL:
     #-(or allegro 
           clisp
           clozure
           cmu
           cormanlisp
           digitool-mcl
           ecl
           lispworks
           sbcl
           scl
           xcl)
     (need-to-port *compiled-file-type*)))

;;; ===========================================================================
;;;  Load the module-manager system (source or compiled file)

(let* ((this-file-truename *load-truename*)
       (root-pathname 
        (make-pathname
         :name nil
         :type  nil
         :version :newest
         :directory (butlast (pathname-directory this-file-truename) 2)
         :defaults this-file-truename)))
  (flet ((load-source-or-compiled-file (name)
           (let* ((source-path 
                   (make-pathname
                    :name name
                    :type "lisp"
                    :directory `(,@(pathname-directory root-pathname)
                                   "source" "module-manager")
                    :version :newest
                    :defaults root-pathname))
                  (compiled-path
                   (make-pathname
                    :type *compiled-file-type*
                    :directory `(,@(pathname-directory root-pathname)
                                   ,*compiled-directory-name*
                                   "module-manager")
                    :defaults source-path))
                  (source-file-date 
                   (or (file-write-date source-path) 0))
                  (compiled-file-date 
                   (or (and (probe-file compiled-path)
                            (file-write-date compiled-path))
                       0)))
             ;; Load the compiled file unless the source file is newer:
             (load (if (> compiled-file-date source-file-date)
                       compiled-path
                       source-path)))))
    (load-source-or-compiled-file "module-manager")))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


