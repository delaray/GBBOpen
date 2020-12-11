;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/commands.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue May 29 15:01:47 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  GBBopen Top-Level-Loop (REPL) Commands
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Loaded by initiate.lisp.  After loading, handy top-level-loop keyword
;;; commands, such as :gbbopen-tools, :gbbopen-core, :gbbopen-user,
;;; :gbbopen-test, :agenda-shell-user, and :agenda-shell-test are available on
;;; Allegro CL, CLISP, Clozure CL, CMUCL, ECL, Lispworks, SBCL, SCL, and XCL.
;;; GBBopen keyword commands are also supported in the SLIME REPL.
;;;
;;; In many CL implementations, commands with arguments can be specified in
;;; either list or spread notation.  However, Allegro CL, CLISP, Lispworks,
;;; and XCL do not support the list notation for commands.  For example:
;;;
;;;    > :gbbopen-test :create-dirs
;;;
;;; or (if not Allegro CL, CLISP, Lispworks, or XCL)
;;;
;;;    > (:gbbopen-test :create-dirs) 
;;;
;;; will compile and load GBBopen and perform a basic trip test.
;;;
;;; On all CL implementations, functions invoking each top-level command, such
;;; as GBBOPEN-TOOLS, GBBOPEN-CORE, GBBOPEN-USER, GBBOPEN-TEST,
;;; AGENDA-SHELL-USER and AGENDA-SHELL-TEST, are defined in the
;;; COMMON-LISP-USER package (using the same symbol-name as the keyword
;;; command).  For example:
;;;
;;;    > (gbbopen-test :create-dirs)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-04-05 Split out from initiate.lisp.  (Corkill)
;;;  08-20-05 Added :agenda-shell-user command.  (Corkill)
;;;  08-21-05 Added :multiprocessing-test command.  (Corkill)
;;;  10-08-05 Added :tutorial-example command.  (Corkill)
;;;  01-02-05 Changed :multiprocessing to :portable-threads.  (Corkill)
;;;  01-05-06 Added :portable-sockets command.  (Corkill)
;;;  11-13-06 Added :abort-ks-execution-example command.  (Corkill)
;;;  02-15-10 Added :double-metaphone command.  (Corkill)
;;;  03-09-10 Added :gbbopen-tools-user command.  (Corkill)
;;;  02-02-11 Added :streaming command. (Corkill)
;;;  02-28-11 Added :network-streaming command. (Corkill)
;;;  05-28-12 SBCL now prefers EXIT over QUIT.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;   Module Manager Bootstrapping Commands

(with-system-name (:module-manager)
  (define-repl-command :startup (&rest args)
    "Load GBBopen startup.lisp file"
    (apply 'startup-gbbopen args))
  
    (define-repl-command (:module-manager-user :no-help) (&rest options)
    "Compile and load Module Manager Facility (MMF) User module"
    (startup-module :module-manager-user options)))

;;; ===========================================================================
;;;   Useful GBBopen Commands

(with-system-name (:gbbopen)

;;; ---------------------------------------------------------------------------
;;;  GBBopen Tools

  (define-repl-command :gbbopen-tools (&rest options)
    "Compile and load GBBopen Tools module"
    (startup-module :gbbopen-tools options :gbbopen-tools))

  (define-repl-command :gbbopen-tools-user (&rest options)
    "Compile and load GBBopen-Tools-User module"
    (startup-module :gbbopen-tools-user options :gbbopen-tools-user))

  (define-repl-command :portable-threads (&rest options)
    "Compile and load Portable Threads module"
    (startup-module :portable-threads options))

  (define-repl-command :portable-sockets (&rest options)
    "Compile and load Portable Sockets module"
    (startup-module :portable-sockets options))

  (define-repl-command :http-services (&rest options)
    "Compile and load HTTP and HTML Services module"
    (startup-module :http-services options))

  (define-repl-command :double-metaphone (&rest options)
    "Compile and load Double-MetaPhone module"
    (startup-module :double-metaphone options))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Core

  (define-repl-command :gbbopen-user (&rest options)
    "Compile and load GBBopen-User module"
    (startup-module :gbbopen-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Agenda Shell

  (define-repl-command :agenda-shell-user (&rest options)
    "Compile and load Agenda-Shell-User module"
    (startup-module :agenda-shell-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Compile All GBBopen Modules

  (define-repl-command :compile-gbbopen (&rest options)
    "Compile all GBBopen modules and exit Common Lisp"
    (startup-module :compile-gbbopen 
                    (list* ':create-dirs ':noautorun options) :gbbopen-user))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Example Modules

(with-system-name (:gbbopen-examples)

  (define-repl-command :tutorial-example (&rest options)
    "Compile and load GBBopen Tutorial-Example module"
    (startup-module :tutorial-example options :tutorial))

  (define-repl-command :abort-ks-execution-example (&rest options)
    "Compile and load the Abort-KS-Execution Example module"
    (startup-module :abort-ks-execution-example options :gbbopen-user))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Test Modules

(with-system-name (:gbbopen-tests)
  
  (define-repl-command :gbbopen-test (&rest options)
    "Compile and load GBBopen Test module"
    (startup-module :gbbopen-test options  :gbbopen-user))

  (define-repl-command :timing-tests (&rest options)
    "Compile and load Timing Tests module"
    (startup-module :timing-tests options  :gbbopen-user))

  (define-repl-command :portable-threads-test (&rest options)
    "Compile and load Portable-Threads Test module"
    (startup-module :portable-threads-test options :portable-threads-user))

  (define-repl-command :portable-sockets-test (&rest options)
    "Compile and load Portable-Sockets Test module"
    (startup-module :portable-sockets-test options :portable-sockets-user))

  (define-repl-command :os-interface-test (&rest options)
    "Compile and load OS-Interface-Test module"
    (startup-module :os-interface-test options :os-interface-user))

  (define-repl-command :gbbopen-tools-test (&rest options)
    "Compile and load GBBopen-Tools Test module"
    (startup-module :gbbopen-tools-test options :gbbopen-tools-user))
  
  (define-repl-command :double-metaphone-test (&rest options)
    "Compile and load Double-MetaPhone-Test module"
    (startup-module :double-metaphone-test options :gbbopen-tools-user))
  
  (define-repl-command :aes-test (&rest options)
    "Compile and load AES-Test module"
    (startup-module :aes-test options :gbbopen-tools-user))
  
  (define-repl-command :agenda-shell-test (&rest options)
    "Compile and load Agenda-Shell-Test module"
    (startup-module :agenda-shell-test options :gbbopen-user))

  ;; end with-system-name
  )
  
;;; ---------------------------------------------------------------------------
;;;  Extensions (not yet completed or documented)

(with-system-name (:gbbopen-extensions)

  (define-repl-command :multinode (&rest options)
    "Compile and load GBBopen multi-node support (under construction)"
    (startup-module :multinode options nil))

  (define-repl-command :streaming (&rest options)
    "Compile and load journaling support [Experimental]"
    (startup-module :streaming options nil))

  (define-repl-command :network-streaming (&rest options)
    "Compile and load network streaming support [Experimental]"
    (startup-module :network-streaming options nil))

  #+not-yet
  (define-repl-command :restricted-eval (&rest options)
    "Compile and load GBBopen restricted-eval support (under construction)"
    (startup-module :restricted-eval options :gbbopen-tools))

  (define-repl-command :web-inspector (&rest options)
    "Compile and load Web inspector (under construction)"
    (startup-module :web-inspector options nil))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Timing

(with-system-name (:timing)

  (define-repl-command :cl-timing (&rest options)
    "Compile and load CL timing tests (under construction)"
    (startup-module :cl-timing options :gbbopen-tools-user))

  ;; end with-system-name
  )

;;; ===========================================================================
;;;   Module Manager Commands
  
(with-system-name (:module-manager)
  
  (define-repl-command (:lm :add-to-native-help)
      (&rest module-name-and-options)
    "Load module"
    (startup-gbbopen)
    (funcall-in-package '#:do-module-manager-repl-command ':module-manager
                        ':lm module-name-and-options))
  
  (define-repl-command (:cm :add-to-native-help)
      (&rest module-name-and-options)
    "Compile and load module"
    (startup-gbbopen)
    (funcall-in-package '#:do-module-manager-repl-command ':module-manager
                        ':cm module-name-and-options))
  
  (define-repl-command (:lmf :add-to-native-help)
      (&rest module-name-and-options)
    "Load module file"
    (startup-gbbopen)
    (funcall-in-package '#:do-module-manager-repl-command ':module-manager
                        ':lmf module-name-and-options))
  
  ;; end with-system-name
  )

;;; ===========================================================================
;;;   Additional Useful Commands
  
(with-system-name (:built-in)

  (define-repl-command (:ds :add-to-native-help) (&optional obj)
    "Describe object"
    (describe (eval obj)))
  
;;; ---------------------------------------------------------------------------

  (define-repl-command (:quit :add-to-native-help
                             #+(or clisp 
                                   clozure
                                   cmu
                                   digitool-mcl
                                   ecl
                                   lispworks
                                   sbcl
                                   scl)
                             :no-cl-user-function)
      (&rest args)
    "Exit Lisp" 
    (apply #'extended-repl-quit-lisp args))
  
  ;; Allegro CL, ECL, and XCL provide :exit commands already, but we still
  ;; define them here on all platforms for SLIME interface:
  (define-repl-command (:exit :add-to-native-help
                             #+(or allegro
                                   clisp
                                   sbcl)
                             :no-cl-user-function)
      (&rest args)
    "Exit Lisp" 
    (apply #'extended-repl-quit-lisp args))
  
;;; ---------------------------------------------------------------------------
  
  ;;  Allegro CL provides :pa, but we repeat for SLIME interface:
  (define-repl-command (:pa :add-to-native-help) (&optional package)
    "Set/show current package"
    ;; Eval specials, on REPLs that don't normally do evaluation:
    (setf package (eval-special-repl-variable package))
    (when package
      ;; Package change:
      (set-repl-package package))
    ;; Package show:
    (format t "~&;; The ~s package is current.~%" 
            (package-name *package*)))
  
;;; ---------------------------------------------------------------------------
  
  ;;  Add :a (:abort) as :reset alias in XCL:
  #+xcl
  (define-repl-command (:a :add-to-native-help) ()
    "Alias to :reset"
    (extensions:reset))
  
;;; ===========================================================================
;;;   Undefine system-name (commands, directories, & modules)
  
  (define-repl-command :undefine-system (&optional system-name)
    "Undefine a system (commands, directories, & modules)"
    (do-undefine-system-repl-command system-name))
  
;;; ===========================================================================
;;;   Help commands for all Extended REPL systems
  
  (define-repl-command (:systems :add-to-native-help) ()
    "Show all systems"
    (show-all-extended-repl-systems))
  
;;; ---------------------------------------------------------------------------
  
  (define-repl-command (:commands :add-to-native-help) (&optional system-name)
    "Show extended-REPL commands"
    (show-all-extended-repl-commands system-name))
  
;;; ---------------------------------------------------------------------------
;;;   Add :help command, where needed:
  
  #+(or clozure 
        sbcl)
  (define-repl-command :help ()
    "Show REPL commands"
    #+sbcl
    (show-all-extended-repl-commands)
    #+clozure
    (ccl::check-toplevel-command ':?))
  
;;; ---------------------------------------------------------------------------
;;;   Add :h abbreviated command, where needed:
  
  #+(or allegro
        sbcl)
  (define-repl-command (:h :no-help) ()
    #+allegro
    (top-level:do-command ':help)
    #+sbcl
    (show-all-extended-repl-commands))
  
;;; ---------------------------------------------------------------------------
;;;   Add :? abbreviated command, where needed:
  
  #+(or allegro
        sbcl)
  (define-repl-command (:? :no-help) ()
    #+allegro
    (top-level:do-command ':help)
    #+sbcl
    (show-all-extended-repl-commands))
  
    ;; end with-system-name
  )
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
