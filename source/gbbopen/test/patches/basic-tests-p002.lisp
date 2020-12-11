;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/patches/basic-tests-p002.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:23:18 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Simple Patch Test for GBBopen Trip-Test
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-23-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(start-patch (2 "06-23-08" 
                :author "Corkill"
                :description "A more complex patch example")
    (printv "More complex example patch started!"))

(eval-when (:compile-toplevel)
  (continue-patch
    (printv "Defining compile-time-only-macro-for-patch...")
    (defmacro compile-time-only-macro-for-patch (x)
      `',x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (continue-patch
   (printv "Defining macro-for-patch at compile & load time...")
   (defmacro macro-for-patch (x)
     `',x)))

(continue-patch
 (printv "Using macro-for-patch at load time...")
 (macro-for-patch abc))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (continue-patch
   (printv "Using macro-for-patch at compile & load time...")
   (macro-for-patch xyz)))

(eval-when (:compile-toplevel)
  (continue-patch
   (printv "Using compile-time-only-macro-for-patch...")
   (compile-time-only-macro-for-patch abc)))

(finish-patch
 (printv "More complex example patch finished!"))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
