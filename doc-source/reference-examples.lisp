;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/doc-source/reference-examples.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:21:54 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *              Example Hyps (used in GBBopen Reference)
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
;;;  02-19-08 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

;;; ---------------------------------------------------------------------------

(defun location.x (location) (first location))
(defun location.y (location) (second location))

;;; ---------------------------------------------------------------------------

(define-unit-class hyp ()
  ((belief :initform 0.0)
   (location :initform nil)
   (velocity-range)
   (color)
   (classification :initform '(:car :truck :bus :motorcycle :train :duck-boat 
                               :lawn-mower :anything))
   (supporting-hyps 
    :link (hyp supported-hyps))
   (supported-hyps 
    :link (hyp supporting-hyps)))
  (:dimensional-values 
   (belief :point belief)
   (velocity-range :interval velocity-range)
   (color (:element eq) color)
   (classification (:element eq) :set classification)
   (x (:point fixnum) #'location.x location)
   (y (:point fixnum) #'location.y location))
  (:initial-space-instances (bb hyps)))

(defmethod print-instance-slots ((obj hyp) stream)
  (call-next-method)
  (when (and (slot-boundp obj 'location)
             (slot-boundp obj 'belief))
    (format stream " ~s ~s~@[ ~{[~s..~s]~}~]"
            (slot-value obj 'location)
            (slot-value obj 'belief)
            (when (slot-boundp obj 'velocity-range)
              (slot-value obj 'velocity-range)))))

;;; ---------------------------------------------------------------------------

(defun make-some-hyps ()
  (make-space-instance '(bb hyps)
      :make-parents 't
      :dimensions (dimensions-of 'hyp)
      :allowed-unit-classes '((hyp :plus-subclasses))
      :storage '(((hyp :plus-subclasses)
                  (x y) 
                  uniform-buckets :layout ((0 10000 100)
                                           (0 10000 100)))))
  (make-instance 'hyp :instance-name 183 :location '(1835 4791) :belief .82
                 :velocity-range '(0 35)
                 :color ':silver
                 :classification '(:car :truck :bus :duck-boat))
  (make-instance 'hyp :instance-name 231 :location '(1488 7405) :belief .63
                 :velocity-range '(0 8)
                 :color ':silver
                 :classification '(:car :truck :bus :motorcycle :duck-boat 
                                   :lawn-mower))
  (make-instance 'hyp :instance-name 233 :location '(1835 4791) :belief .89
                 :velocity-range '(5 35)
                 :color ':silver
                 :classification '(:car :truck :bus))
  (make-instance 'hyp :instance-name 311 :location '(896 388) :belief .68
                 :velocity-range '(0 6)
                 :color ':yellow
                 :classification '(:car :truck :bus :motorcycle :duck-boat 
                                   :lawn-mower))
  (make-instance 'hyp :instance-name 319 :location '(1835 8419) :belief .91
                 :velocity-range '(4 12)
                 :color ':red
                 :classification '(:motorcycle :lawn-mower))
  (make-instance 'hyp :instance-name 331 :location '(1835 8419) :belief .88
                 :velocity-range '(15 30)
                 :color ':yellow
                 :classification '(:truck :bus :duck-boat))
  (make-instance 'hyp :instance-name 335 :location '(1835 8419) :belief .92
                 :velocity-range '(15 35)
                 :color ':dark-blue
                 :classification '(:car))
  (make-instance 'hyp :instance-name 419 :location '(1835 4791) :belief .85
                 :velocity-range '(5 35)
                 :color ':red
                 :classification '(:car :truck) 
                 :supporting-hyps (list (find-instance-by-name 183 'hyp)
                                        (find-instance-by-name 233 'hyp))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================

