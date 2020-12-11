;;;; -*- Mode:COMMON-LISP; Package:TUTORIAL; Base:10 -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/examples/tutorial.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Mar 30 13:55:00 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *              GBBopen Tutorial "Random Walk" Application
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
;;; Runs once (automatically) when loaded using the module-manager system.
;;;
;;; To run manually:
;;;   1. Compile and load the :tutorial-example module
;;;   2. Set the current package to :tutorial
;;;   3. Evaluate: (take-a-walk)
;;;
;;; Use (show-important-events) [default] and (hide-important-events) to
;;; toggle event printing.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  10-08-05 Adapted from the `Getting Started with GBB' example.  (Corkill)
;;;  08-31-08 Converted to GBBopen 1.2.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':tutorial)
    (make-package ':tutorial
                  :use '(:common-lisp :module-manager :portable-threads
                         :gbbopen-tools :gbbopen :agenda-shell))))

(in-package :tutorial)

;;; ------------------------------------------------------------------------

(defvar *the-random-walk* nil
  "Stores the random-walk unit instance.")

(defparameter *center-quadrant-pattern* 
    '(within (x y) ((-25 25) (-25 25))))

;;; ===========================================================================
;;;   Unit Class Definitions

(define-unit-class location ()
  (time
   x
   y
   (next-location
    :link (location previous-location :singular t) 
    :singular t)
   (previous-location
    :link (location next-location :singular t)  
    :singular t)
   (path
    :link (path locations)
    :singular t))
  (:dimensional-values
   (time :point time)
   (x :point x)
   (y :point y))
  (:initial-space-instances (known-world)))

(defmethod print-instance-slots ((location location) stream)
  (call-next-method)
  (when (and (slot-boundp location 'x)
             (slot-boundp location 'y))
    (format stream " (~s ~s)"
            (x-of location)
            (y-of location))))

;;; ---------------------------------------------------------------------------

(defun safe-time-of (location)
  ;; Allow sorting of incomplete LOCATION unit instances (needed only for
  ;; network mirroring):
  (if (slot-boundp location 'time)
      (time-of location)
      infinity$))

;;; ------------------------------------------------------------------------

(define-unit-class path ()
  ((locations
    :link (location path :singular t)
    :sort-function #'<
    :sort-key #'safe-time-of))
  (:initial-space-instances (known-world)))

;;; ===========================================================================
;;;   Initial-location-event

(define-event-class initial-location-event (single-instance-event)
  ())

;;; ===========================================================================
;;;   Create-instance event function

(defun add-location-to-path (event-name &key instance &allow-other-keys)
  (declare (ignore event-name))
  ;; Link the newly created location instance to the path:
  (linkf (path-of instance) *the-random-walk*))

(add-event-function 'add-location-to-path 'instance-created-event 'location)

;;; ===========================================================================
;;;   Startup KS

(defun startup-ks-function (ksa)
  (declare (ignore ksa))
  ;; Create the initial (empty) path instance:
  (setf *the-random-walk* (make-instance 'path))
  ;; Create an initial location instance at (0,0) at time 0:
  (let ((instance (make-instance 'location
                    :time 0
                    :x 0 :y 0)))
    (signal-event 'initial-location-event :instance instance)))

(define-ks startup-ks
    :trigger-events ((control-shell-started-event))
    :rating 100
    :execution-function 'startup-ks-function)

;;; ===========================================================================
;;;   Random-walk KS

(defun add-linear-variance (value max-variance)
  ;;; Returns a new random value in the interval
  ;;; [(- value max-variance), (+ value max-variance)]
  (+ value (- (random (1+ (* max-variance 2))) max-variance)))

;;; ---------------------------------------------------------------------------

(defun random-walk-ks-function (ksa)
  ;;; Move to the next (random) location in the world
  (let* ((trigger-instance (sole-trigger-instance-of ksa))
         ;; The new time is one greater than the stimulus's time:
         (time (1+ (time-of trigger-instance))))
    (cond
     ;; If the maximum time value (75) is reached, tell the user we've
     ;; walked too long:
     ((>= time 75) (format t "~2&Walked too long.~%"))
     (t ;; The new location is +/- 10 of the stimulus's location:
      (let ((x (add-linear-variance (x-of trigger-instance) 10))
            (y (add-linear-variance (y-of trigger-instance) 10)))
        (cond
         ;; Check that the new location is within the known-world
         ;; boundaries.  If so, create the new location instance:
         ((and (<= -50 x 50) (<= -50 y 50))
          (make-instance 'location 
            :time time 
            :x x 
            :y y
            :previous-location trigger-instance))
         ;; Otherwise, tell the user that we've walked too far away:
         (t (format t "~2&Walked off the world: (~d, ~d).~%" x y))))))))

(define-ks random-walk-ks
    :trigger-events ((instance-created-event location))
    :rating 100
    :execution-function 'random-walk-ks-function)

;;; ===========================================================================
;;;   Count-center-locations KS

(defun count-center-locations (ksa)
  ;;; Count the location unit instances within the center quadrant
  ;;; ((-25,-25)(25,25)) of the known world
  (declare (ignore ksa))
  (let ((unit-instances (find-instances 'location '(known-world)
                                        *center-quadrant-pattern*)))
    (format t "~2&~d locations in the center quadrant. ~%"
            (length unit-instances))))

(define-ks count-center-locations-ks
    :trigger-events ((control-shell-started-event))
    ;; The rating of this KS must be lower than the rating of the 
    ;; startup-ks and random-walk-ks:
    :rating 90
    :execution-function 'count-center-locations)

;;; ===========================================================================
;;;   Print-walk KS

(defun print-walk-ks-function (ksa)
  ;;; Starting with the initial location instance, print the instance
  ;;; name and location of the walk
  (format t "~2&The random path:~%")
  (let ((instance (sole-trigger-instance-of ksa)))
    (while instance
      (format t "~6:<~:[~;* ~]~a~> (~s ~s)~%"
              (filter-instances (list instance) *center-quadrant-pattern*)
              (instance-name-of instance)
              (x-of instance)
              (y-of instance))
      (setf instance (next-location-of instance)))))

(define-ks print-walk-ks
    :trigger-events ((initial-location-event location))
    :rating 80
    :execution-function 'print-walk-ks-function)

;;; ===========================================================================
;;;   Initializations (run at Agenda Shell startup)

(defun initializations (event-name &key &allow-other-keys)
  (declare (ignore event-name))
  ;; Clean up any previous run:
  (delete-blackboard-repository)
  ;; Make a new known-world space instance:
  (make-space-instance 
   '(known-world)
   :allowed-unit-classes '(location path)
   :dimensions (dimensions-of 'location)
   :storage '((location (x y) uniform-buckets :layout ((0 100 5)
                                                       (0 100 5))))))

(add-event-function 'initializations 'control-shell-started-event
                    ;; Initializations should be done first!
                    :priority 100)

;;; ===========================================================================
;;;   The main, take-a-walk, function (simply starts the Agenda Shell)

(defun take-a-walk (&rest initargs)
  (declare (dynamic-extent initargs))
  ;; Take-a-walk is equivalent to calling start-control-shell:
  (apply #'start-control-shell initargs))

;;; ===========================================================================
;;;   Save/load repository
;;;

(defun save-tutorial-repository (&rest initargs &aux (pathname "tutorial"))
  (declare (dynamic-extent initargs))
  (when initargs
    (unless (keywordp (first initargs))
      (setf pathname (pop initargs))))
  (apply #'save-blackboard-repository 
         pathname
         :package ':tutorial 
         :value *the-random-walk*
         initargs))

;;; ---------------------------------------------------------------------------

(defun load-tutorial-repository (&rest initargs &aux (pathname "tutorial"))
  (declare (dynamic-extent initargs))
  (when initargs
    (unless (keywordp (first initargs))
      (setf pathname (pop initargs))))
  (multiple-value-bind (loaded-pathname saved-time saved-value)
      (with-events-disabled ()
        (flet ((show-progress (stream percent-loaded)
                 (declare (ignore stream))
                 (format t "~&;; ~3d% loaded~%" percent-loaded)))
          (declare (dynamic-extent #'show-progress))
          (let ((*repository-load-percentage-hook-functions*
                 (list #'show-progress))
                (*repository-load-percentage-reads-per-update* 20))
            (apply #'load-blackboard-repository pathname initargs))))
    (setf *the-random-walk* saved-value)
    (values loaded-pathname saved-time)))

;;; ===========================================================================
;;;   Event printing control

(defun show-important-events ()
  (enable-event-printing 'initial-location-event 'location)
  (enable-event-printing '(control-shell-event :plus-subevents)))

(defun hide-important-events ()
  (disable-event-printing 'initial-location-event 'location)
  (disable-event-printing '(control-shell-event :plus-subevents)))

;;; ---------------------------------------------------------------------------
;;; Initally enable printing of all control shell events:

(show-important-events)

;;; ===========================================================================
;;;  Check link-definition consistency & indicate successful loading

(check-link-definitions nil ':error)        ; Check link-definition consistency

(pushnew ':tutorial *features*)

;;; ===========================================================================
;;;  Autorun actions

(when (and (boundp '*autorun-modules*) 
           *autorun-modules*)
  (format t "~{~&~s~%~}" (multiple-value-list (take-a-walk))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
