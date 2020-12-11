;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-mirroring-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Apr  9 07:04:57 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     GBBopen Network Streaming Master
;;;; *                  (start the slave before this master!)
;;;; *
;;;; *                   [Experimental! Subject to change]
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  02-01-11 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :cl-user)

;; Compile/load GBBopen's :network-streaming module:
(network-streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The slave host:
(define-streamer-node "slave"
    :host "127.0.0.1"
    :passphrase "Open, says me!"
    :package ':tutorial
    :external-format ':utf-8)

;; A 2nd slave host:
(define-streamer-node "slave2"
    :host "127.0.0.1"
    :port (1- (port-of (find-streamer-node "slave")))
    :package ':tutorial
    :external-format ':utf-8)

;; The master host (me!):
(define-streamer-node "master"
    :host "127.0.0.1"
    :port (1+ (port-of (find-streamer-node "slave")))
    :package ':tutorial
    :external-format ':utf-8)

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Connect to slave images:
(defparameter *streamer* (open-network-streamer "slave" "master"))
(defparameter *streamer2* (ignore-errors (open-network-streamer "slave2" "master")))

;; Create a broadcast streamer:

(defparameter *broadcast-streamer* (make-broadcast-streamer *streamer*))
(when *streamer2* 
  (format t "~&;; Adding ~s to ~s...~%" *streamer2* *broadcast-streamer*)
  (add-to-broadcast-streamer *streamer2* *broadcast-streamer*))

(add-mirroring *broadcast-streamer* 'standard-space-instance)
(add-mirroring *broadcast-streamer* 'path)
(add-mirroring *broadcast-streamer* 'location)

;; Generate some data (locally), sending everything as a single queued block:
(with-queued-streaming (*broadcast-streamer* ':tutorial)
  (take-a-walk))
  
;; Delete an instance:
(with-queued-streaming (*broadcast-streamer* ':with-queued)
  (delete-instance (find-instance-by-name 10 'location)))

;; Change a nonlink-slot value:
(setf (time-of (find-instance-by-name 11 'location)) 9)

;; Perform an unlink:
(unlinkf (previous-location-of (find-instance-by-name 9 'location))
         (find-instance-by-name 8 'location))

;; Perform a link (with a link-pointer):
(linkf (next-location-of (find-instance-by-name 8 'location))
       (make-instance 'link-ptr-with-value
         :link-instance (find-instance-by-name 9 'location) 
         :value 0.9))

;; Remove a location from the known-world:
(remove-instance-from-space-instance 
 (find-instance-by-name 8 'location) 
 (find-space-instance-by-path '(known-world)))

;; Add the location back to the known-world:
(add-instance-to-space-instance
 (find-instance-by-name 8 'location) 
 (find-space-instance-by-path '(known-world)))

;; Remove another location from the known-world:
(remove-instance-from-space-instance 
 (find-instance-by-name 5 'location) 
 (find-space-instance-by-path '(known-world)))

;; Create a new world:
(make-space-instance 
    '(new-world)
    :allowed-unit-classes '(location path)
    :dimensions (dimensions-of 'location)
    :storage '((location (x y) uniform-buckets :layout ((0 100 5)
                                                        (0 100 5)))))

;; Add a location to the new world:
(add-instance-to-space-instance 
 (find-instance-by-name 6 'location) 
 (find-space-instance-by-path '(new-world)))

;; Move a location in time:
(let ((instance (find-instance-by-name 3 'location)))
  (with-changing-dimension-values (instance time)
    (setf (time-of instance) -10)))

(defun create-a-bunch (n) 
  (declare (fixnum n))
  (dotimes (i n)
    (make-instance 'location
      :time (+& 1 100)
      :x (-& (random 100) 50)
      :y (-& (random 100) 50))))
(compile 'create-a-bunch)

;; Create a bunch of new locations (with event-printing disabled on the
;; slave):
(stream-command-form '(:disable-event-printing t) *broadcast-streamer*)
(time (create-a-bunch 1000))
#+BIGGER-TEST
(with-queued-streaming (*broadcast-streamer* ':bunch)
  (time (create-a-bunch 5000)))
#+NON-REMOVAL-TEST
(progn
  (setf *remove-mirroring-when-streamer-closes* nil)
  (time (create-a-bunch 10000)))

(defun update-a-bunch (n) 
  (declare (fixnum n))
  (let ((location (find-instance-by-name 100 'location)))
    (dotimes (i n)
      (setf (x-of location)
            (-& (random 100) 50)))))
(compile 'update-a-bunch)

;; Update a bunch of new locations:
(time (update-a-bunch 20000))

;; Check instance links:
(stream-command-form '(:check-all-instance-links) *streamer*)

;; Send a problematic command:
(let ((package (make-package ':bogus)))
  (stream-command-form `(:print ,(intern "ERRORFUL" package)) 
                       *broadcast-streamer*))

;; Send the recovered command:
(stream-command-form '(:print "Recovered correctly!") *broadcast-streamer*)

;; A UTF-8 string:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *utf-8-string* (format nil "UTF-8 characters: ~c~c~c~c~c"
                                       ;; Latin_Capital_Letter_E_With_Grave
                                       (code-char 200)
                                       ;; Latin_Capital_Letter_C_With_Caron
                                       (code-char 268) 
                                       ;; Latin_Small_Letter_L_With_Stroke
                                       (code-char 322)
                                       ;; \Latin_Small_Letter_N_With_Acute
                                       (code-char 324)
                                       ;; Georgian_Paragraph_Separator
                                       (code-char #x10FB))))

;; Journal some UTF-8 characters:
(stream-command-form '(:print #.*utf-8-string*) *broadcast-streamer*)

;; Journal the UTF-8 characters again (with queueing):
(with-queued-streaming (*broadcast-streamer* ':utf-8)
  (stream-command-form '(:print #.*utf-8-string*) *broadcast-streamer*))

;; Send "all done" command:
(stream-command-form '(:print "All done!") *broadcast-streamer*)

;; Close the broadcast streamer:
(close-streamer *broadcast-streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









