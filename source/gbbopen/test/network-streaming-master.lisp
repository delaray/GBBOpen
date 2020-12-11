;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Apr  9 07:00:30 2011 *-*
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
    :read-default-float-format 'double-float
    :external-format ':utf-8)

;; The master host (me!):
(define-streamer-node "master"
    :port (1+ (port-of (find-streamer-node "slave")))
    :package ':cl-user
    :external-format ':utf-8)

;; Connect to slave image:
(defparameter *streamer* (open-network-streamer "slave" "master"))

;; Slightly useful command-form method:
(defmethod handle-streamed-command-form (streamer 
                                         (command (eql ':pa))
                                         &rest args)
  (declare (ignorable streamer) (ignore args))
  (format t "~&;; Package: ~s ~%" *package*))

;; Make a (trivial) broadcast streamer:
(defparameter *broadcast-streamer* (make-broadcast-streamer *streamer*))

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Generate some data (locally):
(take-a-walk)

;; Send the space instances:
(stream-instances (find-space-instances 't) *streamer*)

;; Send everything else (as a single queued block):
(with-queued-streaming (*broadcast-streamer* ':tutorial)
  (stream-instances-of-class 'path *broadcast-streamer*)
  (clear-streamer-queue *broadcast-streamer*)
  (stream-instances-of-class 'path *broadcast-streamer*)
  (stream-instances-of-class 'location *broadcast-streamer*))

;; Close the broadcast streamer:
(close-streamer *broadcast-streamer*)

;; Test empty queue writing:
(with-queued-streaming (*streamer* ':empty-queue-that-should-not-be-written)
  nil)
(with-queued-streaming (*streamer* ':empty-queue-that-should-be-written 't)
  nil)

;; Delete an instance on the slave (but not here), also testing
;; some nested WITH-QUEUED-STREAMING macros:
(with-queued-streaming (*streamer* ':another-empty-queue-that-should-be-written 't)
  (with-queued-streaming (*streamer* ':empty-queue-that-should-not-be-written)
    (with-queued-streaming (*streamer* ':empty-queue-that-should-be-written 't)
      (with-queued-streaming (*streamer* ':with-queued)
        (stream-delete-instance (find-instance-by-name 10 'location) *streamer*)))))

;; Change some nonlink-slot values on the slave (but not here), also testing a
;; unit-instance tag and WRITE-STREAMER-QUEUE:
(with-queued-streaming (*streamer* (find-instance-by-name 11 'location))
  (stream-nonlink-slot-update 
   (find-instance-by-name 11 'location) 'time 9 *streamer*)
  (write-streamer-queue *streamer*)
  (stream-nonlink-slot-update 
   (find-instance-by-name 12 'location) 'time 10 *streamer*)
  (write-streamer-queue *streamer* :tag (find-instance-by-name 13 'location))
  (stream-nonlink-slot-update 
   (find-instance-by-name 13 'location) 'time 10 *streamer*))

;; Perform an unlink on the slave (but not here):
(stream-unlink (find-instance-by-name 9 'location) 
               'previous-location
               (find-instance-by-name 8 'location) 
               *streamer*)

;; Perform a link on the slave (but not here; with a link-pointer):
(stream-link (find-instance-by-name 8 'location) 
             'next-location
             (make-instance 'link-ptr-with-value
               :link-instance (find-instance-by-name 9 'location) 
               :value 0.9)
             *streamer*)

;; Remove a location from the known-world on the slave (but not here):
(stream-remove-instance-from-space-instance
 (find-instance-by-name 8 'location) 
 '(known-world)
 *streamer*)

;; Add the location back to the known-world on the slave (but not here):
(stream-add-instance-to-space-instance
 (find-instance-by-name 8 'location) 
 '(known-world)
 *streamer*)

;; Remove another location from the known-world on the slave (but not here):
(stream-remove-instance-from-space-instance
 (find-instance-by-name 5 'location) 
 (find-space-instance-by-path '(known-world))
 *streamer*)

;; Check instance links:
(stream-command-form '(:check-all-instance-links) *streamer*)

;; Send a problematic command:
(let ((package (make-package ':bogus)))
  (stream-command-form `(:print ,(intern "ERRORFUL" package)) *streamer*))

;; Send the recovered command:
(stream-command-form '(:print "Recovered correctly!") *streamer*)

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
(stream-command-form '(:print #.*utf-8-string*) *streamer*)

;; Journal the UTF-8 characters again (with queueing):
(with-queued-streaming (*streamer* ':utf-8)
  (stream-command-form '(:print #.*utf-8-string*) *streamer*))

;; Send "all done" command:
(stream-command-form '(:print "All done!") *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









