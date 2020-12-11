;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-slave2.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Apr  9 07:04:18 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    GBBopen Network Streaming Slave 
;;;; *                  (start this slave before the master!)
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

;; Display UTF-8 characters on *standard-output* (when the CL requires it):
;; [NOTE: Invoke Clozure CL with -K utf-8 to set *terminal-io* to :utf-8]
#+cmu
(setf (stream-external-format *standard-output*) ':utf-8)

;; Compile/load GBBopen's :network-streaming module:
(network-streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The host (me!):
(define-streamer-node "slave2"
    :host "127.0.0.1"
    :port (1- *default-network-stream-server-port*)
    :package ':common-lisp
    :external-format ':utf-8
    :authorized-nodes ':any
    :accepted-streamer-node-initargs '(:package :gbbopen 
                                       :external-format :utf-8))

;; The master host:
(define-streamer-node "master"
    :port (1+ *default-network-stream-server-port*)
    :read-default-float-format 'long-float
    :package ':gbbopen
    :external-format ':utf-8)


;; Define the bogus package at slave2 (so only slave has a reading issue):
(make-package ':bogus)

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Silly read-queued-streaming-block method:
(defmethod read-queued-streaming-block :around ((tag t) string-stream)
  (declare (ignorable string-stream))
  (format t "~&;; Beginning queued ~a reading...~%" tag)
  (call-next-method)
  (format t "~&;; Ending queued ~a reading.~%" tag))

;; Silly command-form method:
(defmethod handle-streamed-command-form (streamer (command (eql ':print)) &rest args)
  (format t "~&;; Print: ~s~{ ~s~}~%" (streamer-node-of streamer) args))

;; Slightly more useful command-form method:
(defmethod handle-streamed-command-form (streamer (command (eql ':pa)) &rest args)
  (declare (ignorable streamer) (ignore args))
  (format t "~&;; Package: ~s ~%" *package*))

;; A still more useful command-form method:
(defmethod handle-streamed-command-form (streamer 
                                         (command (eql ':disable-event-printing))
                                         &rest args)
  (declare (ignorable streamer) (dynamic-extent args))
  (apply #'disable-event-printing args))

;; Link checking:
(defmethod handle-streamed-command-form
    (streamer (command (eql ':check-all-instance-links)) &rest args)
  (declare (ignorable streamer) (ignore args))
  (check-all-instance-links))

;; Silly connection-exiting method:
(defmethod handle-stream-connection-exiting ((connection stream) exit-status)
  (format t "~&;; Connection ~s closing~@[: (~s)~]~%"
          connection exit-status))

;; Show what is happening once streaming begins!
(enable-event-printing 'instance-created-event 'location)
(enable-event-printing 'delete-instance-event 'location)
(add-event-function
 ;; Enable update-nonlink-slot-event printing only after the delete-instance
 ;; has been received:
 #'(lambda (&rest args)
     (declare (ignore args))
     (enable-event-printing 'nonlink-slot-updated-event 'location :slot-name 'time)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'previous-location)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'next-location)
     (enable-event-printing 'instance-added-to-space-instance-event 'location)
     (enable-event-printing 'instance-removed-from-space-instance-event 'location))
 'delete-instance-event 'location)

;; Don't warn that the Agenda Shell isn't running to process trigger events on
;; received goodies:
(setf *warn-about-unusual-requests* nil)

;; Prepare to receive from the master:
(defparameter *network-stream-server* (start-network-stream-server "slave2"))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


