;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/journal-writer.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Sep 14 17:29:37 2011 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Journal Writer Example
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
;;;  02-16-11 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :cl-user)

;; Compile/load GBBopen's :streaming module:
(streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Create the journal streamer:
(defparameter *streamer*
    (make-journal-streamer "tutorial" 
                           :package ':tutorial
                           :external-format ':utf-8
                           :value (round pi 2)))

(add-mirroring *streamer* 'standard-space-instance)
(add-mirroring *streamer* 'path)
(add-mirroring *streamer* 'location)

;; Write an empty queue:
(with-queued-streaming (*streamer* ':empty 't))

;; Generate some data, writing everything as a single queued block:
(with-queued-streaming (*streamer* ':tutorial)
  (take-a-walk))

;; Delete an instance, also using WITH-QUEUED-STREAMING:
(with-queued-streaming (*streamer* ':with-queued)
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
(stream-remove-instance-from-space-instance
 (find-instance-by-name 8 'location) 
 '(known-world)
 *streamer*)

;; Add the location back to the known-world:
(stream-add-instance-to-space-instance
 (find-instance-by-name 8 'location) 
 '(known-world)
 *streamer*)

;; Remove another location from the known-world:
(stream-remove-instance-from-space-instance
 (find-instance-by-name 5 'location) 
 (find-space-instance-by-path '(known-world))
 *streamer*)

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

;; Create a bunch of new locations:
(time (create-a-bunch 1000))
#+LONGER-TEST
(time (create-a-bunch 10000))

(defun update-a-bunch (n) 
  (declare (fixnum n))
  (let ((location (find-instance-by-name 100 'location)))
    (dotimes (i n)
      (setf (x-of location)
            (-& (random 100) 50)))))
(compile 'update-a-bunch)

;; Update a bunch of new locations:
(time (update-a-bunch 20000))

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

;; Journal a silly command:
(stream-command-form '(:print "All done!") *streamer*)

;; Close the journal:
(close-streamer *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
