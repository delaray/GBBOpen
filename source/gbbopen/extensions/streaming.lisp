;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/streaming.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jun 27 17:26:35 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         Journaling Entities
;;;; *                   [Experimental! Subject to change]
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2011-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-19-11 File created.  (Corkill)
;;;  02-28-11 Moved network-streaming entitites to their own file.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*recorded-class-descriptions-ht*
            gbbopen-tools::write-saving/sending-block-info)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*error-on-unresolved-streamed-instance-reference* ; not yet documented
            *journal-load-percentage-hook-functions* ; not yet documented
            *journal-load-percentage-reads-per-update* ; not yet documented
            add-mirroring
            add-to-broadcast-streamer
            begin-queued-streaming      ; not documented
            broadcast-streamer          ; class-name (not yet documented)
            clear-streamer-queue
            close-streamer
            describe-mirroring          ; not yet documented
            end-queued-streaming        ; not documented
            handle-streamed-command-atom ; not yet documented
            handle-streamed-command-form ; not yet documented
            handle-stream-input-error   ; not yet documented
            invoke-close-stream-restart ; not yet documented
            invoke-skip-form-restart    ; not yet documented
            journal-streamer            ; class-name (not yet documented)
            load-journal
            make-broadcast-streamer
            make-journal-streamer
            open-streamer-p
            read-queued-streaming-block
            remove-from-broadcast-streamer
            remove-mirroring
            skip-form                   ; restart; not yet documented
            stream-command-form         ; not yet documented
            stream-add-instance-to-space-instance
            stream-add-to-space         ; old name, remove soon
            stream-delete-instance
            stream-instance
            stream-instances
            stream-instances-of-class
            stream-instances-on-space-instances
            stream-link
            stream-nonlink-slot-update
            stream-of
            stream-remove-from-space    ; old name, remove soon
            stream-remove-instance-from-space-instance
            stream-slot-update          ; old name, remove soon
            stream-unlink
            streamer                    ; class-name (not yet documented)
            streamer-error              ; condition-name (not yet documented)
	    with-mirroring-disabled
	    with-mirroring-enabled
            with-queued-streaming
            write-streamer-queue)))

;;; ---------------------------------------------------------------------------

(defvar *%%reading-streamer%%* nil) ;; Bound to the reading network streamer
                                    ;; when reading from a connection

;;; ===========================================================================
;;;   Streamer queues

(defvar *%%streamer-queues%%* nil)      ; records thread-local streamer queues
(defvar *queued-read-tag*)              ; remove soon (with the #G! reader)

;;; ---------------------------------------------------------------------------

(defstruct (streamer-queue 
            (:conc-name #.(dotted-conc-name 'streamer-queue))
            (:copier nil))
  stream
  tag
  write-empty-queue-p
  tag-string
  recorded-class-descriptions-ht)

;;; ===========================================================================
;;;   Streamers (%trivial-streamer% is defined in ../events.lisp)

(define-class basic-streamer (%trivial-streamer%)
  (lock
   (stream :initform nil)
   (closed :initform nil)
   (recorded-class-descriptions-ht :initform (make-hash-table :test 'eq))
   ;; Values used for writing:
   package
   external-format
   read-default-float-format))

(defmethod print-instance-slots :after ((streamer basic-streamer) stream)
  (unless (and (slot-boundp streamer 'stream)
               (open-streamer-p streamer))
    (format stream " (closed)")))

;;; ---------------------------------------------------------------------------

(defgeneric close-streamer (streamer))

;;; ---------------------------------------------------------------------------

(define-class streamer (basic-streamer)
  ((broadcast-streamer :initform nil)))

;;; ===========================================================================
;;;   Journal streamer

(define-class journal-streamer (streamer)
  ())
   
;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((journal-streamer journal-streamer) stream)
  (call-next-method)
  (let ((pathname (stream-of journal-streamer)))
    (when pathname
      (format stream " ~s" (enough-namestring pathname)))))

;;; ---------------------------------------------------------------------------

(define-condition streamer-error (error)
  ((streamer :reader streamer-error-streamer :initarg :streamer))
  (:report
   (lambda (condition stream)
     (format stream "Operation on closed streamer ~s"
             (streamer-error-streamer condition)))))

;;; ---------------------------------------------------------------------------

(defun open-streamer-p (streamer)
  (let ((stream (stream-of streamer)))
    (and (streamp stream) (open-stream-p stream))))

;;; ---------------------------------------------------------------------------

(defmethod close-streamer ((streamer journal-streamer))
  (remove-mirroring streamer)
  ;; Remove from broadcast-streamer:
  (let ((broadcast-streamer (broadcast-streamer-of streamer)))
    (when broadcast-streamer
      (remove-from-broadcast-streamer 
       streamer broadcast-streamer)))
  (setf (closed-of streamer) 't)
  ;; Now close the file:
  (let ((stream (stream-of streamer)))
    (close stream)))

;;; ===========================================================================
;;;   Broadcast streamer

(define-class broadcast-streamer (basic-streamer)
  ((streamers :initform nil)))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((broadcast-streamer broadcast-streamer) 
                                 stream)
  (call-next-method)
  (let ((number-of-constituents (length (streamers-of broadcast-streamer))))
    (format stream " ~s constituent~:p" number-of-constituents)))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer-given-initargs 
    (&key (package ':common-lisp-user)
          (read-default-float-format *read-default-float-format*)
          (external-format 'default))
  (make-instance 'broadcast-streamer
    :lock (make-recursive-lock :name "Broadcast streamer lock")
    :package (ensure-package package)
    :read-default-float-format read-default-float-format
    :external-format external-format))

;;; ---------------------------------------------------------------------------

(defun set-broadcast-streamers (broadcast-streamer streamers)
  (setf (stream-of broadcast-streamer)
        (apply #'make-broadcast-stream 
               (flet ((add-it (streamer)
                        (setf (broadcast-streamer-of streamer)
                              broadcast-streamer)
                        (stream-of streamer)))
                 (declare (dynamic-extent #'add-it))
                 (mapcar #'add-it streamers))))
  (setf (streamers-of broadcast-streamer) streamers))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer-given-streamers (streamers)
  ;; Check if any of the streamers are queued (at least in this thread); TODO:
  ;; deal with queueing better than this!
  (dolist (streamer streamers)
    (when (assq streamer *%%streamer-queues%%*)
      (error "A queued streamer cannot be added to a broadcast streamer: ~s"
             streamer)))
  ;; TODO: Check that all streamers have the same package/default-float, no
  ;; duplicates, creating without any streamers, etc.
  (let* ((1st-streamer (car streamers))
         (broadcast-streamer
          (make-broadcast-streamer-given-initargs
          ;; For now, use the attributes of the 1st streamer:
          :package (package-of 1st-streamer)
          :read-default-float-format (read-default-float-format-of 1st-streamer)
          :external-format (external-format-of 1st-streamer))))
    (set-broadcast-streamers broadcast-streamer streamers)
    ;; Return the broadcast streamer:
    broadcast-streamer))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer (&rest streamers-or-initargs)
  (if (and (consp streamers-or-initargs)
           (typep (car streamers-or-initargs) 'streamer))
      (make-broadcast-streamer-given-streamers streamers-or-initargs)
      (apply #'make-broadcast-streamer-given-initargs streamers-or-initargs)))

;;; ---------------------------------------------------------------------------

(defun check-broadcast-streamer-compatibility (streamer broadcast-streamer)
  ;; Check :package:
  (let ((streamer-package (package-of streamer))
        (broadcast-package (package-of broadcast-streamer)))
    (unless (eq streamer-package broadcast-package)
      (error "The ~s ~s of ~s does not match ~s of ~s"
             ':package 
             streamer-package
             streamer
             broadcast-package
             broadcast-streamer)))
  ;; Check :read-default-float-format:
  (let ((streamer-rdff (read-default-float-format-of streamer))
        (broadcast-rdff (read-default-float-format-of broadcast-streamer)))
    (unless (eq streamer-rdff broadcast-rdff)
      (error "The ~s ~s of ~s does not match ~s of ~s"
             ':read-default-float-format
             streamer-rdff
             streamer
             broadcast-rdff
             broadcast-streamer)))
  ;; Check :external-format:
  (let ((streamer-external-format (external-format-of streamer))
        (broadcast-external-format (external-format-of broadcast-streamer)))
    (unless (eq streamer-external-format broadcast-external-format)
      (error "The ~s ~s of ~s does not match ~s of ~s"
             ':external-format
             streamer-external-format
             streamer
             broadcast-external-format
             broadcast-streamer))))

;;; ---------------------------------------------------------------------------

(defun add-to-broadcast-streamer (streamer broadcast-streamer)
  (when (closed-of broadcast-streamer)
    (error 'streamer-error :streamer broadcast-streamer))
  (cond 
   ((broadcast-streamer-of streamer)
    (error "Streamer ~s is a member of broadcast streamer ~s"
           streamer broadcast-streamer))
   (t (with-lock-held ((lock-of broadcast-streamer))
        (let ((streamers (cons streamer (streamers-of broadcast-streamer))))
          (check-broadcast-streamer-compatibility streamer broadcast-streamer)
          ;; Clear the recorded class descriptions of the broadcast streamer:
          (clrhash (recorded-class-descriptions-ht-of broadcast-streamer))
          (setf (broadcast-streamer-of streamer) broadcast-streamer)
          (set-broadcast-streamers broadcast-streamer streamers)))
      ;; Return streamer:
      streamer)))

;;; ---------------------------------------------------------------------------

(defun remove-from-broadcast-streamer (streamer broadcast-streamer)
  (when (closed-of broadcast-streamer)
    (error 'streamer-error :streamer broadcast-streamer))
  (with-lock-held ((lock-of broadcast-streamer))
    (let ((streamers (streamers-of broadcast-streamer)))
      (when (memq streamer streamers)
        (setf (broadcast-streamer-of streamer) nil)
        (set-broadcast-streamers 
         broadcast-streamer
         (remove streamer streamers :test #'eq))
        ;; Return streamer on success:
        streamer))))

;;; ---------------------------------------------------------------------------

(defmethod close-streamer ((streamer broadcast-streamer))
  (remove-mirroring streamer)
  (with-lock-held ((lock-of streamer))
    (setf (closed-of streamer) 't)
    (setf (stream-of streamer) nil)
    ;; Disconnect all constituents:
    (dolist (constituent-streamer (streamers-of streamer))
      (setf (broadcast-streamer-of constituent-streamer) nil))
    (setf (streamers-of streamer) nil))
  ;; Return success:
  't)

;;; ===========================================================================
;;;   Streamer entities

(defvar *error-on-unresolved-streamed-instance-reference* 't)

;;; ---------------------------------------------------------------------------

(defun %on-each-constituent-streamer (fn broadcast-streamer)
  ;; MAJOR HACK: Until robust broadcast streams are provided, write each
  ;; constituent streamer separately, catching write errors and closing the
  ;; constituent:
  (dolist (constituent-streamer (streamers-of broadcast-streamer))
    (with-error-handling (funcall fn constituent-streamer)
      (remove-from-broadcast-streamer constituent-streamer broadcast-streamer)
      (let ((*print-readably* nil))
        (princ (error-message) *error-output*)
        (terpri *error-output*)
        (let ((connection (stream-of constituent-streamer)))
          (when (and (streamp connection) (open-stream-p connection))
            (format *error-output* 
                    "~&;; Closing ~s due to write error...~%"
                    connection)
            (force-output *error-output*)
            (close connection)))))))

;;; ---------------------------------------------------------------------------

(defun %do-with-streamer-stream (streamer body-form-fn)
  (when (closed-of streamer)
    (error 'streamer-error :streamer streamer))
  (let ((streamer-for-writing streamer)
        (broadcast-streamer-of-streamer
         (when (typep streamer 'streamer)
           (broadcast-streamer-of streamer))))
    ;; If streamer is a constituent of a broadcast-streamer, use the
    ;; streamer for writing/queuing but the broadcast-streamer for locking:
    (when broadcast-streamer-of-streamer
      (setf streamer broadcast-streamer-of-streamer))
    (let* ((streamer-acons (assq streamer-for-writing *%%streamer-queues%%*))
           (streamer-queue (cdr streamer-acons)))
      (with-standard-io-syntax 
        (setf *package* (package-of streamer))
        (setf *read-default-float-format* 
              (read-default-float-format-of streamer))
        (flet
            ((do-it ()
               (if streamer-queue
                   ;; queued streaming:
                   (let ((*recorded-class-descriptions-ht* 
                          (streamer-queue.recorded-class-descriptions-ht
                           streamer-queue)))
                     (funcall body-form-fn 
                              (streamer-queue.stream streamer-queue)))
                   ;; regular streaming:
                   (with-lock-held ((lock-of streamer))
                     (let ((*recorded-class-descriptions-ht* 
                            (recorded-class-descriptions-ht-of 
                             streamer-for-writing))
                           (stream (stream-of streamer-for-writing)))
                       (cond 
                        ;; broadcast, non-queued streaming; formst to
                        ;; string-stream and then write each constituent
                        ;; streamer separately:
                        ((typep streamer-for-writing 'broadcast-streamer)
                         (let ((string-stream (make-string-output-stream)))
                           (funcall body-form-fn string-stream)
                           (let ((string 
                                  (get-output-stream-string string-stream)))
                             (flet
                                 ((write-it (constituent-streamer)
                                    (let ((stream
                                           (stream-of constituent-streamer)))
                                      (write-sequence string stream)
                                      (force-output stream))))
                               (declare (dynamic-extent #'write-it))
                               (%on-each-constituent-streamer 
                                #'write-it streamer-for-writing)))))
                        ;; non-broadcast, non-queued streaming:
                        (t (funcall body-form-fn stream)))
                       ;; Flush the non-queued output:
                       (force-output stream))))))
          ;; Quick & dirty handling of stream errors -- timeout checks and
          ;; notifying other endpoint needed!!
          (with-error-handling ((do-it) :conditions 'stream-error)
            (let ((*print-readably* nil))
              (princ (error-message) *error-output*)
              (terpri *error-output*)
              (let ((connection (stream-of streamer-for-writing)))
                (when (and (streamp connection) (open-stream-p connection))
                  (format *error-output* "~&;; Closing ~s due to error...~%"
                          connection)
                  (force-output *error-output*)
                  (setf (closed-of streamer-for-writing)
                        ':closed-due-to-errors)
                  (when streamer-queue
                    (format *error-output* 
                            "~&;; Terminating queued streamer ~s...~%"
                            streamer-queue)
                    (force-output *error-output*)
                    (setf *%%streamer-queues%%*
                          (delq streamer-acons *%%streamer-queues%%*)))
                  (close connection)
                  (error 'streamer-error 
                         :streamer streamer-for-writing))))))))))
    
;;; ---------------------------------------------------------------------------

(defmacro %with-streamer-stream ((var streamer) &body body)
  ;; Internal-use macro establishing standard-streaming-synatx & lock
  ;; grabbing, if needed:
  `(flet ((body-form (.stream.)
            (let ((,var .stream.))
              ,@body)))
     (declare (dynamic-extent #'body-form))
     (%do-with-streamer-stream ,streamer #'body-form)))

;;; ---------------------------------------------------------------------------

(defun %clone-recorded-class-descriptions-ht (ht)
  (let ((clone-ht (make-hash-table :test 'eq :size (hash-table-count ht))))
    ;; transfer ht's entries:
    (flet ((add-it (key value)
             (setf (gethash key clone-ht) value)))
      (declare (dynamic-extent #'add-it))
      (maphash #'add-it ht))
    ;; return the cloned ht:
    clone-ht))

;;; ---------------------------------------------------------------------------

(defun %merge-recorded-class-descriptions-hts (queued-stream-ht ht)
  ;; insert the queued-stream ht's entries:
  (flet ((add-it (key value)
           (setf (gethash key ht) value)))
    (declare (dynamic-extent #'add-it))
    (maphash #'add-it queued-stream-ht)))

;;; ---------------------------------------------------------------------------

(defun begin-queued-streaming (streamer tag write-empty-queue-p)
  (with-lock-held ((lock-of streamer))
    (force-output (stream-of streamer))
    (let* ((queue-stream (make-string-output-stream))
           (recorded-class-descriptions-ht
            (%clone-recorded-class-descriptions-ht 
             (recorded-class-descriptions-ht-of streamer)))
           (streamer-queue
            (make-streamer-queue
             :stream queue-stream
             :tag tag
             :write-empty-queue-p write-empty-queue-p
             :recorded-class-descriptions-ht recorded-class-descriptions-ht)))
      (let ((*recorded-class-descriptions-ht* recorded-class-descriptions-ht))
        (with-standard-io-syntax 
          (setf *package* (package-of streamer))
          (setf *read-default-float-format* 
                (read-default-float-format-of streamer))
          (print-object-for-saving/sending tag queue-stream)))
      ;; Push the new streamer-queue for this streamer:
      (push-acons streamer streamer-queue *%%streamer-queues%%*)
      ;; Stash the tag-string:
      (setf (streamer-queue.tag-string streamer-queue)
            (get-output-stream-string queue-stream)))))

;;; ---------------------------------------------------------------------------

(defun no-streamer-queue-error (streamer)
  (error "Streamer ~s is not being queued." streamer))

;;; ---------------------------------------------------------------------------

(defun %write-streamer-queue (streamer)
  (let* ((streamer-queue (or (cdr (assq streamer *%%streamer-queues%%*))
                             (no-streamer-queue-error streamer)))
         (queue-stream (streamer-queue.stream streamer-queue)))
    ;; End the current queuing block:
    (let ((string (get-output-stream-string queue-stream)))
      (flet
          ((write-it (streamer-for-locking streamer-for-writing)
             (let ((stream (stream-of streamer-for-writing)))
               (cond
                ;; Empty queue:
                ((zerop& (length string))
                 (when (streamer-queue.write-empty-queue-p streamer-queue)
                   (with-lock-held ((lock-of streamer-for-locking))
                     (format stream "~&#GQ(")
                     (write-sequence 
                      (streamer-queue.tag-string streamer-queue) stream)
                     (format stream " 0)")
                     (force-output stream))))
                ;; Non-empty queue:
                (t (with-lock-held ((lock-of streamer-for-locking))
                     (format stream "~&#GQ(")
                     (write-sequence 
                      (streamer-queue.tag-string streamer-queue) stream)
                     (format stream " ~s " (length string))
                     (write-sequence string stream)
                     (princ ")" stream)
                     (force-output stream))
                   ;; If needed, merge the streamer-queue's recorded
                   ;; class-descriptions HT:
                   (let ((queued-ht
                          (streamer-queue.recorded-class-descriptions-ht
                           streamer-queue))
                         (streamer-ht
                          (recorded-class-descriptions-ht-of 
                           streamer-for-writing)))
                     ;; Only merge if the queued-ht has more elements than the
                     ;; streamer (a heuristic that misses merging when
                     ;; different new descriptions have been added to the
                     ;; streamer than to the queued, but missed merging only
                     ;; results in unnecessary class-description writing):
                     (when (>& (hash-table-count queued-ht)
                               (hash-table-count streamer-ht))
                       (with-lock-held ((lock-of streamer-for-writing))
                         (%merge-recorded-class-descriptions-hts 
                          queued-ht streamer-ht)))))))))
        (if (typep streamer 'broadcast-streamer)
            ;; Write each constituent streamer separately:
            (flet ((do-it (constituent-streamer) 
                     (write-it streamer constituent-streamer)))
              (declare (dynamic-extent #'do-it))
              (%on-each-constituent-streamer #'do-it streamer))
            (write-it 
             ;; If the streamer is a constituent of a broadcast-streamer
             ;; use the broadcast-streamer's lock:
             (or (broadcast-streamer-of streamer) streamer) 
             streamer))))
    ;; Must return the streamer-queue:
    streamer-queue))

;;; ---------------------------------------------------------------------------

(defun end-queued-streaming (streamer)
  (let ((streamer-queue (%write-streamer-queue streamer)))
    ;; Leave evidence that the streamer-queue has ended:
    (setf (streamer-queue.stream streamer-queue) ':ended)))

;;; ---------------------------------------------------------------------------

(defmacro with-queued-streaming ((streamer &optional tag write-empty-queue-p)
                                 &body body)
  (with-once-only-bindings (streamer)
    `(let ((*%%streamer-queues%%* *%%streamer-queues%%*))
       (begin-queued-streaming ,streamer ,tag ,write-empty-queue-p)
       (unwind-protect (progn ,@body)
         (end-queued-streaming ,streamer)))))

;;; ---------------------------------------------------------------------------

(defun clear-streamer-queue (streamer)
  (let ((streamer-queue
         (or (cdr (assq streamer gbbopen::*%%streamer-queues%%*))
             (no-streamer-queue-error streamer))))
    ;; Substitute a new (empty) string stream:
    (setf (streamer-queue.stream streamer-queue) (make-string-output-stream))
    ;; And reset the recorded class-descriptions hash table for this queue:
    (setf (streamer-queue.recorded-class-descriptions-ht streamer-queue)
          (%clone-recorded-class-descriptions-ht
           (recorded-class-descriptions-ht-of streamer)))))

;;; ---------------------------------------------------------------------------

(defun write-streamer-queue (streamer 
                             &key (tag nil tag-supplied-p)
                                  (write-empty-queue-p nil weqp-supplied-p))
  (let* ((streamer-queue (%write-streamer-queue streamer))
         (queue-stream (streamer-queue.stream streamer-queue))
         (recorded-class-descriptions-ht
          (%clone-recorded-class-descriptions-ht 
           (recorded-class-descriptions-ht-of streamer))))
    ;; Reset the recorded class-descriptions hash table for this queue:
    (setf (streamer-queue.recorded-class-descriptions-ht streamer-queue)
          recorded-class-descriptions-ht)
    ;; Setup tag & write-empty-queue-p values, using the previous tag and
    ;; write-empty-queue-p values if they weren't supplied:
    (if tag-supplied-p
        (setf (streamer-queue.tag streamer-queue) tag)
        (setf tag (streamer-queue.tag streamer-queue)))
    (if weqp-supplied-p
        (setf (streamer-queue.write-empty-queue-p streamer-queue) 
              write-empty-queue-p)
        (setf write-empty-queue-p 
              (streamer-queue.write-empty-queue-p streamer-queue)))
    ;; Begin a new queuing block:    
    (let ((*recorded-class-descriptions-ht* recorded-class-descriptions-ht))
      (with-standard-io-syntax 
        (setf *package* (package-of streamer))
        (setf *read-default-float-format* 
              (read-default-float-format-of streamer))
        (print-object-for-saving/sending tag queue-stream)))
    ;; Stash the tag-string:
    (setf (streamer-queue.tag-string streamer-queue)
          (get-output-stream-string queue-stream))))

;;; ---------------------------------------------------------------------------
;;;  Delete unit instance reader

(defmethod saved/sent-object-reader ((char (eql #\X)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name 
                     instance-name class-name 
                     *error-on-unresolved-streamed-instance-reference*)))
      (when instance (delete-instance instance)))))

;;; ---------------------------------------------------------------------------
;;;  Unit-instance slot-update reader

(defmethod saved/sent-object-reader ((char (eql #\S)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name slot-name new-value)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name 
                     instance-name class-name
                     *error-on-unresolved-streamed-instance-reference*)))
      (when instance
        ;; TODO: CHECK FOR CHANGE TO A LINK SLOT OR MISSING SLOT...
        (let ((source-slot 
               (cdr (assq slot-name
                          (standard-unit-class.effective-dv-source-slots
                           (class-of instance))))))
          (if source-slot
              ;; TODO: Optimize this!
              (with-changing-dimension-values (instance)  
                (setf (slot-value instance slot-name) new-value))
              (setf (slot-value instance slot-name) new-value)))))))
  
;;; ---------------------------------------------------------------------------
;;;  Unit-instance link reader

(defmethod saved/sent-object-reader ((char (eql #\+)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name
                     instance-name class-name
                     *error-on-unresolved-streamed-instance-reference*)))
      (when instance
        ;; TODO: CHECK FOR CHANGE TO A NON-LINK SLOT OR MISSING SLOT...
        (let ((source-slot 
               (cdr (assq slot-name
                          (standard-unit-class.effective-dv-source-slots
                           (class-of instance))))))
          (if source-slot
              ;; TODO: Optimize this!
              (with-changing-dimension-values (instance)  
                (linkf (slot-value instance slot-name) other-instances))
              (linkf (slot-value instance slot-name) other-instances)))))))
  
;;; ---------------------------------------------------------------------------
;;;  Unit-instance unlink reader

(defmethod saved/sent-object-reader ((char (eql #\-)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name 
                     instance-name class-name 
                     *error-on-unresolved-streamed-instance-reference*)))
      (when instance
        ;; TODO: CHECK FOR CHANGE TO A NON-LINK SLOT OR MISSING SLOT...
        (let ((source-slot 
               (cdr (assq slot-name
                          (standard-unit-class.effective-dv-source-slots
                           (class-of instance))))))
          (if source-slot
              ;; TODO: Optimize this!
              (with-changing-dimension-values (instance)  
                (unlinkf (slot-value instance slot-name) other-instances))
              (unlinkf (slot-value instance slot-name) other-instances)))))))
  
;;; ---------------------------------------------------------------------------
;;;  Add instance to space-instance reader

(defmethod saved/sent-object-reader ((char (eql #\a)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name 
                       space-instance-class-name space-instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (setf space-instance-class-name
          (possibly-translate-class-name space-instance-class-name))
    (let ((instance (find-instance-by-name
                     instance-name class-name
                     *error-on-unresolved-streamed-instance-reference*))
          (space-instance (find-instance-by-name 
                           space-instance-name space-instance-class-name 
                           *error-on-unresolved-streamed-instance-reference*)))
      (when (and instance space-instance)
        (add-instance-to-space-instance instance space-instance)))))
        
;;; ---------------------------------------------------------------------------
;;;  Remove instance from space-instance reader

(defmethod saved/sent-object-reader ((char (eql #\r)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name instance-name 
                       space-instance-class-name space-instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (setf space-instance-class-name
          (possibly-translate-class-name space-instance-class-name))
    (let ((instance (find-instance-by-name 
                     instance-name class-name 
                     *error-on-unresolved-streamed-instance-reference*))
          (space-instance (find-instance-by-name 
                           space-instance-name space-instance-class-name
                           *error-on-unresolved-streamed-instance-reference*)))
      (when (and instance space-instance)
        (remove-instance-from-space-instance instance space-instance)))))
        
;;; ---------------------------------------------------------------------------
;;;  Command form reader

(defmethod saved/sent-object-reader ((char (eql #\.)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (form)
      (read stream t nil 't)
    (if (consp form)
        (apply #'handle-streamed-command-form *%%reading-streamer%%* form)
        (handle-streamed-command-atom *%%reading-streamer%%* form))))
        
;;; ---------------------------------------------------------------------------
;;;  Command form methods

(defgeneric handle-streamed-command-form (streamer command &rest args))
(defgeneric handle-streamed-command-atom (streamer command))

;; Default error methods:
(defmethod handle-streamed-command-form (streamer command &rest args)
  (declare (ignorable streamer))
  (error "Unhandled streamed command: ~s" (cons command args)))

(defmethod handle-streamed-command-atom (streamer command)
  (declare (ignorable streamer))
  (error "Unhandled streamed command: ~s" command))
         
;;; ---------------------------------------------------------------------------
;;;  Restartable reader & restarts

(defun skip-stream-input-form (stream)
  ;;; Scan to the next Newline character, which *could* be the next
  ;;; form.  If the in-process form contains Newlines, then spurious reader
  ;;; errors will be generated (but it is the best that we can do).
  (peek-char #\newline stream nil))

;;; ---------------------------------------------------------------------------

(defgeneric handle-stream-input-error (condition stream))

;; Default method invokes the debugger:
(defmethod handle-stream-input-error (condition stream)
  (declare (ignore stream))
  (break condition))  

;;; ---------------------------------------------------------------------------

(defun invoke-skip-form-restart (&optional (allow-close 't))
  (let ((restart (or (find-restart 'skip-form)
                     ;; If the skip-form restart is not available because
                     ;; form processing has not begun, close the stream if
                     ;; allowed:
                     (and allow-close (find-restart 'close)))))
    (when restart (invoke-restart restart))))

;;; ---------------------------------------------------------------------------

(defun invoke-close-stream-restart ()
  (invoke-restart (find-restart 'close)))

;;; ---------------------------------------------------------------------------

(defun restartable-reader (input-stream eof-marker)
  (with-error-handling 
      ((restart-case (read input-stream nil eof-marker)
         (skip-form () 
             :report "Skip the offending input form"
           (skip-stream-input-form input-stream)
           nil)
         (close ()
             :report (lambda (stream)
                       (format stream "Close the input stream ~s" 
                               input-stream))
           (throw 'close-stream ':error)))
       ;; Handler
       (handle-stream-input-error (error-condition) input-stream))))

;;; ===========================================================================
;;;  Queued block methods

(defgeneric read-queued-streaming-block (tag string-stream))

(defmethod read-queued-streaming-block (tag string-stream)
  (declare (ignore tag))
  ;; Read the queue-block:
  (let ((eof-marker '#:eof))
    (until (eq eof-marker (restartable-reader string-stream eof-marker)))))

;;; ---------------------------------------------------------------------------
;;;  Queued-streaming-block reader
         
(defmethod saved/sent-object-reader ((char (eql #\Q)) stream)
  #+ecl (declare (ignore char))
  ;; Skip the opening parenthesis:
  (read-char stream 't nil 't)
  (let* ((tag (read stream 't nil 't))
         (length (read-preserving-whitespace stream))
         (block-string (make-string length)))
    (unless (zerop& length)
      (read-char stream)                ; skip the space following the length
      (read-sequence block-string stream))
    (read-queued-streaming-block tag (make-string-input-stream block-string)))
  ;; Skip the closing parenthesis:
  (read-char stream 't nil 't))

;;; ---------------------------------------------------------------------------
;;;  GBBopen streamer-command reader (remove soon!)
         
(defmethod saved/sent-object-reader ((char (eql #\!)) stream)
  #+ecl (declare (ignore char))
  (let ((form (read stream 't nil 't)))
    (case (first form)
      (otherwise (printv form)))))

;;; ===========================================================================
;;;   Streamers

(defun stream-instance (instance streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (print-object-for-saving/sending instance stream))))

;;; ---------------------------------------------------------------------------

(defun stream-instances (instances streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (dolist (instance instances)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-instances-of-class (unit-class-specifier streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (do-instances-of-class (instance unit-class-specifier)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-instances-on-space-instances (unit-class-specifier
                                            space-instances 
                                            streamer
                                            &key (pattern ':all)
                                                 filter-before filter-after
                                                 (use-marking *use-marking*)
                                                 (verbose *find-verbose*))
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (do-instances-on-space-instances (instance unit-class-specifier
                                                 space-instances
                                                 :pattern pattern
                                                 :filter-before filter-before
                                                 :filter-after filter-after
                                                 :use-marking use-marking
                                                 :verbose verbose)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-delete-instance (instance streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "~&#GX(~s "
            (if (typep instance 'deleted-unit-instance)
                (class-name (original-class-of instance))
                (type-of instance)))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-nonlink-slot-update (instance slot/slot-name new-value streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "~&#GS(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending new-value stream)
    (princ ")" stream))
  ;; Return the new value:
  new-value)

;;; ---------------------------------------------------------------------------

;;  Old name, remove soon:
(defun stream-slot-update (instance slot/slot-name new-value streamer)
  (stream-nonlink-slot-update instance slot/slot-name new-value streamer))

;;; ---------------------------------------------------------------------------

(defun stream-link (instance slot/slot-name other-instances streamer)
  (when other-instances
    (%with-streamer-stream (stream streamer)
      (format stream "~&#G+(~s " (type-of instance))
      (print-object-for-saving/sending (instance-name-of instance) stream)
      (format stream " ~s " (if (symbolp slot/slot-name)
                                slot/slot-name
                                (slot-definition-name slot/slot-name)))
      (print-object-for-saving/sending other-instances stream)
      (princ ")" stream))))

;;; ---------------------------------------------------------------------------

(defun stream-unlink (instance slot/slot-name other-instances streamer)
  (when other-instances
    (%with-streamer-stream (stream streamer)
      (format stream "~&#G-(~s " (type-of instance))
      (print-object-for-saving/sending (instance-name-of instance) stream)
      (format stream " ~s " (if (symbolp slot/slot-name)
                                slot/slot-name
                                (slot-definition-name slot/slot-name)))
      (print-object-for-saving/sending other-instances stream)
      (princ ")" stream))))

;;; ---------------------------------------------------------------------------

(defun stream-add-instance-to-space-instance (instance space-instance streamer)
  (unless (typep space-instance 'standard-space-instance)
    (setf space-instance 
          (find-space-instance-by-path space-instance ':with-error)))
  (%with-streamer-stream (stream streamer)
    (format stream "~&#Ga(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (type-of space-instance))
    (print-object-for-saving/sending (instance-name-of space-instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

;;  Old name, remove soon
(defun stream-add-to-space (instance space-instance streamer)
  (stream-add-instance-to-space-instance instance space-instance streamer))

;;; ---------------------------------------------------------------------------

(defun stream-remove-instance-from-space-instance (instance space-instance
                                                   streamer)
  (unless (typep space-instance 'standard-space-instance)
    (setf space-instance 
          (find-space-instance-by-path space-instance ':with-error)))
  (%with-streamer-stream (stream streamer)
    (format stream "~&#Gr(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (type-of space-instance))
    (print-object-for-saving/sending (instance-name-of space-instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

;;  Old name, remove soon
(defun stream-remove-from-space (instance space-instance streamer)
  (stream-remove-instance-from-space-instance instance space-instance streamer))

;;; ---------------------------------------------------------------------------

(defun stream-command-form (form streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "~&#G.(")
    (print-object-for-saving/sending form stream)
    (princ ")" stream)))

;;; ===========================================================================
;;;   Mirroring

(defvar *%%mirroring-enabled%%* 't)

;;; ---------------------------------------------------------------------------
;;;   Mirroring disable/enable macros

(defmacro with-mirroring-disabled ((&key) &body body)
  ;;; Disables mirroring during execution of `body'
  `(let ((*%%mirroring-enabled%%* nil))
     ,@body))

;;; ---------------------------------------------------------------------------

(defmacro with-mirroring-enabled ((&key) &body body)
  ;;; Enables mirroring during execution of `body'
  `(let ((*%%mirroring-enabled%%* 't))
     ,@body))

;;; ---------------------------------------------------------------------------
;;;   Mirroring setup

(defun add-mirroring (streamer &optional (unit-class-spec 't)
                                         (slot-names 't)
                                         (paths 't))
  (add-event-function streamer '(instance-created-event +) unit-class-spec)
  (add-event-function streamer '(instance-deleted-event +) unit-class-spec)
  (add-event-function streamer '(nonlink-slot-updated-event +) 
                      unit-class-spec)
  (add-event-function streamer '(link-event +)
                      unit-class-spec :slot-names slot-names)
  (add-event-function streamer '(unlink-event +)
                      unit-class-spec :slot-names slot-names)
  (add-event-function streamer '(instance-added-to-space-instance-event +)
                      unit-class-spec :paths paths)
  (add-event-function streamer '(instance-removed-from-space-instance-event +)
                      unit-class-spec :paths paths))

;;; ---------------------------------------------------------------------------

(defun remove-mirroring (streamer &optional (unit-class-spec 't)
                                            (slot-names 't)
                                            (paths 't))
  (remove-event-function streamer '(instance-created-event +) unit-class-spec)
  (remove-event-function streamer '(instance-deleted-event +) unit-class-spec)
  (remove-event-function streamer '(nonlink-slot-updated-event +) 
                         unit-class-spec)
  (remove-event-function streamer '(link-event +) 
                         unit-class-spec :slot-names slot-names)
  (remove-event-function streamer '(unlink-event +) 
                         unit-class-spec :slot-names slot-names)
  (remove-event-function streamer '(instance-added-to-space-instance-event +)
                         unit-class-spec :paths paths)
  (remove-event-function streamer '(instance-removed-from-space-instance-event +)
                         unit-class-spec :paths paths))

;;; ---------------------------------------------------------------------------

(defun do-instance-mirroring (evstreamers event-class 
                              &key instance &allow-other-keys)
  (when *%%mirroring-enabled%%*
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (instance-created-event
       (dolist (evstreamer evstreamers)
         (stream-instance instance (evstreamer.streamer evstreamer))))
      (instance-deleted-event
       (dolist (evstreamer evstreamers)
         (stream-delete-instance instance (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun do-space-instance-mirroring (evstreamers event-class 
                                    &key instance space-instance
                                         initialization
                                    &allow-other-keys)
  (when (and (not initialization) *%%mirroring-enabled%%*)
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (instance-added-to-space-instance-event
       (dolist (evstreamer evstreamers)
         (stream-add-to-space
          instance space-instance (evstreamer.streamer evstreamer))))
      (instance-removed-from-space-instance-event
       (dolist (evstreamer evstreamers)
         (stream-remove-from-space
          instance space-instance (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun do-nonlink-slot-mirroring (evstreamers event-class 
                                  &key instance slot current-value
                                       initialization
                                  &allow-other-keys)
  (declare (ignore event-class))
  (when (and (not initialization) *%%mirroring-enabled%%*)
    (dolist (evstreamer evstreamers)
      (stream-slot-update 
       instance slot current-value (evstreamer.streamer evstreamer)))))

;;; ---------------------------------------------------------------------------

(defun do-link-slot-mirroring (evstreamers event-class 
                               &key instance slot added-instances
                                    removed-instances initialization
                               &allow-other-keys)
  (when (and (not initialization) *%%mirroring-enabled%%*)
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (link-event
       (dolist (evstreamer evstreamers)
         (stream-link 
          instance slot added-instances (evstreamer.streamer evstreamer))))
      (unlink-event
       (dolist (evstreamer evstreamers)
         (stream-unlink
          instance slot removed-instances (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun describe-mirroring (&optional (event-classes-spec 't)
                           &rest args)
  ;;; Prints streamers for the specified event signature(s).
  (declare (dynamic-extent args))
  (multiple-value-bind (unit-class-spec slot-names paths)
      (parse-event-function-args args)
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (flet ((fn (event-class plus-subevents) 
               (ds-evfn-using-class 'describe-mirroring
                                    event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths)))
        (declare (dynamic-extent #'fn))
        (map-extended-event-classes #'fn event-classes-spec))))
  (values))

;;; ===========================================================================
;;;   Journal entities

(defun make-jnl-pathname (pathname) 
  ;; Adds type "jnl", if not supplied ; then adds defaults from
  ;; (user-homedir-pathname), as needed:
  (merge-pathnames 
   pathname
   (make-pathname :type "jnl"
                  :defaults (user-homedir-pathname))))

;;; ---------------------------------------------------------------------------

(defun make-journal-streamer (pathname
                              &rest initargs
                              &key (if-exists ':supersede)                   
                                   (package ':common-lisp-user)
                                   (external-format ':default)
                                   (read-default-float-format 
                                    *read-default-float-format*)
                                   (streamer-class 'journal-streamer)
                                   (value nil))
  (let ((stream (if (streamp pathname)
                    pathname
                    (open (make-jnl-pathname pathname)
                          ;; TODO: ** Deal with appended journaling **
                          :direction ':output
                          :if-exists if-exists
                          :external-format external-format
                          #+clozure :sharing #+clozure ':external)))
        (*package* (ensure-package package))
        (*read-default-float-format* read-default-float-format))
    (write-saving/sending-block-info stream value)
    ;; Why is this needed to prevent reading problems...?
    (princ " " stream)
    (force-output stream)
    ;; Make and return the streamer:
    (apply #'make-instance
           streamer-class
           :lock (make-lock :name "Journal streamer lock")
           :package *package*
           :external-format external-format
           :read-default-float-format read-default-float-format 
           :stream stream
           (remove-properties initargs
                              '(:if-exists :package :streamer-class :value)))))

;;; ---------------------------------------------------------------------------

(defvar *journal-load-percentage-reads-per-update* 1000)
(defvar *journal-load-percentage-hook-functions* nil)

;;; ---------------------------------------------------------------------------

(defun load-journal (pathname
                     &key (class-name-translations nil)
                          (coalesce-strings nil)
                          (disable-events 't)
                          (estimated-peak-forward-references 
                           *default-estimated-peak-forward-references*)
                          (external-format ':default)
                          (readtable *reading-saved/sent-objects-readtable*)
                          (read-eval nil))
  (with-open-file (stream (make-jnl-pathname pathname)
                   :direction ':input
                   :external-format external-format)
    (let ((*%%events-enabled%%* (not disable-events))
          (journal-length/100 (round (file-length stream) 100)))
      (with-reading-saved/sent-objects-block 
          (stream :class-name-translations class-name-translations
                  :coalesce-strings coalesce-strings
                  :estimated-peak-forward-references 
                    estimated-peak-forward-references
                  :readtable readtable
                  :read-eval read-eval)
        (with-blackboard-repository-locked ()
          ;; Read everything:
          (let ((eof-marker '#:eof)
                (counter 0))
            (catch 'close-stream
              (until (eq eof-marker (restartable-reader stream eof-marker))
                ;; Load percentage hooks:
                (when *journal-load-percentage-hook-functions*
                  (unless (plusp& (decf& counter))
                    (setf counter *journal-load-percentage-reads-per-update*)
                    (let ((load-percentage 
                           (round (file-position stream) journal-length/100)))
                      (dolist (fn *journal-load-percentage-hook-functions*)
                        (funcall fn stream load-percentage)))))))))
        ;; Return the pathname, saved/sent-time, and saved/sent-value:
        (values (pathname stream)
                *block-saved/sent-time* 
                *block-saved/sent-value*)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
