;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 25 02:57:55 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Read Saved/Sent Object Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-09-08 File created.  (Corkill)
;;;  08-03-10 Allow user-provided string-coalescing hash table in 
;;;           WITH-READING-SAVED/SENT-OBJECTS-BLOCK.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*block-saved/sent-time*
            *block-saved/sent-value*
            *default-estimated-peak-forward-references* ; not yet documented
            *forward-referenced-saved/sent-instances* ; not yet documented
            *reading-saved/sent-statistics-verbose* ; not yet documented
            *reading-saved/sent-objects-readtable* ; not yet documented
            allocate-saved/sent-instance ; not yet documented
            saved/sent-object-reader    ; not yet documented
            initialize-saved/sent-instance
            with-reading-saved/sent-objects-block)))

;;; ===========================================================================
;;;  With-reading-saved/sent-objects-block


;; Controls printing of string-coalescing information and forward-referenced
;; instances peak count:
(defvar *reading-saved/sent-statistics-verbose* 't)

;; Dynamically bound in with-reading-saved/sent-objects-block to maintain class
;; descriptions that have been read:
(defvar *read-class-descriptions-ht*)

;; Dynamically bound in with-reading-saved/sent-objects-block to
;; maintain instances that have been referenced but not yet read:
(defvar *forward-referenced-saved/sent-instances*)
;; Controls initial size of *forward-referenced-saved/sent-instances* hash
;; table:
(defvar *default-estimated-peak-forward-references* 1000)
;; Dynamically bound in with-reading-saved/sent-objects-block to determine the
;; peak count of instances that are referenced before they are read:
(defvar *forward-referenced-peak-count*)

;; Dynamically bound in with-reading-saved/sent-objects-block to hold the
;; saved/sent time & value associated with the block:
(defvar *block-saved/sent-time*)
(defvar *block-saved/sent-value*)

;; Dynamically bound in with-reading-saved/sent-objects-block to hold the
;; user-specified alist of class-name translations:
(defvar *reading-saved/sent-class-name-translations*)

;; Dynamically bound in with-reading-saved/sent-objects-block to hold the
;; string coalescing hash table and duplicate count:
(defvar *coalesce-save/sent-strings-ht*)
(defvar *duplicate-string-count*)

 ;;; ---------------------------------------------------------------------------

(defun outside-reading-saved/sent-objects-block-error (function-name)
  (error "Call to ~s is not within a ~s form"
         function-name
         'with-reading-saved/sent-objects-block))

;;; ---------------------------------------------------------------------------

(defun show-coalescing (ht duplicate-string-count)
  (when *reading-saved/sent-statistics-verbose*
    (format t "~&;; ~:d distinct ~@(~a~) strings read~
               ~%;; ~:d duplicate strings coalesced~%"
            (hash-table-test ht)
            (hash-table-count ht)
            duplicate-string-count)))

;;; ---------------------------------------------------------------------------

(defun show-forward-referenced-peak-count (peak-count)
  (when (and *reading-saved/sent-statistics-verbose*
             (not (zerop& peak-count)))
    (format t "~&;; ~:d temporarily forward-referenced instance~:p ~
                    (peak count)~%"
            peak-count)))

;;; ---------------------------------------------------------------------------

(defun check-for-undefined-instance-references ()
  (when (plusp& (hash-table-count *forward-referenced-saved/sent-instances*))
    (warn "These instances were referenced and never defined: ~s"
          (loop for key being the hash-keys 
              of *forward-referenced-saved/sent-instances* 
              collect key))))

;;; ---------------------------------------------------------------------------

(defun possibly-translate-class-name (class-name)
  ;; Performs any class-name translation for reading-saved/sent-objects-block:
  (let ((translations *reading-saved/sent-class-name-translations*))
    (if translations
        (let ((new-class-name (cdr (assq class-name translations))))
          (or new-class-name class-name))
        class-name)))

;;; ---------------------------------------------------------------------------

(defun read-saving/sending-block-info (stream class-name-translations)
  ;; Read the format-version, time, *package*, and *read-default-float-format*
  ;; recorded by with-saving/sending-block, returning the saved/sent value:
  (destructuring-bind (format-version saved/sent-time package-name 
                       read-default-float-format)
      (read stream)
    (unless (= format-version *saving/sending-block-format-version*)
      (warn "Reading old ~s format version ~a (the current version is ~a)"
            'with-saving/sending-block
            format-version
            *saving/sending-block-format-version*))
    (setf *package* (ensure-package package-name))
    (setf *read-default-float-format* read-default-float-format)
    (values saved/sent-time 
            ;; Read the saved/sent value:
            (when (>= format-version 3)
              (setf *reading-saved/sent-class-name-translations* 
                    class-name-translations)
              (read stream)))))

;;; ---------------------------------------------------------------------------

(defmacro with-reading-saved/sent-objects-block 
    ((stream 
      &key (class-name-translations nil)
           (coalesce-strings nil)
           (estimated-peak-forward-references 
            *default-estimated-peak-forward-references*)
           (readtable
            '*reading-saved/sent-objects-readtable*)
           (read-eval nil))
     &body body)
  `(with-standard-io-syntax 
     (setf *print-readably* nil)        ; Turn off to catch missing
                                        ; print-object-for-saving/sending
                                        ; methods
     (setf *readtable* ,readtable)           
     (setf *read-eval* ,read-eval)
     (let (*reading-saved/sent-class-name-translations* 
           (*read-class-descriptions-ht* (make-hash-table :test 'eq))
           (*duplicate-string-count* 0)
           (*forward-referenced-peak-count* 0)
           (*coalesce-save/sent-strings-ht* 
            (let ((.coalesce-strings. ,coalesce-strings))
              (when .coalesce-strings.
                (cond
                 ;; User provided the hash-table--use it!
                 ((hash-table-p .coalesce-strings.) 
                  .coalesce-strings.)
                 ;; User specified the initial size for the temporary hash
                 ;; table:
                 ((integerp .coalesce-strings.) 
                  (make-keys-only-hash-table-if-supported 
                   :test 'equal 
                   :size .coalesce-strings.))
                 ;; No size was specified:
                 (t (make-keys-only-hash-table-if-supported :test 'equal))))))
           (*forward-referenced-saved/sent-instances* 
            (make-keys-only-hash-table-if-supported 
             :test 'eq
             :size ,estimated-peak-forward-references)))
       (multiple-value-bind (*block-saved/sent-time* *block-saved/sent-value*)
           ;; Note that read-saving/sending-block-info also sets *package*,
           ;; *read-default-float-format*, and
           ;; *reading-saved/sent-class-name-translations* :
           (read-saving/sending-block-info ,stream ,class-name-translations)
         (multiple-value-prog1
             (progn ,@body)
           (when *coalesce-save/sent-strings-ht*
             (show-coalescing *coalesce-save/sent-strings-ht* 
                              *duplicate-string-count*))
           (show-forward-referenced-peak-count
            *forward-referenced-peak-count*)
           (check-for-undefined-instance-references))))))

;;; ===========================================================================
;;;  Standard GBBopen save/send reader methods

(defgeneric saved/sent-object-reader (char stream))
(defgeneric allocate-saved/sent-instance (class-prototype slots slot-values))
(defgeneric initialize-saved/sent-instance
    (instance incoming-slots slot-values missing-slot-names))

;;; ---------------------------------------------------------------------------
;;;  Default (error) reader

(defmethod saved/sent-object-reader (char stream)
  (declare (ignore stream))
  (error "No ~s defined for #G~c" 
         'saved/sent-object-reader
         char))

;;; ---------------------------------------------------------------------------
;;;  Unbound value reader
         
(defmethod saved/sent-object-reader ((char (eql #\U)) stream)
  #+ecl (declare (ignore char))
  (declare (ignore stream))
  unbound-value-indicator)

;;; ---------------------------------------------------------------------------
;;;  Special string reader
         
(defmethod saved/sent-object-reader ((char (eql #\")) stream)
  (unread-char char stream)
  (let ((string (read stream)))
    (if *coalesce-save/sent-strings-ht*
        (let ((coalesced-string 
               (gethash string *coalesce-save/sent-strings-ht*)))
          (cond (coalesced-string 
                 (incf *duplicate-string-count*)
                 coalesced-string )
                (t (setf (gethash string *coalesce-save/sent-strings-ht*)
                         string))))
        string)))

;;; ---------------------------------------------------------------------------
;;;  Non-simple vector reader
         
(defmethod saved/sent-object-reader ((char (eql #\()) stream)
  #+ecl (declare (ignore char))
  (let* ((dimension (read stream))
         (adjustable? (read stream))
         (fill-pointer (read stream))
         (element-type (read stream))
         (vector (make-array (list dimension)
                             :adjustable adjustable? 
                             :fill-pointer fill-pointer
                             :element-type element-type)))
    (dotimes (i (or fill-pointer dimension))
      (declare (fixnum i))
      (setf (aref vector i) (read stream)))
    (read-char stream)                  ; skip close parenthesis
    vector))
  
;;; ---------------------------------------------------------------------------
;;;  Non-simple bit-vector reader
         
(defmethod saved/sent-object-reader ((char (eql #\*)) stream)
  #+ecl (declare (ignore char))
  (read-char stream)                    ; skip open parenthesis
  (let* ((dimension (read stream))
         (adjustable? (read stream))
         (fill-pointer (read stream))
         (bit-vector (make-array (list dimension)
                                 :adjustable adjustable? 
                                 :fill-pointer fill-pointer
                                 :element-type 'bit)))
    (dotimes (i (or fill-pointer dimension))
      (declare (fixnum i))
      (setf (aref bit-vector i) 
            (ecase (read-char stream)
              (#\0 0)
              (#\1 1))))
    (read-char stream)                  ; skip close parenthesis
    bit-vector))
  
;;; ---------------------------------------------------------------------------
;;;  Non-simple array reader
         
(defmethod saved/sent-object-reader  ((char (eql #\A)) stream)
  #+ecl (declare (ignore char))
  (read-char stream)                    ; skip open parenthesis
  (let* ((dimensions (read stream))
         (adjustable? (read stream))
         (element-type (read stream))
         (array (make-array dimensions
                            :adjustable adjustable? 
                            :element-type element-type)))
    (dotimes (i (apply #'* dimensions))
      (declare (fixnum i))
      (setf (row-major-aref array i) (read stream)))
    (read-char stream)                  ; skip close parenthesis
    array))
  
;;; ---------------------------------------------------------------------------
;;;  Class (reference) reader
         
(defmethod saved/sent-object-reader ((char (eql #\C)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (class-name)
      (read stream 't nil 't)
    (find-class (possibly-translate-class-name class-name) 't)))

;;; ---------------------------------------------------------------------------
;;;  Class-description reader

(defmethod saved/sent-object-reader ((char (eql #\D)) stream)
  #+ecl (declare (ignore char))
  ;; Check that we are in a with-saving/sending-block:
  (unless (boundp '*read-class-descriptions-ht*)
    (outside-reading-saved/sent-objects-block-error 'saved/sent-object-reader))
  (destructuring-bind (class-name &rest slot-names)
      (read stream 't nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let* ((class (find-class class-name 't))
           (class-slots (copy-list (progn
                                     (ensure-finalized-class class)
                                     (class-slots class))))
           (supplied-slots nil))
      (dolist (slot-name slot-names)
        ;; Check that incoming slot-name is present in the current class
        ;; definition:
        (let ((slot
               ;; (car (member ...)) with :test & :key often optimizes better
               ;; than (find ...):
               (car (member slot-name class-slots
                            :key #'slot-definition-name
                            :test #'eq))))
          (unless slot
            (warn "Slot ~s is no longer defined for class ~s; saved values ~
                   for this slot will be discarded."
                  slot-name class-name))
          (push slot supplied-slots)
          (setf class-slots (delq-one slot class-slots))))
      ;; Save the incoming class, class-slots, and the missing slot-names for this
      ;; class:
      (setf (gethash class-name *read-class-descriptions-ht*)
            (list* (find-class class-name 't)
                   (nreverse supplied-slots)
                   (mapcar #'slot-definition-name class-slots)))))
  ;; The next form should always be the object that triggered the class
  ;; definition, so we read it here (to "hide" the class-definition and return
  ;; the object in its place):
  (read stream 't nil 't))

;;; ---------------------------------------------------------------------------
;;;  Function reader
         
(defmethod saved/sent-object-reader ((char (eql #\F)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (function-name)
      (read stream 't nil 't)
    (coerce function-name 'function)))

;;; ---------------------------------------------------------------------------
;;;  Hash-table reader

(defmethod saved/sent-object-reader ((char (eql #\H)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (ht-test ht-count ht-values &rest keys-and-values)
      (read stream 't nil 't)
      (cond 
       (ht-values 
        (let ((ht (make-hash-table :test ht-test :size ht-count)))
          (loop for (key value) on keys-and-values by #'cddr
              do (setf (gethash key ht) value))
          ht))
       ;; value-less hash table:
       (t (let ((ht (make-keys-only-hash-table-if-supported
                     :test ht-test :size ht-count)))
            (dolist (key keys-and-values)
              (setf (gethash key ht) 't))
            ht)))))

;;; ---------------------------------------------------------------------------
;;;  Standard-object reader

(defmethod allocate-saved/sent-instance ((class-prototype standard-object)
                                         slots slot-values)
  (declare (ignore slots slot-values))
  (allocate-instance (class-of class-prototype)))

;;; ---------------------------------------------------------------------------

(defvar *%%skip-gbbopen-shared-initialize-method-processing%%* nil)

(defmethod initialize-saved/sent-instance ((instance standard-object)
                                           slots slot-values missing-slot-names)
  (let ((class (class-of instance)))
    (loop for slot in slots
        and value in slot-values
        do (when slot
             ;; Set the slot value, unless it is to remain unbound:
             (unless (eq value unbound-value-indicator)
               ;; Lispworks can't use slot-value-using-class here
               ;; (prior to shared-initialize?):
               #+lispworks
               (setf (slot-value instance (slot-definition-name slot)) value)
               #-lispworks
               (setf (slot-value-using-class class instance slot) 
                     value))))
    ;; Initialize any remaining slots:
    (when missing-slot-names
      (let ((*%%skip-gbbopen-shared-initialize-method-processing%%* 't))
        (shared-initialize instance missing-slot-names)))))

;;; ---------------------------------------------------------------------------

(defmethod saved/sent-object-reader ((char (eql #\I)) stream)
  #+ecl (declare (ignore char))
  ;; Check that we are in a with-saving/sending-block:
  (unless (boundp '*read-class-descriptions-ht*)
    (outside-reading-saved/sent-objects-block-error 'saved/sent-object-reader))
  (destructuring-bind (class-name &rest slot-values)
      (read stream 't nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (destructuring-bind (unit-class incoming-slots . missing-slot-names)
        (or (gethash class-name *read-class-descriptions-ht*)
            (error "No class description has been read for class ~s"
                   class-name))
      (let ((instance (allocate-saved/sent-instance 
                       (class-prototype unit-class)
                       incoming-slots
                       slot-values)))
        (initialize-saved/sent-instance 
         instance incoming-slots slot-values missing-slot-names)
        instance))))

;;; ---------------------------------------------------------------------------
;;;  Package (reference) reader
         
(defmethod saved/sent-object-reader ((char (eql #\P)) stream)
  #+ecl (declare (ignore char))
  (destructuring-bind (package-name)
      (read stream 't nil 't)
    (ensure-package package-name)))

;;; ===========================================================================
;;;  The saved/sent-object readtable

(defun saved/sent-reader-dispatcher (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (saved/sent-object-reader (read-char stream) stream))

;;; ---------------------------------------------------------------------------

(defparameter *reading-saved/sent-objects-readtable*
    (let ((*readtable* (copy-readtable)))
      ;; Duplicate infinity reader (from declared-numerics.lisp):
      (safely-set-dispatch-macro-character
       #\# #\@ 
       #-cormanlisp 'inf-reader
       #+cormanlisp #'inf-reader)
      (safely-set-dispatch-macro-character 
       #\# #\G 
       'saved/sent-reader-dispatcher)
      *readtable*))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
