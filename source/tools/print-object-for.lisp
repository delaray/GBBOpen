;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/print-object-for.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jun 27 16:48:49 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *             Print Object For Saving & For Sending Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-07 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*print-object-for-sending*
            *save/send-references-only*
            *warn-about-nonportable-saving/sending* ; not yet documented
            omitted-slots-for-saving/sending
            print-object-for-saving/sending
            print-slot-for-saving/sending
            unable-to-save/send-object-error ; not documented
            with-saving/sending-block)))

;;; ---------------------------------------------------------------------------
;;;  Control warnings when:
;;;    1. a hash-table with a non-standard test is saved/sent

(defvar *warn-about-nonportable-saving/sending* 't)

;;; ---------------------------------------------------------------------------

(defgeneric omitted-slots-for-saving/sending (instance))
(defgeneric print-object-for-saving/sending (object stream))
(defgeneric print-slot-for-saving/sending (instance slot-name stream))

;;; ---------------------------------------------------------------------------

(defvar *print-object-for-sending* nil)
(defvar *save/send-references-only* 't)

;;; ---------------------------------------------------------------------------
;;;  Saving/sending-block format version

(defparameter *saving/sending-block-format-version* 4)

;;; ---------------------------------------------------------------------------

(defun write-saving/sending-block-info (stream &optional saved/sent-value)
  ;; Record the format-version, saved/sent-time, *package*,
  ;; *read-default-float-format*, and saved/sent-value for reading by
  ;; read-saving/sending-block-info (in read-object.lisp) which is called by
  ;; with-reading-object-block:
  (format stream "(~s ~s :~a ~s)~%"
          *saving/sending-block-format-version*
          (get-universal-time)
          ;; We write package-name as keyword symbol, to avoid
          ;; package/modern-mode issues:
          (package-name *package*)
          *read-default-float-format*)
  (print-object-for-saving/sending saved/sent-value stream)
  ;; Required to help reading:
  (write-char #\space stream))

;;; ===========================================================================
;;;  With-sending/saving-block

(defvar *recorded-class-descriptions-ht*)

(defmacro with-saving/sending-block ((stream 
                                      &key (package '':common-lisp)
                                           (read-default-float-format 
                                            ''single-float)
                                           (value nil))
                                     &body body)
  (with-gensyms (package-sym)
    `(with-standard-io-syntax 
       (let ((*recorded-class-descriptions-ht* (make-hash-table :test 'eq))
             (stream ,stream))
         (setf *package*
               (let ((,package-sym ,package))
                 (if (packagep ,package-sym)
                     ,package-sym
                     (ensure-package ,package-sym))))
         (setf *read-default-float-format* ,read-default-float-format)
         (write-saving/sending-block-info stream ,value)
               ,@body))))

;;; ---------------------------------------------------------------------------

(defun outside-saving/sending-block-error (function-name)
  (error "Call to ~s is not within a ~s form"
         function-name
         'with-saving/sending-block))

;;; ---------------------------------------------------------------------------

(defun unable-to-save/send-object-error (object)
  (let ((*print-readably* nil))
    (error "Unable to save/send ~s" object)))

;;; ---------------------------------------------------------------------------
;;;  Class Descriptions

(defun print-class-description-for-saving/sending (class stream)
  ;; Check that we are in a with-saving/sending-block:
  (unless (boundp '*recorded-class-descriptions-ht*)
    (outside-saving/sending-block-error 'print-class-description-for-saving/sending))
  (let ((class-name (class-name class)))
    (multiple-value-bind (slots-for-saving/sending present-p)
        (gethash class-name *recorded-class-descriptions-ht*)
      (unless present-p
        (error "Class ~s has not been recorded in this saving/sending block"
               class-name))
      (format stream "~&#GD(~s" class-name)
      (dolist (slot slots-for-saving/sending)
        (let ((slot-name (slot-definition-name slot)))
          (format stream " ~s" slot-name)))
      (write-char #\) stream)))
  class)

;;; ===========================================================================
;;;  Slots-for-saving/sending methods

(defun slots-for-saving/sending (instance stream perform-built-in-class-check?)
  ;; Caches the slots that should be saved/sent for the class of `instance',
  ;; saving/sending the class description, if we've not done so in the
  ;; save/send block.  Always return the list of slots that should be
  ;; saved/sent.
  (let* ((class (class-of instance))
         (class-name (class-name class)))
    ;; Check that we are in a with-saving/sending-block:
    (unless (boundp '*recorded-class-descriptions-ht*)
      (outside-saving/sending-block-error 'slots-for-saving/sending))
    (multiple-value-bind (slots-for-saving/sending present-p)
        (gethash class-name *recorded-class-descriptions-ht*)
      ;; Determine the slots that should be saved/sent and save/send the class
      ;; description, if we've not done so already in this block:
      (unless present-p
        ;; Clozure CL has some built-in-class objects that are also
        ;; standard-objects (e.g., CCL::BASIC-TCP-STREAM). Check here (on all
        ;; CLs, just to be paranoid) if instance is a built-in-class object:
        (when (and perform-built-in-class-check?
                   (typep class 'built-in-class))
          (error "~s instance ~s does not have a specialized ~s and cannot be ~
                  saved/sent" 
                 'built-in-class
                 instance
                 'print-object-for-saving/sending))
        (let ((omitted-slot-names (omitted-slots-for-saving/sending instance)))
          (setf slots-for-saving/sending
                (setf (gethash class-name *recorded-class-descriptions-ht*)
                      (flet ((fn (slot)
                               (memq (slot-definition-name slot)
                                     omitted-slot-names)))
                        (declare (dynamic-extent #'fn))
                        (remove-if #'fn (class-slots class))))))
        (print-class-description-for-saving/sending class stream))
      slots-for-saving/sending)))

;;; ---------------------------------------------------------------------------
;;;  Default

(defmethod omitted-slots-for-saving/sending (object)
  (declare (ignore object))
  nil)

;;; ---------------------------------------------------------------------------

(defun check-for-displaced-array (array)
  (when (array-displacement array)
    (error "Saving/sending a displaced array is not supported.")))

;;; ===========================================================================
;;;  Standard print-object-for-saving/sending methods
;;;
;;; ---------------------------------------------------------------------------
;;;  Default

(defmethod print-object-for-saving/sending (object stream)
  (prin1 object stream))

;;; ---------------------------------------------------------------------------
;;;  Unbound value indicator

(defmethod print-object-for-saving/sending ((object
                                             (eql unbound-value-indicator))
                                            stream)
  (princ "#GU" stream)
  object)

;;; ---------------------------------------------------------------------------
;;;  Lists

(defmethod print-object-for-saving/sending ((cons cons) stream)
  (cond
   ;; Compact (quote object) printing:
   ((and (eq (first cons) 'quote)
         (null (cddr cons))
         (not (null (cdr cons))))
    (write-char #\' stream)
    (print-object-for-saving/sending (second cons) stream))
   ;; Regular list printing:
   (t (let ((ptr cons))
        (write-char #\( stream)
        (print-object-for-saving/sending (car ptr) stream)
        (loop
          (when (atom (setf ptr (cdr ptr))) (return))
          (write-char #\space stream)
          (print-object-for-saving/sending (car ptr) stream))
        (unless (null ptr)
          ;; Dotted list:
          (princ " . " stream)
          (print-object-for-saving/sending ptr stream))
        (write-char #\) stream))))
  cons)

;;; ---------------------------------------------------------------------------
;;;  Strings (coalescable)

(defmethod print-object-for-saving/sending ((string string) stream)
  (princ "#G" stream)
  (prin1 string stream))

;;; ---------------------------------------------------------------------------
;;;  Vectors

(defmethod print-object-for-saving/sending ((vector vector) stream)
  (typecase vector
    ;; A simple vector:
    (simple-vector (format stream "#("))
    ;; A complex vector:
    (t (check-for-displaced-array vector)
       (let ((has-fill-pointer? (array-has-fill-pointer-p vector)))
         (format stream "#G(~s ~s ~s ~s "
                 (sole-element (array-dimensions vector))
                 (adjustable-array-p vector)
                 (when has-fill-pointer? (fill-pointer vector))
                 (array-element-type vector)))))
  ;; Print the elements:
  (dotimes (i (length vector))
    (declare (fixnum i))
    (unless (zerop i) (write-char #\space stream))
    (print-object-for-saving/sending (aref vector i) stream))
  (write-char #\) stream)
  vector)

;;; ---------------------------------------------------------------------------
;;;  Bit-vectors 

(defmethod print-object-for-saving/sending ((bit-vector bit-vector) stream)
  (typecase bit-vector
    ;; A simple bit-vector:
    (simple-bit-vector (prin1 bit-vector stream))
    ;; A complex bit-vector:
    (t (check-for-displaced-array bit-vector)
       (let ((has-fill-pointer? (array-has-fill-pointer-p bit-vector))
             (adjustable? (adjustable-array-p bit-vector)))
         (format stream "#G*(~s ~s ~s "
                 (sole-element (array-dimensions bit-vector))
                 adjustable?              
                 (when has-fill-pointer? (fill-pointer bit-vector))))
       (dotimes (i (length bit-vector))
         (declare (fixnum i))
         (prin1 (aref bit-vector i) stream))
       (write-char #\) stream)))
  bit-vector)

;;; ---------------------------------------------------------------------------
;;;  Arrays

(defmethod print-object-for-saving/sending ((array array) stream)
  (let ((dimensions (array-dimensions array))
        (index -1))
    (declare (fixnum index))
    (labels
        ((helper (dimensions)
           (cond 
            ((null dimensions)
             (print-object-for-saving/sending
              (row-major-aref array (the fixnum (incf index))) stream))
            (t (let ((dimension (first dimensions)))
                 (declare (fixnum dimension))
                 (write-char #\( stream)
                 (dotimes (i dimension)
                   (declare (fixnum i))
                   (unless (zerop i)
                     (write-char #\space stream))
                   (helper (rest dimensions)))
                 (write-char #\) stream))))))
      (cond 
        ;; A simple array:
       ((and (simple-array-p array)
             ;; A simple-array is not required to hold all element types (unlike a simple-vector):
             (eq (array-element-type array) 't))
        (format stream "#~sA" (array-rank array))
        (helper dimensions))
       ;; A complex array:
       (t (check-for-displaced-array array)
          (format stream "#GA(~s ~s ~s "
                  (array-dimensions array)                   
                  (adjustable-array-p array)
                  (array-element-type array))
          (dotimes (i (array-total-size array))
            (declare (fixnum i))
            (unless (zerop i) (write-char #\space stream))
            (print-object-for-saving/sending (row-major-aref array i) stream))
          (write-char #\) stream)))))
  array)

;;; ---------------------------------------------------------------------------
;;;  Structures

(defmethod print-object-for-saving/sending ((structure structure-object) 
                                            stream)
  (let ((slots-for-saving/sending   
         ;; Cache/write the class description, if needed & always get the
         ;; slots to be written:
         (slots-for-saving/sending structure stream nil)))
    (format stream "#S(~s" (type-of structure))
    (dolist (slot slots-for-saving/sending)
      (let ((slot-name (slot-definition-name slot)))
        (format stream " :~a " slot-name)
        (print-object-for-saving/sending
         (slot-value structure slot-name) stream)))
    (write-char #\) stream))
  structure)

;;; ---------------------------------------------------------------------------
;;;  Classes (references only)

(defmethod print-object-for-saving/sending ((class standard-class) stream)
  (format stream "~&#GC(~s)" (class-name class))
  class)

;;; ---------------------------------------------------------------------------
;;;  Instances

(defmethod print-slot-for-saving/sending ((instance standard-object)
                                          slot-name
                                          stream)
  (print-object-for-saving/sending (slot-value instance slot-name) stream))

;;; ---------------------------------------------------------------------------

(defmethod print-object-for-saving/sending ((instance standard-object) stream)
  (let ((slots-for-saving/sending   
         ;; Cache/write the class description, if needed, always getting the
         ;; slots that are to be written:
         (slots-for-saving/sending 
          instance stream 
          ;; performing the check for built-in-class instances:
          't))
        (class (class-of instance)))
    (format stream "~&#GI(~s" (type-of instance))
    (dolist (slot slots-for-saving/sending)
      (write-char #\space stream)      
      (if (slot-boundp-using-class class instance slot)
          (print-slot-for-saving/sending 
           instance (slot-definition-name slot) stream)
          ;; Unbound value indicator:
          (format stream "#GU")))
    (write-char #\) stream))
  instance)

;;; ---------------------------------------------------------------------------
;;;  Hash Tables

(defmethod print-object-for-saving/sending ((hash-table hash-table) stream)
  (let ((keys-and-values-hash-table? 
         #+allegro (excl:hash-table-values hash-table)
         #-has-keys-only-hash-tables
         't)
        (hash-table-test (hash-table-test hash-table)))
    (unless (and *warn-about-nonportable-saving/sending*
                 (memq hash-table-test '(eq eql equal equalp)))
      (warn "Hash-table-test ~s is not portable." hash-table-test))
    (format stream "#GH(~s ~s ~s"
            hash-table-test
            (hash-table-count hash-table)
            keys-and-values-hash-table?)
    (flet ((write-key-value (key value)
             (write-char #\space stream)
             (print-object-for-saving/sending key stream)
             (write-char #\space stream)
             (print-object-for-saving/sending value stream))
           #+has-keys-only-hash-tables
           (write-key-only (key value)
             (declare (ignore value))
             (write-char #\space stream)
             (print-object-for-saving/sending key stream)))
      (declare (dynamic-extent #'write-key-value
                               #+has-keys-only-hash-tables
                               #'write-key-only))
      (maphash 
       (if keys-and-values-hash-table? 
           #'write-key-value
           #+has-keys-only-hash-tables
           #'write-key-only)
       hash-table))
    (write-char #\) stream)
    hash-table))

;;; ---------------------------------------------------------------------------
;;;  Packages (references only)

(defmethod print-object-for-saving/sending ((package package) stream)
  (format stream "~&#GP(~s)" (package-name package))
  package)

;;; ---------------------------------------------------------------------------
;;;  Functions

(defun print-function-for-saving/sending (fn stream)
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression fn)
    #+(or cmu sbcl)
    (declare (ignore closure-p))
    (cond
     ;; CMUCL and SBCL always return that closure-p is true, so we ignore
     ;; their closure-p value and risk that `fn' might be a non-trivial
     ;; closure that can't be represented externally.  Everywhere else, we
     ;; don't allow closure saving/sending:
     #-(or cmu sbcl)
     (closure-p (unable-to-save/send-object-error fn))
     ;; We have the lambda-expression:
     (lambda-expression (format stream "#GF(~s)" lambda-expression))
     ;; We have a useful name.  CL implementations are free to always return
     ;; nil as name or return an object that is not valid for use as a name in
     ;; a function form.  Fortunately, implementations usually provide a
     ;; useful name value:
     ((and (symbolp name)
           (not (keywordp name)))
      (format stream "#GF(~s)" name))
     ;; Anything else, and we're out of luck:
     (t (unable-to-save/send-object-error fn))))
  fn)

;;; ---------------------------------------------------------------------------

;; The class-precedence-list of generic-function has standard-object
;; before function, so they need their own method to print like functions
;; rather than CLOS objects:

(defmethod print-object-for-saving/sending ((fn generic-function) stream)
  (let ((name (generic-function-name fn)))
    (if (symbolp name)
        ;; We assume the generic function is not a non-trival closure:
        (format stream "#GF(~s)" name)
        (print-function-for-saving/sending fn stream))))

;;; ---------------------------------------------------------------------------

(defmethod print-object-for-saving/sending ((fn function) stream)
  (print-function-for-saving/sending fn stream))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
