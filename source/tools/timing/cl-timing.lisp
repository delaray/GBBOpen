;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Sep  7 17:12:14 2011 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        CL Implementation Timing 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-02-08 File created.  (Corkill)
;;;  09-07-11 Add HASH-TABLE-COUNT timing.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools-user)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Import symbols (defined in tools/atable.lisp):
  (import '(gbbopen-tools::%atable-data-list
            gbbopen-tools::*atable-transition-sizes*
            gbbopen-tools::atable-data
            gbbopen-tools::auto-transition-margin
            gbbopen-tools::determine-key/value-atable-index
            gbbopen-tools::determine-keys-only-atable-index
            gbbopen-tools::eset-transition-size
            gbbopen-tools::et-transition-size
            gbbopen-tools::fastest-fixnum-div-operator
            gbbopen-tools::get-et%timing
            gbbopen-tools::in-eset%timing)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

;;; ---------------------------------------------------------------------------
;;;   Timing parameters

;; Size for timing basic CL operations:
(defparameter *timing-iterations*
    #+clisp 2000000                     ; CLISP is slow
    #+abcl  2000000                     ; as is ABCL
    #+ecl   6000000                     ; ECL is a bit faster
    #-(or abcl clisp ecl) 
    10000000)

;; Size for timing atable transitions:
(defparameter *transition-timing-iterations*
    #+clisp 100000                      ; CLISP is slow
    #+abcl 100000                       ; as is ABCL
    #+ecl 1000000                       ; ECL is a bit faster
    #-(or abcl clisp ecl)
    10000000)

;; Size for timing unit-instance operations:
(defparameter *timing-instances* 100000) 

(defparameter *max-list-test-size* 140)

(defparameter *required-contiguous-misses* 3)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%timing-result%*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro brief-timer (n &body body)
    `(with-full-optimization ()
       ;; Do one untimed trial to prepare everything:
       (let ((i 0))
         (declare (ignorable i))
         (setf *%timing-result%* ,@body))
       (let ((%start-time% (get-internal-run-time)))
         (dotimes (i (& ,n))
           (declare (fixnum i))
           (setf *%timing-result%* ,@body))
         (locally (declare (notinline -))
           (- (get-internal-run-time) %start-time%))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro indexed-timer (n max-index &body body)
    ;; Binds and increments `test-index' from 1 to `max-index' during timing:
    `(with-full-optimization ()
       (let ((test-index ,max-index))
         ;; Do one worst-case untimed trial to prepare everything:
         (setf *%timing-result%* ,@body)
         (setf test-index 1)
         (let ((%start-time% (get-internal-run-time)))
           (dotimes (i (& ,n))
             (declare (fixnum i))
             (setf *%timing-result%* ,@body)
             (incf& test-index)
             (when (>& test-index ,max-index)
               (setf test-index 1)))
           (locally (declare (notinline -))
             (- (get-internal-run-time) %start-time%)))))))
             
;;; ---------------------------------------------------------------------------

(defun fformat (stream &rest args)
  ;; Format followed by a force-output on the stream:
  (declare (dynamic-extent args))
  (apply #'format stream args)
  (force-output (if (eq stream 't) *standard-output* stream)))

;;; ---------------------------------------------------------------------------

(defun %make-key-string (i)
  (make-string 5 :initial-element (code-char (+& i #.(char-code #\A)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun %hidden-identity 
      ;; Identity function for testing (one that compilers don't know about to
      ;; optimize):
      (x) x))

;;; ---------------------------------------------------------------------------

(defun format-ticks (ticks)
  (format t "~,2f seconds"
          (locally (declare (optimize (speed 0) (safety 3)))
            (/ ticks
               #.(float internal-time-units-per-second)))))
  
;;; ---------------------------------------------------------------------------

(defun do-list-timing (&optional (iterations *timing-iterations*))
  (let* ((max-size *max-list-test-size*)
         (list (loop for i fixnum from 1 to max-size collect i))
         (alist (loop for i fixnum from 1 to max-size collect (cons i i)))
         (warning-time 0)
         (time 0))
    (declare (type list list alist))
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (brief-timer iterations ,form))
              (format-ticks time)
              (when (and (not (zerop& warning-time))
                         (>& time warning-time))
                (format t " *****"))
              (terpri))))
      (fformat t "~&;;   Do nothing timing (~:d iterations)..."
               iterations)
      (time-it nil)
      (fformat t "~&;;   Fastest memq timing (~:d iterations)..."
               iterations)
      (time-it (car (memq 1 (the list list))))
      (setf warning-time (*& 2 time))
      (fformat t "~&;;   Fastest member timing (~:d iterations)..."
               iterations)
      (time-it (car (member 1 (the list list))))
      (fformat t "~&;;   Fastest member eq timing (~:d iterations)..."
               iterations)
      (time-it (car (member 1 (the list list) :test #'eq)))
      (fformat t "~&;;   Fastest member eq key timing (~:d iterations)..."
               iterations)
      (time-it (car (member 1 (the list list)
                            :test #'eq :key #'%hidden-identity)))
      (fformat t "~&;;   Fastest member-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 item)))
        (declare (dynamic-extent #'fn))
        (time-it (car (member-if #'fn (the list list)))))
      (fformat t "~&;;   Fastest member-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 (%hidden-identity item))))
        (declare (dynamic-extent #'fn))
        (time-it (car (member-if #'fn (the list list)))))
      (fformat t "~&;;   Fastest find timing (~:d iterations)..."
               iterations)
      (time-it (find 1 (the list list)))
      (fformat t "~&;;   Fastest find eq timing (~:d iterations)..."
               iterations)
      (time-it (find 1 (the list list) :test #'eq))
      (fformat t "~&;;   Fastest find eq key timing (~:d iterations)..."
               iterations)
      (time-it (find 1 (the list list) 
                     :test #'eq :key #'%hidden-identity))
      (fformat t "~&;;   Fastest find-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 item)))
        (declare (dynamic-extent #'fn))
        (time-it (find-if #'fn (the list list))))
      (fformat t "~&;;   Fastest find-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 (%hidden-identity item))))
        (declare (dynamic-extent #'fn))
        (time-it (find-if #'fn (the list list))))
      ;; Assoc
      (fformat t "~&;;   Fastest assq timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assq 1 (the list alist))))
      (fformat t "~&;;   Fastest assoc eq timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assoc 1 (the list alist) :test #'eq)))
      (fformat t "~&;;   Fastest assoc eq key timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assoc 1 (the list alist) 
                           :test #'eq
                           :key #'%hidden-identity)))
      (fformat t "~&;;   Fastest assoc-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (key) (eq 1 key)))
        (declare (dynamic-extent #'fn))
        (time-it (cdr (assoc-if #'fn (the list alist)))))
      (fformat t "~&;;   Fastest assoc-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (key) (eq 1 (%hidden-identity key))))
        (declare (dynamic-extent #'fn))
        (time-it (cdr (assoc-if #'fn (the list alist)))))
      ;; Deletes
      (setf warning-time 0)
      (fformat t "~&;;   Fastest delq-one timing (~:d iterations)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delq-one 1 (the cons cons)))))
      (setf warning-time (*& 2 time))
      (fformat t "~&;;   Fastest delete eq :count 1 timing (~:d iterations)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delete 1 (the list list)
                                          :test #'eq
                                          :count 1))))
      (setf list (list 2))
      (fformat t "~&;;   Fastest delq timing (~:d iterations, 2 elements)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delq 1 list))))
      (fformat t "~&;;   Fastest delete eq timing (~:d iterations, 2 elements)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delete 1 (the list list)
                                          :test #'eq)))))))

;;; ---------------------------------------------------------------------------

(defun do-computed-case-timing (&optional (iterations *timing-iterations*))
  (flet
      ((time-case (n)
         (declare (fixnum n))
         (with-full-optimization ()
           (case n
             (0 (+& 10 n))
             (1 (+&  9 n))
             (2 (+& -8 n))
             (3 (+&  7 n))
             (4 (+& -6 n))
             (5 (+&  5 n))
             (6 (+& -4 n))
             (7 (+&  3 n))
             (8 (+& -2 n))
             (9 (+&  1 n))
             (10 (+& -1 n))
             (11 (+&  2 n))
             (12 (+& -3 n))
             (13 (+&  4 n))
             (14 (+& -5 n))
             (15 (+&  6 n))
             (16 (+& -7 n))
             (17 (+&  8 n))
             (18 (+& -9 n))
             (19 (+& 10 n))
             (20 (+& 10 n))
             (21 (+&  9 n))
             (22 (+& -8 n))
             (23 (+&  7 n))
             (24 (+& -6 n))
             (25 (+&  5 n))
             (26 (+& -4 n))
             (27 (+&  3 n))
             (28 (+& -2 n))
             (29 (+&  1 n))
             (30 (+& -1 n))
             (31 (+&  2 n))
             (32 (+& -3 n))
             (33 (+&  4 n))
             (34 (+& -5 n))
             (35 (+&  6 n))
             (36 (+& -7 n))
             (37 (+&  8 n))
             (38 (+& -9 n))
             (39 (+& -10 n))))))
    (fformat t "~&;;   Fastest computed case timing (~:d iterations)..."
             iterations)
    (let ((fast-time (brief-timer iterations (time-case 0))))
      (format-ticks fast-time)
      (fformat t "~&;;   Slower computed case timing (~:d iterations)..."
               iterations)
      (let ((slower-time (brief-timer iterations (time-case 39))))
        (format-ticks slower-time)
        (when (>& slower-time (*& 2 fast-time))
          (format t " *****"))
        (terpri)))))
  
;;; ---------------------------------------------------------------------------

(defun do-ht-timing (&optional (iterations *timing-iterations*))
  (let (#+has-keys-only-hash-tables
        (keys-only-ht (make-keys-only-hash-table-if-supported :test #'eq))
        (ht (make-hash-table :test #'eq))
        (max-size *max-list-test-size*)
        time)
    (dotimes (i max-size)
      #+has-keys-only-hash-tables
      (setf (gethash i keys-only-ht) 't)
      (setf (gethash i ht) 'i))
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (indexed-timer iterations max-size ,form))
              (format-ticks time)
              (terpri))))
      ;; Hash table timings:
      #-has-keys-only-hash-tables
      (format t "~&;; * Keys-only hash tables are standard key/value hash ~
                        tables:")
      #+has-keys-only-hash-tables
      (progn
        (fformat t "~&;;   Keys-only eq hash table timing (~:d iterations)..."
                 iterations)
        (time-it (gethash 1 keys-only-ht)))
      (fformat t "~&;;   Normal eq hash table timing (~:d iterations)..."
               iterations)
      (time-it (gethash 1 ht))      
      (let ((hash-time time))
        (fformat t "~&;;   Normal eq hash-table-count timing (~:d iterations)..."
                 iterations)
        (time-it (hash-table-count ht))
        (when (>& time hash-time)
          (format t " *****"))))))

;;; ---------------------------------------------------------------------------

(defun do-eset-timing (&optional (iterations *timing-iterations*))
  (let ((transition
         (max& 1 (+& eset-transition-size auto-transition-margin)))
        time)
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (indexed-timer iterations transition ,form))
              (format-ticks time)
              (terpri))))
      ;; ESET timings:
      (let ((eset-list (make-eset))
            (eset-ht (make-eset))
            (ht (make-keys-only-hash-table-if-supported :test #'eq)))
        ;; Fill them:
        (loop for i fixnum from transition downto 1 do
              (add-to-eset i eset-list))
        (dotimes (i (1+& transition))
          (add-to-eset i eset-ht)
          (setf (gethash i ht) 't))
        ;; Time them:
        (fformat t "~&;;   Do nothing timing (~:d iterations)..."
                 iterations)
        (time-it nil)
        (fformat t "~&;;   Fastest ESET (list) timing (~:d iterations)..."
                 iterations)
        (time-it (in-eset 1 eset-list))
        (fformat t "~&;;   Slowest ESET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (in-eset transition eset-list))
        (fformat t "~&;;   Average ESET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (in-eset test-index eset-list))
        (fformat t "~&;;   ESET (transitioned) timing (~:d iterations)..."
                 iterations)
        (time-it (in-eset test-index eset-ht))
        (fformat t "~&;;   eq ~:[key/value~;keys-only~] hash table timing ~
                           (~:d iterations)..."
                 (memq ':has-keys-only-hash-tables *features*)
                 iterations)
        (time-it (gethash test-index ht))
        (clear-eset eset-list)
        (fformat t "~&;;   Fastest add/delete ESET (list) timing ~
                           (~:d iterations)..."
                 iterations)
        (time-it (progn (add-to-eset 1 eset-list)
                        (delete-from-eset 1 eset-list)))
        (fformat t "~&;;   eq ~:[key/value~;keys-only~] hash table ~
                             add/delete timing (~:d iterations)..."
                 (memq ':has-keys-only-hash-tables *features*)
                 iterations)
        (time-it (progn (setf (gethash 1 ht) 't)
                        (remhash 1 ht)))))))
  
;;; ---------------------------------------------------------------------------

(defun do-et-timing (&optional (iterations *timing-iterations*))
  (let ((transition
         (max& 1 (+& et-transition-size auto-transition-margin)))
        time)
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (indexed-timer iterations transition ,form))
              (format-ticks time)
              (terpri))))
      ;; ET timings:
      (let ((et-list (make-et))
            (et-ht (make-et))
            (ht (make-hash-table :test #'eq)))
        ;; Fill them:
        (loop for i fixnum from transition downto 1 do
              (setf (get-et i et-list) i))
        (dotimes (i (1+& transition))
          (setf (get-et i et-ht) i)
          (setf (gethash i ht) i))
        ;; Time them:
        (fformat t "~&;;   Do nothing timing (~:d iterations)..."
                 iterations)
        (time-it nil)
        (fformat t "~&;;   Fastest ET (list) timing (~:d iterations)..."
                 iterations)
        (time-it (get-et 1 et-list))
        (fformat t "~&;;   Slowest ET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (get-et transition et-list))
        (fformat t "~&;;   Average ET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (get-et test-index et-list))
        (fformat t "~&;;   ET (transitioned) timing (~:d iterations)..."
                 iterations)
        (time-it (get-et test-index et-ht))
        (fformat t "~&;;   eq key/value hash table timing (~:d iterations)..."
                 iterations)
        (time-it (gethash test-index ht))
        (clear-et et-list)
        (fformat t "~&;;   Fastest add/delete ET (list) timing ~
                               (~:d iterations)..."
                 iterations)
        (time-it (progn (setf (get-et 1 et-list) 1)
                        (delete-et 1 et-list)))
        (fformat t "~&;;   eq key/value hash table add/delete timing ~
                             (~:d iterations)..."
                 iterations)
        (time-it (progn (setf (gethash 1 ht) 1)
                        (remhash 1 ht)))))))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (declare (fixnum numerator denominator))
  (let ((iterations (truncate& *timing-iterations* 2))
        fastest-time
        timed-fastest-fixnum-div-operator)
    (with-full-optimization ()
      (fformat t "~&;;   Division timing (~:d iterations)..."
               iterations)
      (fformat t "~&;; ~:[   ~;-->~] /&:        "
               (eq fastest-fixnum-div-operator '/&))
      (let ((time (brief-timer iterations
                               (/& numerator denominator))))
        (setf fastest-time time
              timed-fastest-fixnum-div-operator '/&)
        (format-ticks time))
      (fformat t "~%;; ~:[   ~;-->~] floor&:    "
               (eq fastest-fixnum-div-operator 'floor&))
      (let ((time (brief-timer iterations
                               (floor& numerator denominator))))
        (when (< time fastest-time)
          (setf fastest-time time
                timed-fastest-fixnum-div-operator 'floor&))
        (format-ticks time))
      (fformat t "~%;; ~:[   ~;-->~] truncate&: "
               (eq fastest-fixnum-div-operator 'truncate&))
      (let ((time (brief-timer iterations
                               (truncate& numerator denominator))))
        (when (< time fastest-time)
          (setf fastest-time time
                timed-fastest-fixnum-div-operator 'truncate&))
        (format-ticks time))
      (unless (eq timed-fastest-fixnum-div-operator
                  fastest-fixnum-div-operator)
        (format t "~&;;   The computed fastest fixnum division operator is ~s~
                   ~%;;   (the specified fastest operator is ~s) ***~%"
                timed-fastest-fixnum-div-operator
                fastest-fixnum-div-operator)))))
        

;;; ---------------------------------------------------------------------------

;;  CMUCL & SCL require these definitions at compile time:
(eval-when (#+(or cmu scl) :compile-toplevel :load-toplevel :execute)

  (define-class test-instance ()
    (common-slot
     (non-cloned-slot :initform 1)))
  
  (define-class test-instance-clone ()
    (common-slot 
     (new-slot :initform -1))))

;;; ---------------------------------------------------------------------------

(defun do-instance-timing (&optional (size *timing-instances*))
  (declare (fixnum size))
  (let ((list nil)
        (warning-time 0))
    (fformat t "~&;;   Make-instance timing (~:d instances)..." size)
    (let ((%start-time% (get-internal-run-time)))
      (with-full-optimization ()
        (dotimes (i size)
          (declare (fixnum i))
          (push (make-instance 'test-instance :common-slot i) list)))
      (let ((time (- (get-internal-run-time) %start-time%)))
        (setf warning-time (*& time 2))
        (format-ticks time)))
    (fformat t "~&;;   Change-class timing (~:d instances)..." size)
    (let* ((class (find-class 'test-instance-clone))
           (%start-time% (get-internal-run-time)))
      (with-full-optimization ()
        (dolist (instance list)
          (change-class instance class)))
      (let ((time (- (get-internal-run-time) %start-time%)))
        (format-ticks time)
        (when (>& time warning-time)
          (format t " *****"))))))
    
;;; ===========================================================================
;;;  Compute the size transition value for ESETs

(defun compute-eset-transition (&optional (start-size 1) (verbose? 't))
  (let ((iterations *transition-timing-iterations*)
        (max-size *max-list-test-size*)
        (transition 0)
        (contiguous-misses 0)
        (eset)
        (ht (make-keys-only-hash-table-if-supported :test #'eq)))
    (with-full-optimization ()
      ;; Initialize values
      (setf eset (cons max-size
                     (loop for i fixnum from 1 to max-size
                         collect i
                         do (setf (gethash i ht) 't))))
      ;; Time to time:
      (flet 
          ((time-eset (max-index)
             (indexed-timer iterations max-index (in-eset test-index eset)))
           (time-ht (max-index)
             (indexed-timer iterations max-index (gethash test-index ht))))
        (loop for i fixnum from start-size to max-size do
              (let ((time-eset (time-eset i))
                    (time-ht (time-ht i)))
                (declare (fixnum time-eset time-ht))
                (when verbose?
                  (cond 
                   ((eq verbose? ':dots)
                    (write-char #\.)
                    (force-output))
                   (t (format t "~&;; i: ~d eset: ~d ht: ~d | eset: "
                              i time-eset time-ht)
                      (format-ticks time-eset))))
                (cond ((<=& time-eset time-ht)
                       (setf contiguous-misses 0)
                       (setf transition i))
                      ((>=& (incf& contiguous-misses) 
                            *required-contiguous-misses*)
                       (return)))))
        (when (and verbose? (not (eq verbose? ':dots)))
          (format t "~&;; ESET transition: ~d~%" transition))
        transition))))
        
;;; ---------------------------------------------------------------------------
;;;  Compute the size transition value for ETs

(defun compute-et-transition (&optional (start-size 1) (verbose? 't))
  (let ((iterations *transition-timing-iterations*)
        (max-size *max-list-test-size*)
        (transition 0)
        (contiguous-misses 0)
        (et)
        (ht (make-hash-table :test #'eq)))
    (with-full-optimization ()
      ;; Initialize values
      (setf et (cons max-size
                     (loop for i fixnum from 1 to max-size
                         collect (cons i i)
                         do (setf (gethash i ht) i))))
      ;; Time to time:
      (flet 
          ((time-et (max-index)
             (indexed-timer iterations max-index (get-et test-index et)))
           (time-ht (max-index)
             (indexed-timer iterations max-index (gethash test-index ht))))
        (loop for i fixnum from start-size to max-size do
              (let ((time-ht (time-ht i))
                    (time-et (time-et i)))
                (declare (fixnum time-et time-ht))
                (when verbose?
                  (cond
                   ((eq verbose? ':dots)
                    (write-char #\.)
                    (force-output))
                   (t (format t "~&;; i: ~d et: ~d ht: ~d | et: "
                              i time-et time-ht)
                      (format-ticks time-et))))
                (cond ((<=& time-et time-ht)
                       (setf contiguous-misses 0)
                       (setf transition i))
                      ((>=& (incf& contiguous-misses) 3)
                       (return)))))
        (when (and verbose? (not (eq verbose? ':dots)))
          (format t "~&;; ET transition: ~d~%" transition))
        transition))))
        
;;; ---------------------------------------------------------------------------
;;;  Compute the size transition values where hash-tables outperform lists

#-slow-atable
(defun compute-atable-transitions (&optional (verbose? 't))
  ;; Optional `verbose?' value can be :very for detailed output...
  (let ((iterations *transition-timing-iterations*)
        (max-size *max-list-test-size*)
        (computed-sizes-vector
         (make-array '(10) :initial-element 0))
        (contiguous-misses 0)
        (very-verbose? (eq verbose? ':very)))
    (flet ((string-it (i)
             (make-string 2 :initial-element (code-char (+& i 32)))))
      (macrolet
          ((do-it (test keys-only keygen-fn)
             (let ((atable-index 
                    (if keys-only
                        (determine-keys-only-atable-index test)
                        (determine-key/value-atable-index test))))
               (flet ((build-at/ht-timer (name reader)
                        `(,name (max-index at/ht)
                                ;; Do one untimed trial to prepare everything:
                                (setf *%timing-result%* 
                                      (,reader (svref keys max-index) at/ht))
                                (let ((test-index 1)
                                      (%start-time% (get-internal-run-time)))
                                  (setf test-index 1)
                                  (dotimes (i iterations)
                                    (declare (fixnum i))
                                    (setf *%timing-result%*
                                          (,reader (svref keys test-index) at/ht))
                                    (incf& test-index)
                                    (when (>& test-index max-index)
                                      (setf test-index 1)))
                                  (locally (declare (notinline -))
                                    (- (get-internal-run-time) %start-time%))))))
                 `(let ((atl (make-atable :test ',test
                                          :keys-only ,keys-only))
                        (ath (make-atable :test ',test
                                          :keys-only ,keys-only
                                          :size (1+& max-size)))
                        (ht (make-hash-table :test ',test))
                        (keys (make-array (list (1+& max-size)))))
                    (with-full-optimization ()
                      (setf (%atable-data-list (atable-data atl))
                            ;; Fill tables (list in increasing key order):
                            (loop for i fixnum from 1 to max-size 
                                for key = (funcall ,keygen-fn i)
                                collect ,(if keys-only 'key '(cons key i))
                                do (setf (get-entry key ath) i)
                                   (setf (gethash key ht) i)
                                   (setf (svref keys i) key)))
                      (when verbose?
                        (format t "~&;; Timing ~s~@[ (keys only)~*~] "
                                ',test 
                                ,keys-only))
                      ;; Time to time:
                      (flet (,(build-at/ht-timer 'time-at 'get-entry)
                             ,(build-at/ht-timer 'time-ht 'gethash))
                        (setf contiguous-misses 0)
                        (loop for i fixnum from 1 to max-size do
                              (let ((time-atl (time-at i atl))
                                    (time-ath (when very-verbose?
                                                (time-at i ath)))
                                    (time-ht (time-ht i ht)))
                                (declare (fixnum time-atl time-ht))
                                (when (and verbose? (not very-verbose?))
                                  (write-char #\.)
                                  (force-output))
                                (when very-verbose?
                                  (format t "~&;; i: ~d, atl: ~d, ath: ~d, ht: ~d~%"
                                          i time-atl time-ath time-ht))
                                (cond 
                                 ((<=& time-atl time-ht)
                                  (setf contiguous-misses 0)
                                  (setf (svref computed-sizes-vector ,atable-index)
                                        i))
                                 ((>=& (incf& contiguous-misses) 
                                       *required-contiguous-misses*)
                                  (when verbose?
                                    (format t " transition: ~d~%"
                                            (svref computed-sizes-vector ,atable-index)))
                                  (return))))))))))))
        (do-it eq t #'identity)
        (do-it eq nil #'identity)
        (do-it eql t #'identity)
        (do-it eql nil #'identity)
        (do-it equal t #'string-it)
        (do-it equal nil #'string-it)
        (do-it equalp t #'string-it)
        (do-it equalp nil #'string-it)
        computed-sizes-vector))))
  
;;; ---------------------------------------------------------------------------
;;;  Check Clozure CL's combined-method-hash-table threshold value

#+clozure
(defun check-combined-method-hash-table-threshold ()
  (format t "~&;; Computing Clozure CL's combined-method-hash-threshold...~%")
  (let ((threshold
         (truncate$
          (*$ 0.75 
              (float 
               (let ((*standard-output* 
                      ;; Run silently (to the null stream):
                      (make-broadcast-stream)))
                 (ccl::compute-eql-combined-method-hash-table-threshold)))))))
    (format t "~&;; Computed ~s value: ~d (current value is ~d)~%"
            'ccl::*eql-combined-method-hash-table-threshold*
            threshold
            ccl::*eql-combined-method-hash-table-threshold*)
    (unless (<& (abs& (-& ccl::*eql-combined-method-hash-table-threshold*
                          threshold))
                2)
      (format t "~&;; ***** For top performance, set ~s to ~s"
              'ccl::*eql-combined-method-hash-table-threshold*
              threshold))))

;;; ---------------------------------------------------------------------------

(defun request-results ()
  (format t
          "~&;; ~60,,,'*<*~>~%~
             ;; **  Please help support GBBopen development by e-mailing  **~%~
             ;; **  this entire timing output log to support@GBBopen.org  **~%~
             ;; ~60,,,'*<*~>~%")
  (let ((*print-length* nil)
        (*print-right-margin* 70))
    (printv *features*))
  (values))

;;; ---------------------------------------------------------------------------

(defun check-transition-sizes (&optional (verbose? ':dots))
  (let ((request-results? nil))
    (flet 
        ((check-size (label compute-fn size)
           (fformat t "~&;; Checking ~a transition size" label)
           (let ((computed-transition
                  (funcall compute-fn
                           ;; Start timing slightly below the specified size:
                           (-& size (max& 3 (round (* size 0.2))))
                           verbose?)))
             ;; Does the specified transition size appear too high?  Recompute
             ;; it from size 1:
             (when (zerop& computed-transition)
               (fformat t "~&;; Recomputing the ~a transition size"
                        label)
               (setf computed-transition (funcall compute-fn 1 verbose?)))
             ;; Compare the computed size with the specified transition size:
             (let ((size-difference (-& computed-transition size)))
               (cond
                ((>& (abs& size-difference) (max& 4 (truncate (* size 0.3))))
                 (setf request-results? 't)
                 (format t "~&;; ***** The specified ~a transition size ~s is ~
                                 too ~:[low~;high~], the computed value is ~
                                 ~s~%"
                         label size 
                         (minusp& size-difference)
                         computed-transition))
                (t (format t "~&;; The computed ~a transition size is ~s ~
                                   (the specified size is ~s)~%"
                           label computed-transition size)))))))
      ;; Check ESET
      (check-size "ESET" #'compute-eset-transition eset-transition-size)
      ;; Check ET
      (check-size "ET" #'compute-et-transition et-transition-size))
    (when request-results? (request-results)))
  (values))

;;; ---------------------------------------------------------------------------

(defun cl-timing ()
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Characters are~@[ not~] eq.~%"
          (not (eval '(eq (code-char 70) (code-char 70)))))
  (format t "~&;; Fixnums are~@[ not~] eq.~%"
          (not (eval '(eq most-positive-fixnum most-positive-fixnum))))
  ;; Check fill-pointer removal:
  (let ((fill-pointer-string 
         (make-array '(2) :fill-pointer 0 :element-type 'base-char)))
    (when (array-has-fill-pointer-p 
           (coerce fill-pointer-string 'simple-base-string))
      (format t "~&;; **** Coerce to ~s does not eliminate fill pointer ****~%"
              'simple-base-string)))
  ;;; Check the threshold on Clozure CL:
  #+(and clozure ignore)
  (check-combined-method-hash-table-threshold)
  ;; Do the timing:
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Starting timings...~%")
  (do-list-timing)
  (format t "~&;;~%")
  (do-ht-timing)
  (format t "~&;;~%")
  (do-division-timing)
  (format t "~&;;~%")
  (do-computed-case-timing)
  (format t "~&;;~%")
  (do-eset-timing)
  (format t "~&;;~%")
  (do-et-timing)
  (format t "~&;;~%")
  (do-instance-timing)
  (format t "~&;; Timings completed.~%")
  ;; Check transition values:
  (format t "~&;; ~50,,,'-<-~>~%")
  (check-transition-sizes))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  