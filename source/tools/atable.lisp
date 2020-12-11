;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/atable.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jan 16 10:40:16 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *    Autotransitioning "Fast Hash" Tables (ATABLEs),  EQ Sets (ESETS), 
;;;; *                             and EQ Tables (ETs)
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
;;; Ideally, native CL hash tables should always be fast.  On some CLs,
;;; however, we can save a small amount of time (and some space) by using
;;; list-based representations for tables with small counts that
;;; autotransition to regular hash tables as the count grows (and back to the
;;; list representation if it shrinks).  Of course these autotransitioning
;;; representations also add some overhead to normal hash table operations, so
;;; their use should be considered very carefully (and restricted to where the
;;; counts are likely to be low in most situations)....  You have been warned!
;;;
;;; Porting Notice:
;;;
;;;   For top performance, transition sizes for ATABLEs, ESETs, and ETs need
;;;   to be determined for each CL implementation and platform
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-15-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(add-to-eset
            atable                      ; data type
            atable-count
            atable-test
            clear-atable
            clear-eset
            clear-et
            delete-et
            delete-from-eset
            delete-entry
            eset                        ; data type
            et                          ; data type
            get-entry
            get-et
            in-eset
            make-atable
            make-et
            make-eset
            map-atable
            map-eset
            map-et
            minimum-transition-size)))  ; not documented

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant minimum-transition-size 8)
  (defconstant auto-transition-margin 2))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-featured-value (variable &rest args)
    ;; Ensure that only one feature-selection matched:
    (unless args
      (error
       "No ~s value was defined for ~a~@[ running on ~a~]."
       variable
       (lisp-implementation-type) 
       (machine-type)))             
    (when (cdr args)
      (error
       "Multiple ~s values, ~s, were defined for ~a~@[ running on ~a~]."
       variable
       args
       (lisp-implementation-type) 
       (machine-type)))
    ;; Return the sole-element value:
    (first args)))

;;; ===========================================================================
;;;  EQ Sets (ESETs)
;;;
;;; ESETs are slower in CLISP and ECL; we also don't bother when sizes are less
;;; than MINIMUM-TRANSITION-SIZE:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant eset-transition-size 
      (check-featured-value
       'eset-transition-size
       ;; Armed Bear Common Lisp
       #+abcl 37
       ;; Allegro
       #+(and allegro macosx x86) 10
       #+(and allegro macosx powerpc) 34
       #+(and allegro                   ; also applies to windows
              (not (and macosx (or x86 powerpc)))) 108
       ;; CLISP
       #+clisp 0
       ;; Clozure
       #+(and clozure darwinx8632-target) 10
       #+(and clozure darwinx8664-target) 30
       #+(and clozure darwinppc-target) 18
       #+(and clozure (not (or darwinx8632-target
                               darwinx8664-target
                               darwinppc-target))) 15
       ;; CMUCL
       #+(and cmu darwin x86) 26
       #+(and cmu (not (and darwin x86))) 46
       ;; Digitool MCL
       #+digitool-mcl 18
       ;; ECL
       #+ecl 0
       ;; Lispworks
       #+lispworks 18
       ;; SBCL
       #+(and sbcl darwin (not ppc)) 48
       #+(and sbcl darwin ppc) 62
       #+(and sbcl (not darwin)) 32
       ;; SCL
       #+(and scl darwin x86) 26
       #+(and scl (not (and darwin x86))) 46
       ;; New port (values needed)
       #-(or abcl
             allegro 
             clisp
             clozure
             cmu
             digitool-mcl
             ecl
             lispworks
             sbcl
             scl)
       ;; Something reasonable, but need to determine for the port:
       (progn
         (warn "Need to compute ~s"
               'eset-transition-size)
         10))))

;;; ---------------------------------------------------------------------------
;;;  ESET "structure"

(defconstruct %eset (count data))

;;; ---------------------------------------------------------------------------

(defun make-eset (&key size)
  ;; Use the specified size if it is larger than the transition size:
  (if (and (fixnump size)
           (>& size (+& eset-transition-size auto-transition-margin)))
      (cons nil (make-keys-only-hash-table-if-supported :test #'eq :size size))
      (list 0)))

(defcm make-eset (&whole whole &key size)
  (if size
      ;; if size is specified, compile the normal make-eset call:
      whole
      ;; otherwise, just make an empty eset:
      `(list 0)))

;;; ---------------------------------------------------------------------------
;;;  ESET membership

(defun in-eset (item et)
  (with-full-optimization ()
    (let* ((count (%eset-count et))
           (data (%eset-data (the cons et))))
      (if count
          (if (memq item data)
              (values item 't)
              (values nil nil))
          (gethash item data)))))

;;; ---------------------------------------------------------------------------
;;;  ESET insertion

(defun add-to-eset (item et)
  (with-full-optimization ()
    (let* ((count (%eset-count et))
           (data (%eset-data (the cons et))))
      (if count
          (let ((sublist (memq item data)))
            (unless sublist
              (let ((transition-size                    
                     (+& eset-transition-size auto-transition-margin)))
                (cond 
                 ((<=& (incf& (%eset-count et)) transition-size)
                  (push item (%eset-data et)))
                 ;; Transition to hash-table:
                 (t (let ((ht (make-keys-only-hash-table-if-supported
                               :test 'eq
                               :size transition-size)))
                      ;; Copy existing items:
                      (dolist (item data)
                        (setf (gethash item ht) 't))
                      ;; Add new entry
                      (setf (%eset-count et) nil) ; mark as ht
                      (setf (%eset-data et) ht)
                      (setf (gethash item ht) 't)))))
              ;; Return item
              item))
          ;; Already a hash-table:
          (setf (gethash item data) 't)))))
  
;;; ---------------------------------------------------------------------------
;;;  ESET removal

(defun delete-from-eset (item et)
  (with-full-optimization ()
    (let* ((count (%eset-count et))
           (data (%eset-data (the cons et))))
      (if count
          ;; List deletion (inlined DELQ-ONE with add'l ET actions for top
          ;; performance):
          (cond
           ;; Empty table:
           ((zerop& count)
            ;; return failure:
            nil)
           ;; Deleting the first element:
           ((eq item (first data))
            (setf (%eset-data et) (rest data))
            (decf& (%eset-count et))
            ;; Return success:
            't)
           ;; Search for it:
           (t (let ((ptr data)
                    next-ptr)
                (declare (list ptr next-ptr))
                (loop
                  (unless (consp (setf next-ptr (cdr ptr)))
                    (return-from delete-from-eset nil))
                  (when (eq item (car (the cons next-ptr)))
                    (setf (cdr (the cons ptr)) (cdr next-ptr))
                    (decf& (%eset-count et))                    
                    (return-from delete-from-eset 't))
                  (setf ptr next-ptr)))))
          ;; Hash-table deletion:
          (let ((result (remhash item data)))
            (when result
              (let ((count (hash-table-count data)))
                (when (<& count
                          (-& eset-transition-size auto-transition-margin))
                  (setf (%eset-data et)
                        (loop for item being each hash-key in data
                            collect item))
                  ;; Mark as list representation:
                  (setf (%eset-count et) count)))
              ;; Return success:
              't))))))

;;; ---------------------------------------------------------------------------
;;;  ESET clear

(defun clear-eset (et &key retain-as-hash-table)
  (with-full-optimization ()
    (let ((count (%eset-count et)))
      (cond
       ((or count (not retain-as-hash-table))
        (setf (%eset-count et) 0)
        (setf (%eset-data et) nil))
       (t (clrhash (%eset-data et)))))))

;;; ---------------------------------------------------------------------------
;;;  ESET mapper

(defun map-eset (function et)
  #+cmu (declare (notinline coerce))
  (let ((fn (coerce function 'function)))
    (declare (function fn))
    (with-full-optimization ()
      (let* ((count (%eset-count et))
             (data (%eset-data (the cons et))))
        (if count
            (mapc fn data)
            (maphash fn data))))))

;;; ===========================================================================
;;;  EQ Tables (ETs)
;;;
;;; ETs are slower in ABCL, CLISP, and ECL; we also don't bother when sizes
;;; are less than MINIMUM-TRANSITION-SIZE:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant et-transition-size 
      (check-featured-value
       'et-transition-size
       ;; Armed Bear Common Lisp
       #+abcl 0
       ;; Allegro
       #+(and allegro macosx x86) 6
       #+(and allegro macosx powerpc) 30
       #+(and allegro                   ; also applies to windows
              (not (and macosx (or x86 powerpc)))) 60
       ;; CLISP
       #+clisp 0
       ;; Clozure
       #+(and clozure darwinx8632-target) 4
       #+(and clozure darwinx8664-target) 26
       #+(and clozure darwinppc-target) 14
       #+(and clozure (not (or darwinx8632-target
                               darwinx8664-target
                               darwinppc-target))) 9
       ;; CMUCL
       #+(and cmu darwin x86) 14
       #+(and cmu (not (and darwin x86))) 78
       ;; Digitool MCL
       #+digitool-mcl 14
       ;; ECL
       #+ecl 0
       ;; Lispworks
       #+lispworks 3
       ;; SBCL
       #+(and sbcl darwin (not ppc)) 27
       #+(and sbcl darwin ppc) 42
       #+(and sbcl (not darwin)) 20
       ;; SCL
       #+(and scl darwin x86) 14
       #+(and scl (not (and darwin x86))) 78
       ;; New port (values needed)
       #-(or abcl
             allegro 
             clisp
             clozure
             cmu
             digitool-mcl
             ecl
             lispworks
             sbcl
             scl)
       ;; Something reasonable, but need to determine for the port:
       (progn
         (warn "Need to compute ~s"
               'et-transition-size)
         10))))
  
;;; ---------------------------------------------------------------------------
;;;  ET "structure"

(defconstruct %et (count data))

;;; ---------------------------------------------------------------------------

(defun make-et (&key size)
  ;; Use the specified size if it is larger than the transition size:
  (if (and (fixnump size) 
           (>& size (+& et-transition-size auto-transition-margin)))
      (cons nil (make-hash-table :test #'eq :size size))
      (list 0)))

(defcm make-et (&whole whole &key size)
    (if size
      ;; if size is specified, compile the normal make-et call:
      whole
      ;; otherwise, just make an empty et:
      `(list 0)))

;;; ---------------------------------------------------------------------------
;;;  ET searcher

(defun get-et (key et &optional default)       
  (with-full-optimization ()
    (let* ((count (%et-count et))
           (data (%et-data (the cons et))))
      (if count
          (let ((acons (assq key data)))
            (if acons 
                (values (cdr (the cons acons)) 't)
                (values default nil)))
          (gethash key data default)))))

;;; ---------------------------------------------------------------------------
;;;  ET adder

(defun (setf get-et) (nv key et &optional default)
  (declare (ignore default))
  (with-full-optimization ()
    (let* ((count (%et-count et))
           (data (%et-data (the cons et))))
      (if count
          ;; Currently an alist:
          (let ((acons (assoc key data :test #'eq)))
            (if acons
                ;; Just update the value:
                (setf (cdr acons) nv)
                (let ((transition-size 
                       (+& et-transition-size auto-transition-margin)))
                  (cond
                   ;; Push new pair onto the alist:
                   ((<=& (incf& (%et-count et)) transition-size)
                    (push (cons key nv) (%et-data et))
                    ;; Return nv:
                    nv)
                   ;; Transition alist to hash-table:
                   (t (let ((ht (make-hash-table
                                 :test 'eq
                                 :size transition-size)))
                        ;; Copy existing pairs:
                        (dolist (acons data)
                          (setf (gethash (car acons) ht) (cdr acons)))
                        ;; Add new entry
                        (setf (%et-count et) nil) ; mark as ht
                        (setf (%et-data et) ht)
                        (setf (gethash key ht) nv)))))))
          ;; Already a hash-table:
          (setf (gethash key data) nv)))))
  
;;; ---------------------------------------------------------------------------
;;;  ET remover

(defun delete-et (key et)
  (with-full-optimization ()
    (let* ((count (%et-count et))
           (data (%et-data (the cons et))))
      (if count
          ;; List deletion (inlined DELQ-ONE with add'l ET actions for top
          ;; performance):
          (cond
           ;; Empty table:
           ((zerop& count)
            ;; return failure:
            nil)
           ;; Deleting the first element:
           ((eq key (caar data))
            (setf (%et-data et) (rest data))
            (decf& (%et-count et))
            ;; Return success:
            't)
           ;; Search for it:
           (t (let ((ptr data)
                    next-ptr)
                (declare (list ptr next-ptr))
                (loop
                  (unless (consp (setf next-ptr (cdr ptr)))
                    (return-from delete-et nil))
                  (when (eq key (caar next-ptr))
                    (setf (cdr ptr) (cdr next-ptr))
                    (decf& (%et-count et))                    
                    (return-from delete-et 't))
                  (setf ptr next-ptr)))))
          ;; Hash-table deletion:
          (let ((result (remhash key data)))
            (when result
              (let ((count (hash-table-count data)))
                (when (<& count
                          (-& et-transition-size auto-transition-margin))
                  (setf (%et-data et)
                        (loop for key being each hash-key in data
                            using (hash-value value) 
                            collect (cons key value)))
                  ;; Mark as list representation:
                  (setf (%et-count et) count)))
              ;; Return success:
              't))))))

;;; ---------------------------------------------------------------------------
;;;  ET clear

(defun clear-et (et &key retain-as-hash-table)
  (with-full-optimization ()
    (let ((count (%et-count et)))
      (cond
       ((or count (not retain-as-hash-table))
        (setf (%et-count et) 0)
        (setf (%et-data et) nil))
       (t (clrhash (%et-data et)))))))

;;; ---------------------------------------------------------------------------
;;;  ET mapper

(defun map-et (function et)
  #+cmu (declare (notinline coerce))
  (let ((fn (coerce function 'function)))
    (declare (function fn))
    (with-full-optimization ()
      (let* ((count (%et-count et))
             (data (%et-data (the cons et))))
        (if count
            (dolist (acons data)
              (funcall fn (car (the cons acons)) (cdr (the cons acons))))
            (maphash fn data))))))

;;; ===========================================================================
;;;  General ATABLEs (not a huge speed win on many CLs these days)
;;;
;;;  Transition values:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *atable-transition-sizes* 
      (check-featured-value
       '*atable-transition-sizes*
       ;; Armed Bear Common Lisp
       #+abcl #(0 0 2 0 0 0 0 0 0 0)
       ;; Allegro
       #+(and allegro macosx x86) #(0 0 12 7 6 4 11 11 11 11)
       #+(and allegro macosx powerpc) #(0 0 36 28 6 5 5 6 15 15)
       #+(and allegro                   ; also applies to windows
              (not (and macosx (or x86 powerpc)))) #(0 0 108 80 15 11 8 8 12 12)
       ;; CLISP
       #+clisp #(0 0 0 0 0 0 0 0 0 0)
       ;; Clozure
       #+(and clozure darwinx8632-target) #(0 0 5 4 6 4 5 5 6 6)
       #+(and clozure darwinx8664-target) #(0 0 26 24 12 5 5 5 6 6)
       #+(and clozure darwinppc-target) #(0 0 32 12 14 7 7 7 7 7)
       #+(and clozure (not (or darwinx8632-target
                               darwinx8664-target
                               darwinppc-target))) #(0 0 13 9 10 6 6 6 7 7)
       ;; CMUCL
       #+cmu #(0 0 20 7 4 9 2 2 3 3)
       ;; Digitool MCL
       #+digitool-mcl #(0 0 26 24 12 5 5 5 6 6)
       ;; ECL
       #+ecl #(0 0 0 0 0 0 0 0 0 0)
       ;; Lispworks
       #+lispworks #(0 0 2 2 0 0 2 2 4 4)
       ;; SBCL
       #+(and sbcl darwin (not ppc)) #(0 0 10 7 5 5 2 2 3 3)
       #+(and sbcl darwin ppc) #(0 0 56 30 6 6 2 2 2 2)
       #+(and sbcl (not darwin)) #(0 0 28 16 6 5 2 2 2 2)
       ;; SCL
       #+scl #(0 0 20 7 4 9 2 2 3 3)
       ;; New port (values needed)
       #-(or abcl
             allegro
             clisp
             clozure
             cmu
             digitool-mcl
             ecl
             lispworks 
             sbcl
             scl)
       ;; Something reasonable, but need to determine for the port:
       (progn
         (warn "Need to compute ~s"
               '*atable-transition-sizes*)
         #(0 0 10 10 4 4 4 4 4 4)))))

;;; ---------------------------------------------------------------------------
;;;  Test <--> index lookups
;;;    - keys-only tables are even indicies, key/value tables are odd indicies
;;;    - hash-tables are 0 & 1, lists are 2-9

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *atable-test-vector*
      #(nil nil eq eq eql eql equal equal equalp equalp)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-full-optimization ()
    (defun determine-keys-only-atable-index (test)
      (case test
        (eq 2)
        (eql 4)
        (equal 6)
        (equalp 8)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-full-optimization ()
    (defun determine-key/value-atable-index (test)
      (case test
        (eq 3)
        (eql 5)
        (equal 7)
        (equalp 9)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun determine-atable-test (index)
    (declare (fixnum index))
    ;; CMUCL (20a) can't handle the load-time-value:
    (svref #+cmu #.*atable-test-vector*
           #-cmu (load-time-value *atable-test-vector*) index)))

;;; ---------------------------------------------------------------------------
;;;  Atable-data "structure"

(defconstruct %atable-data (list count))

;;; ---------------------------------------------------------------------------
;;;  Atable "structure"

(defconstruct (atable :constructor %make-atable)
    (type-index data))

;;; ---------------------------------------------------------------------------

(defun atable-count (atable)
  (let* ((index (atable-type-index atable))
         (data (atable-data (the cons atable)))
         (list? (>& index 1)))
    (if list?
        (%atable-data-count data)
        (hash-table-count data))))

;;; ---------------------------------------------------------------------------

(defun atable-test (atable)
  (let* ((index (atable-type-index atable))
         (data (atable-data (the cons atable)))
         (list? (>& index 1)))
    (if list?
        (determine-atable-test index)
        (hash-table-test data))))

;;; ---------------------------------------------------------------------------
;;;  Atable constructor

(defun make-atable (&key (test 'eql) (size 0)
                         keys-only
                         rehash-size rehash-threshold)
  (with-full-optimization ()
    (unless (symbolp test)
      (let ((test-name (nth-value 2 (function-lambda-expression test))))
        (when (symbolp test-name)
          (setf test test-name))))
    (let* ((index 
            (if keys-only
                (determine-keys-only-atable-index test)
                (determine-key/value-atable-index test)))
           (transition-size
            (svref  
             ;; CMUCL (20a) can't handle the load-time-value:
             #+cmu #.*atable-transition-sizes*
             #-cmu (load-time-value *atable-transition-sizes*)
             index))
           (table-data
            (cond ((>& size transition-size)
                   (setf index (if keys-only 0 1))
                   (apply (if keys-only
                              #'make-keys-only-hash-table-if-supported
                              #'make-hash-table)
                          :test test 
                          :size size 
                          `(,@(when rehash-size
                                `(:rehash-size ,rehash-size))
                              ,@(when rehash-size 
                                  `(:rehash-threshold ,rehash-threshold)))))
                  (t (make-%atable-data nil 0)))))
      (%make-atable index table-data))))

;;; ---------------------------------------------------------------------------
;;;  Atable reader

(macrolet 
    ((make-reader ()
       (flet ((make-list-fn (test)
                `(let* ((data-list (%atable-data-list (the cons data)))
                        (sublist
                         ,(ecase test
                            (eq '(memq key data-list))
                            (eql #-(or cmu sbcl)
                                 '(member key data-list)
                                 ;; Tell CMUCL & SBCL we can't use float-eql
                                 #+(or cmu sbcl)
                                 '(locally (declare (notinline member))
                                   (member key data-list)))
                            ((equal equalp) 
                             `(member key data-list :test #',test)))))
                   (if sublist
                       (values (car (the cons sublist)) 't)
                       (values nil nil))))
              (make-assoc-fn (test)
                `(let* ((alist (%atable-data-list (the cons data)))
                        (acons 
                         ,(ecase test
                            (eq '(assq key alist))
                            (eql #-(or cmu sbcl)
                                 '(assoc key alist)
                                 ;; Tell CMUCL & SBCL we can't use float-eql
                                 #+(or cmu sbcl)
                                 '(locally (declare (notinline assoc))
                                   (assoc key alist)))
                            ((equal equalp)
                             `(assoc key alist :test #',test)))))
                   (if acons
                       (values (cdr (the cons acons)) 't)
                       (values default nil)))))
         `(defun get-entry (key atable &optional default)
            (with-full-optimization ()
              (let* ((index (atable-type-index atable))
                     (data (atable-data (the cons atable))))
                (declare (fixnum index))
                (case index
                  ((0 1) (gethash key data default))
                  (2 ,(make-list-fn 'eq))
                  (3 ,(make-assoc-fn 'eq))
                  (4 ,(make-list-fn 'eql))
                  (5 ,(make-assoc-fn 'eql))
                  (6 ,(make-list-fn 'equal))
                  (7 ,(make-assoc-fn 'equal))
                  (8 ,(make-list-fn 'equalp))
                  (9 ,(make-assoc-fn 'equalp)))))))))
  (make-reader))

;;; ---------------------------------------------------------------------------
;;;  Atable writer

(macrolet-debug
    ((make-writer ()
       (flet ((make-list-fn (index)
                (let ((test (determine-atable-test index))
                      (threshold
                       (+& (svref *atable-transition-sizes* index)
                           auto-transition-margin)))
                  `(let* ((list (%atable-data-list data))
                          (sublist 
                           ,(case test
                              (eq '(memq key list))
                              (eql #-(or cmu sbcl)
                                   '(member key list)
                                   ;; Tell CMUCL & SBCL we can't use float-eql
                                   #+(or cmu sbcl)
                                   '(locally (declare (notinline member))
                                     (member key list)))
                              ((equal equalp) 
                               `(member key list :test #',test)))))
                     (unless sublist
                       (cond 
                        ((<& (incf& (%atable-data-count data))
                             ,threshold)
                         (push key (%atable-data-list data)))
                        ;; Transition to hash-table:
                        (t (let ((ht (make-keys-only-hash-table-if-supported
                                      :test ',test
                                      :size ,threshold)))
                             (dolist (key (%atable-data-list data))
                               (setf (gethash key ht) key))
                             ;; Add new entry
                             (setf (gethash key ht) key)
                             (setf (atable-type-index atable) 0)
                             (setf (atable-data atable) ht)))))
                     key)))
              (make-assoc-fn (index)
                (let ((test (determine-atable-test index))
                      (threshold
                       (+& (svref *atable-transition-sizes* index)
                           auto-transition-margin)))
                  `(let* ((alist (%atable-data-list data))
                          (acons
                           ,(ecase test
                              (eq '(assq key alist))
                              (eql #-(or cmu sbcl)
                                   '(assoc key alist)
                                   ;; Tell CMUCL & SBCL we can't use float-eql
                                   #+(or cmu sbcl)
                                   '(locally (declare (notinline assoc))
                                     (assoc key alist)))
                              ((equal equalp)
                               `(assoc key alist :test #',test)))))
                     (cond
                      ;; Replace value:
                      (acons (setf (cdr acons) nv))
                      ;; Add assoc-based entry:
                      ((<& (incf& (%atable-data-count data))
                           ,threshold)
                       (setf (%atable-data-list data)
                             (acons key nv alist)))
                      ;; Transition to hash-table:
                      (t (let ((ht (make-hash-table :test ',test)))
                           (dolist (acons (%atable-data-list data))
                             (setf (gethash (car acons) ht) (cdr acons)))
                           ;; Add new entry
                           (setf (gethash key ht) nv)
                           (setf (atable-type-index atable) 1)
                           (setf (atable-data atable) ht))))
                     nv))))
         `(defun (setf get-entry) (nv key atable)
            (with-full-optimization ()
              (let* ((index (atable-type-index atable))
                     (data (atable-data (the cons atable))))
                (declare (fixnum index))
                (case index
                  ((0 1) (setf (gethash key data) nv))
                  (2 ,(make-list-fn 2))
                  (3 ,(make-assoc-fn 3))
                  (4 ,(make-list-fn 4))
                  (5 ,(make-assoc-fn 5))
                  (6 ,(make-list-fn 6))
                  (7 ,(make-assoc-fn 7))
                  (8 ,(make-list-fn 8))
                  (9 ,(make-assoc-fn 9)))))))))
  (make-writer))

;;; ---------------------------------------------------------------------------
;;;  Atable delete-entry

(macrolet
    ((make-deleter ()
       (flet ((make-list-fn (test)
                `(let* ((list (%atable-data-list data))
                        (delete-performed? nil))
                   (declare (type list list))
                   (flet ((test-fn (item)
                            ;; Tell CMUCL & SBCL we can't use float-eql
                            #+(or cmu sbcl)
                            ,@(when (eq test 'eql)
                                `((declare (notinline ,test))))
                            (when (,test key item)
                              (setf delete-performed? 't))))
                     (declare (dynamic-extent #'test-fn))
                     (setf (%atable-data-list data)
                           (delete-if #'test-fn list)))
                   (when delete-performed?
                     (decf& (%atable-data-count data))
                     't)))
              (make-assoc-fn (test)
                `(let* ((alist (%atable-data-list data))
                        (delete-performed? nil))
                   (declare (type list alist))
                   (flet ((test-fn (item)
                            ;; Tell CMUCL & SBCL we can't use float-eql
                            #+(or cmu sbcl)
                            ,@(when (eq test 'eql)
                                `((declare (notinline ,test))))
                            (when (,test key (car (the cons item)))
                              (setf delete-performed? 't))))
                     (declare (dynamic-extent #'test-fn))
                     (setf (%atable-data-list data)
                           (delete-if #'test-fn alist)))
                   (when delete-performed?
                     (decf& (%atable-data-count data))
                     't))))
         `(defun delete-entry (key atable)
            (with-full-optimization ()
              (let* ((index (atable-type-index atable))
                     (data (atable-data (the cons atable))))
                (declare (fixnum index))
                (case index
                  ;; keys-only hash table:
                  (0                    
                   (let ((result (remhash key data)))
                     (when result 
                       (let* ((test (hash-table-test data))
                              (new-index (determine-keys-only-atable-index test))
                              (threshold 
                               (svref 
                                ;; CMUCL (20a) can't handle the load-time-value:
                                #+cmu #.*atable-transition-sizes*
                                #-cmu 
                                (load-time-value *atable-transition-sizes*) 
                                new-index))
                              (count (hash-table-count data)))
                         (when (<& count threshold)
                           (setf (atable-type-index atable)
                                 new-index)
                           (setf (atable-data atable)
                                 (make-%atable-data
                                  (loop for key being each hash-key in data
                                      collect key)
                                  count))))
                       result)))
                  ;; key/value hash-table
                  (1                    
                   (let ((result (remhash key data)))
                     (when result 
                       (let* ((test (hash-table-test data))
                              (new-index (determine-key/value-atable-index test))
                              (threshold 
                               (svref
                                ;; CMUCL (20a) can't handle the load-time-value:
                                #+cmu 
                                #.*atable-transition-sizes*
                                #-cmu
                                (load-time-value *atable-transition-sizes*)
                                new-index))
                              (count (hash-table-count data)))
                         (when (<& count threshold)
                           (setf (atable-type-index atable) new-index)
                           (setf (atable-data atable)
                                 (make-%atable-data 
                                  (loop for key being each hash-key in data 
                                      using (hash-value value) 
                                      collect (cons key value))
                                  count))))
                       result)))
                  (2 ,(make-list-fn 'eq))
                  (3 ,(make-assoc-fn 'eq))
                  (4 ,(make-list-fn 'eql))
                  (5 ,(make-assoc-fn 'eql))
                  (6 ,(make-list-fn 'equal))
                  (7 ,(make-assoc-fn 'equal))
                  (8 ,(make-list-fn 'equalp))
                  (9 ,(make-assoc-fn 'equalp)))))))))
  (make-deleter))

;;; ---------------------------------------------------------------------------
;;;  Atable clear

(defun clear-atable (atable &key retain-as-hash-table)
  (with-full-optimization ()
    (let* ((index (atable-type-index atable))
           (data (atable-data (the cons atable))))
      (cond 
       ;; Hash-table implementation:
       ((<=& index 1)
        (if retain-as-hash-table
            (clrhash data)
            (let ((test (hash-table-test data)))
              (setf (atable-type-index atable)
                    (if (zerop& index)
                        (determine-keys-only-atable-index test)
                        (determine-key/value-atable-index test)))
              (setf (atable-data atable)
                    (make-%atable-data nil 0))
              ;; return nil:
              nil)))
       ;; List-based implementation:
       (t (setf (%atable-data-count data) 0)
          (setf (%atable-data-list data) nil))))))
  
;;; ---------------------------------------------------------------------------
;;;  Atable mapper

(defun map-atable (function atable)
  #+cmu (declare (notinline coerce))
  (let ((fn (coerce function 'function)))
    (declare (function fn))
    (with-full-optimization ()
      (let* ((index (atable-type-index atable))
             (data (atable-data (the cons atable))))
        (cond 
         ;; Hash-table implementation:
         ((<=& index 1)
          (maphash fn data))
         ;; Member (keys-only) implementations:
         ((evenp& index)
          (dolist (key (%atable-data-list data))
            (funcall fn key key)))
         ;; Assoc-based implementations:
         (t (dolist (acons (%atable-data-list data))
              (declare (type cons acons))
              (funcall fn (car acons) (cdr acons)))))))))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

