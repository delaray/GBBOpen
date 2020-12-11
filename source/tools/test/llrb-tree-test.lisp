;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/llrb-tree-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Sep 16 15:50:35 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                              LLRB Tree Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-15-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::llrb-node-is-red?
            gbbopen-tools::llrb-node-key
            gbbopen-tools::llrb-node-left
            gbbopen-tools::llrb-node-right
            gbbopen-tools::llrb-prefix-map
            gbbopen-tools::llrb-tree-root)))

;;; ---------------------------------------------------------------------------

(defun llrb-check-traversal (llrb-tree)
  (let ((last-key -infinity&))
    (flet ((fn (key value)
             (declare (ignore value))
             (unless (>& key last-key)
               (error "Wrong key ordering"))
             (setf last-key key)))
      (declare (dynamic-extent #'fn))
      (map-llrb-tree #'fn llrb-tree))))

;;; ---------------------------------------------------------------------------

(defun llrb-print-tree (node &optional (indent 0))
  (when node
    (let ((left (llrb-node-right node))
	  (right (llrb-node-left node)))
      (when left
	(llrb-print-tree left (1+& indent)))
      (loop for i from 1 to indent
          do (format t "  "))
      (format t "~a ~:[B~;R~]~%" 
              (llrb-node-key node)
              (llrb-node-is-red? node))
      (when right
	(llrb-print-tree right (1+& indent))))))

;;; ---------------------------------------------------------------------------

(defun llrb-print (llrb-tree)
  (let ((root (llrb-tree-root llrb-tree)))
    (if root
        (flet ((fn (node)
                 (format t "~&~s~%" node)))
          (declare (dynamic-extent #'fn))
          (llrb-prefix-map #'fn root))
        (format t "#<empty>~%")))
  (values))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-count-nodes (llrb-tree)
  (let ((count 0))
    (flet ((count-it (key value)
             (declare (ignore key value))
             (incf& count)))
      (declare (dynamic-extent #'count-it))
      (map-llrb-tree #'count-it llrb-tree))
    count))

;;; ===========================================================================
;;;   Timing

(defun random-size-llrb-tree-test (n)
  (format t "~&;;   Starting random-size LLRB-tree tests...~%")
  ;; Build the test-data hash table:
  (let ((numbers (make-hash-table :size n))
        tree
        ht
        continuep 
        key)
    (declare (ignorable continuep))     ; for CL's that optimize this var away!
    (dotimes (i n)
      (let ((key (random n)))
        (setf (gethash key numbers) key)))
    ;; Build the LLRH tree:
    (format t "~&;;     Building ~s-node LLRH tree:~%" 
            (hash-table-count numbers))
    (nicer-time
     (with-hash-table-iterator (next numbers)
       (setf tree (make-llrb-tree #'compare&))
       (while (multiple-value-setq (continuep key) (next))
         (setf (llrb-tree-value key tree) (-& key)))))
    (unless (= (hash-table-count numbers)
               (llrb-tree-count-nodes tree))
      (error "Wrong node count"))
    (llrb-check-traversal tree)
    ;; Now build the hash table:
    (format t "~&;;     Building ~s-entry hash table:~%" 
            (hash-table-count numbers))
    (nicer-time
     (with-hash-table-iterator (next numbers)
       (setf ht (make-hash-table :size (hash-table-count numbers)))
       (while (multiple-value-setq (continuep key) (next))
         (setf (gethash key ht) (-& key)))))
    ;; LLRH retrievals:
    (format t "~&;;     Performing ~s LLRB-tree retrievals:~%"
            (hash-table-count numbers))
    (nicer-time
     (with-hash-table-iterator (next numbers)
       (while (multiple-value-setq (continuep key) (next))
         (let ((value (llrb-tree-value key tree)))
           (unless (and value (=& value (-& key)))
             (error "LLRB did not retrieve ~s" key))))))
    ;; Hash-table retrievals:
    (format t "~&;;     Performing ~s hash-table retrievals:~%"
            (hash-table-count numbers))
    (nicer-time
     (with-hash-table-iterator (next numbers)
       (while (multiple-value-setq (continuep key) (next))
         (let ((value (gethash key ht)))
           (unless (and value (=& value (-& key)))
             (error "Hash-table did not retrieve ~s" key))))))
    ;; Now delete about 1/2 of the numbers in the test data:
    (with-hash-table-iterator (next numbers)
       (while (multiple-value-setq (continuep key) (next))
         (when (plusp& (random 2)) (remhash key numbers))))
    (let ((remaining-numbers (-& (hash-table-count ht)
                                 (hash-table-count numbers))))
      ;; LLRH deletes:
      (format t "~&;;     Performing ~s LLRB-tree deletes:~%"
              (hash-table-count numbers))
      (nicer-time
       (with-hash-table-iterator (next numbers)
         (while (multiple-value-setq (continuep key) (next))
           (llrb-tree-delete key tree))))
      ;; Hash-table deletes:
    (format t "~&;;     Performing ~s hash-table deletes:~%"
            (hash-table-count numbers))
      (nicer-time
       (with-hash-table-iterator (next numbers)
         (while (multiple-value-setq (continuep key) (next))
           (remhash key ht))))
      (let ((count (llrb-tree-count tree))
            (node-count (llrb-tree-count-nodes tree))
            (hash-table-count (hash-table-count ht)))
        (unless (= hash-table-count count node-count)
          (error "Wrong LLRB-TREE-COUNT after deletes: ~s ~s ~s ~s" 
                 count node-count hash-table-count remaining-numbers)))))
  (format t "~&;;   Random-size LLRB-tree tests completed.~%"))

;;; ===========================================================================
;;;  Basic LLRB-tree trip test

(defun basic-llrb-tree-test (&optional verbose)
  (format t "~&;;   Starting basic LLRB-tree tests...~%")
  (let (llrb-tree)
    (macrolet
        ((logger ((s &body setup) &body body)
           `(progn (when verbose (format t "~&;; ~40,,,'-<-~>~%;; ~a~%" ,s))
                   ,@setup
                   (when verbose (llrb-print llrb-tree))
                   ,@body
                   (when verbose (llrb-print llrb-tree)))))
      (flet ((nil-result-error ()
               (error "Result was nil"))
             (non-nil-result-error ()
               (error "Result was not nil")))
        (logger ("Delete missing from empty tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&)))
                (when (llrb-tree-delete 5 llrb-tree)
                  (non-nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete root:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5))
                (unless (llrb-tree-delete 5 llrb-tree)
                  (nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete missing (left) from singleton tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5))
                (when (llrb-tree-delete 1 llrb-tree)
                  (non-nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete missing (right) from singleton tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5))
                (when (llrb-tree-delete 9 llrb-tree)
                  (non-nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete 1 from 5-1 tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5)
                 (setf (llrb-tree-value 1 llrb-tree) -1))
                (unless (llrb-tree-delete 1 llrb-tree)
                  (nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete 9 from 1-5-9 tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5)
                 (setf (llrb-tree-value 1 llrb-tree) -1)
                 (setf (llrb-tree-value 9 llrb-tree) -1))
                (unless (llrb-tree-delete 9 llrb-tree)
                  (nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Make 1:8 tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (loop for i from 1 to 8 do
                       (setf (llrb-tree-value i llrb-tree) (-& i)))))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Delete 5 from 5-1 tree:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (setf (llrb-tree-value 5 llrb-tree) -5)
                 (setf (llrb-tree-value 1 llrb-tree) nil))
                (unless (llrb-tree-delete 5 llrb-tree)
                  (nil-result-error)))
        (when verbose (llrb-print-tree llrb-tree))
        (logger ("Create, retrieve, and delete with character values:"
                 (setf llrb-tree (make-llrb-tree #'compare&))
                 (loop for i from 65 to 90 do
                       (setf (llrb-tree-value i llrb-tree) (code-char i))))
                (loop for i from 65 to 90 do
                      (let ((value (llrb-tree-value i llrb-tree)))
                        (unless (and value (eql value (code-char i)))
                          (error "Incorrect character value ~s for ~s."
                                 value i))))))))
    (format t "~&;;   Basic LLRB-tree tests completed.~%"))
  
;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (basic-llrb-tree-test))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

