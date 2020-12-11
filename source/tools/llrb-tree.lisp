;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/llrb-tree.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:57:22 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                               LLRB Trees
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill & Eric O'Connor
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-26-08 File created.  (Corkill)
;;;  07-21-09 File released.  (O'Connor)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(llrb-compare
	    llrb-current-left-child     ; not yet documented
	    llrb-current-right-child    ; not yet documented
	    llrb-iterator
	    llrb-iterator-decrement
	    llrb-iterator-get
	    llrb-iterator-increment
	    llrb-iterator-p
	    llrb-iterator-value
            llrb-node                   ; structure type
            llrb-tree                   ; structure type
            llrb-tree-count
            llrb-tree-delete
	    llrb-tree-find
            llrb-tree-node              ; not yet documented
            llrb-tree-p
            llrb-tree-test
            llrb-tree-value
	    make-llrb-iterator          ; export needed?
            make-llrb-tree
            map-llrb-tree
	    map-llrb-tree-with-conditional-descent
	    override-setf-value
	    set-current-node-value
	    with-llrb-iterator)))

;;; ---------------------------------------------------------------------------

(defstruct (llrb-node 
            (:constructor make-llrb-node (key value red-p))
            (:copier))
  key
  value
  (red-p nil :type boolean)
  (left nil)
  (right nil))

(defmethod print-object ((object llrb-node) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream)
        (format stream "~s ~:[B~;R~] ~s ~s"
                (llrb-node-key object)
                (llrb-node-red-p object)
                (let ((left (llrb-node-left object)))
                  (and left (llrb-node-key left)))
                (let ((right (llrb-node-right object)))
                  (and right (llrb-node-key right)))))
      ;; Print-object must return object:
      object)))

;;; ---------------------------------------------------------------------------
;;;  LLRB tree object (contains the count, test, and rtb-nodes)
;;;
;;;  `test' is a two-argument comparison function that must return a fixnum 
;;;   where:
;;;         negative => a < b
;;;         zero     => a = b
;;;         positive => a > b

(defstruct (llrb-tree
            (:constructor %make-llrb-tree (test))
            (:copier nil))
  (count 0 :type integer)
  test
  (root nil :type (or llrb-node null)))
            
(defmethod print-object ((object llrb-tree) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream)
        (format stream "~@[~s ~]~s with ~s entr~:@p"
                (let ((test (llrb-tree-test object)))
                  (if (functionp test)
                      (nth-value 2 (function-lambda-expression test))
                      test))
                (type-of object)
                (llrb-tree-count object)))
      ;; Print-object must return object:
      object)))

;;; ---------------------------------------------------------------------------

(defvar *%llrb-delete-succeeded%* nil)  ; used to signal successful llrb-delete
(defvar *%llrb-insert-succeeded%* nil)  ; used to signal added llrb-node
(defvar *%llrb-current-node%* nil)      ; used to set the current llrb-node
                                        ; value during traversals
(defvar *%llrb-current-test%* nil)
(defvar *%llrb-set-override%* nil)
(defvar *%llrb-in-setf%* nil)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro llrb-node-is-red? (node)
    (with-once-only-bindings (node)
      `(and ,node (llrb-node-red-p ,node))))
  
  (defmacro llrb-node-left-left (node)
    `(let ((left (llrb-node-left ,node)))
       (when left (llrb-node-left left))))

  (defmacro llrb-node-right-left (node)
    `(let ((right (llrb-node-right ,node)))
       (when right (llrb-node-left right))))

  (defmacro llrb-node-left-right (node)
    `(let ((left (llrb-node-left ,node)))
       (when left (llrb-node-right left))))
  
  ) ;; end eval-when

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-left (node)
    (declare (type llrb-node node))
    (let ((x (llrb-node-right node)))
      (declare (type llrb-node x))
      (setf (llrb-node-right node) (llrb-node-left x))
      (setf (llrb-node-left x) node)
      (setf (llrb-node-red-p x) (llrb-node-red-p node))
      (setf (llrb-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-right (node)
    (let ((x (llrb-node-left node)))
      (declare (type llrb-node x))
      (setf (llrb-node-left node) (llrb-node-right x))
      (setf (llrb-node-right x) node)
      (setf (llrb-node-red-p x) (llrb-node-red-p node))
      (setf (llrb-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-color-flip (node)
    (declare (type llrb-node node))
    ;; Toggle node, left, & right colors:
    (let ((left (llrb-node-left node))
          (right (llrb-node-right node)))
      (declare (type (or llrb-node null) left right))
      (setf (llrb-node-red-p node) (not (llrb-node-red-p node)))
      (when left
        (setf (llrb-node-red-p left) (not (llrb-node-red-p left))))
      (when right
        (setf (llrb-node-red-p right) (not (llrb-node-red-p right)))))))
  
;;; ===========================================================================
;;;  Insertion

(defun make-llrb-root (key value)
  (setf *%llrb-insert-succeeded%* 't)
  (make-llrb-node key value nil))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-fixup (node)
    (declare (type llrb-node node))
    (when (and (llrb-node-is-red? (llrb-node-right node))
               (not (llrb-node-is-red? (llrb-node-left node))))
      (setf node (llrb-rotate-left node)))
    ;; Fix two reds in a row on the way up:
    (when (and (llrb-node-is-red? (llrb-node-left node))
               (llrb-node-is-red? (llrb-node-left (llrb-node-left node))))
      (setf node (llrb-rotate-right node)))
    ;; Split (eliminate) 4-nodes on the way up:
    (when (and (llrb-node-is-red? (llrb-node-left node))
               (llrb-node-is-red? (llrb-node-right node)))
      (llrb-color-flip node))
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-insert (key node value test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (cond
     ;; Empty LLRB tree (insert at the bottom):
     ((not node) 
      (setf *%llrb-insert-succeeded%* 't)
      (make-llrb-node key value 't))
     ;; Do an interior insert:
     (t (let ((result (funcall test key (llrb-node-key node))))
          ;; Standard BST insert:
          (cond 
           ((zerop& result) 
            (setf (llrb-node-value node) value))
           ((minusp& result)
            (setf (llrb-node-left node)
                  (llrb-insert key (llrb-node-left node) value test)))
           (t (setf (llrb-node-right node)
                    (llrb-insert key (llrb-node-right node) value test))))
          (setf node (llrb-fixup node)))
        ;; Return node
        node))))

;;; ===========================================================================
;;;  Deletion

(with-full-optimization ()
  (defun llrb-move-red-left (node)
    (declare (type llrb-node node))
    (llrb-color-flip node)
    (when (llrb-node-is-red? (llrb-node-right-left node))
      (setf (llrb-node-right node) (llrb-rotate-right (llrb-node-right node)))
      (setf node (llrb-rotate-left node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-move-red-right (node)
    (declare (type llrb-node node))
    (llrb-color-flip node)
    (when (llrb-node-is-red? (llrb-node-left-left node))
      (setf node (llrb-rotate-right node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-delete-min (node)
    (declare (type (or llrb-node null) node))
    (unless node (error "Have to deal with this & root!"))
    ;; Remove node on bottom level (node must be red by invariant):
    (unless (llrb-node-left node)
      (return-from llrb-delete-min nil))
    ;; Push red link down, if necessary:
    (when (and (not (llrb-node-is-red? (llrb-node-left node)))
               (not (llrb-node-is-red? (llrb-node-left-left node))))
      (setf node (llrb-move-red-left node)))
    ;; Move down one level:
    (setf (llrb-node-left node) (llrb-delete-min (llrb-node-left node)))
    ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
    (llrb-fixup node)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-delete (key node test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (when node
      (cond 
       ;; Left: 
       ((minusp& (funcall test key (llrb-node-key node)))
        ;; push red right, if necessary:
        (when (and (not (llrb-node-is-red? (llrb-node-left node)))
                   (not (llrb-node-is-red? (llrb-node-left-left node))))
          (setf node (llrb-move-red-left node)))
        ;; move down (left):
        (setf (llrb-node-left node) 
              (llrb-delete key (llrb-node-left node) test)))
       ;; Equal or Right:
       (t 
        ;; Rotate to push red right:
        (when (llrb-node-is-red? (llrb-node-left node))
          (setf node (llrb-rotate-right node)))
        ;; If Equal and at bottom, delete node (by returning nil):
        (when (and (zerop& (funcall test key (llrb-node-key node)))
                   (not (llrb-node-right node)))
          (setf *%llrb-delete-succeeded%* 't)
          (return-from llrb-delete nil))
        ;; Push red right, if necessary:
        (when (and (not (llrb-node-is-red? (llrb-node-right node)))
                   (not (llrb-node-is-red? (llrb-node-right-left node))))
          (setf node (llrb-move-red-right node)))
        (cond 
         ;; If equal and not at bottom, replace current node's values with
         ;; successor's values and delete successor:
         ((zerop& (funcall test key (llrb-node-key node)))
          (let* ((right (llrb-node-right node))
                 (successor (llrb-min-node right)))
            (setf *%llrb-delete-succeeded%* 't)
            (setf (llrb-node-key node) (llrb-node-key successor))
            (setf (llrb-node-value node) (llrb-node-value successor))
            (setf (llrb-node-right node) (llrb-delete-min right))))
         ;; Otherwise, move down (right):
         (t (setf (llrb-node-right node) 
                  (llrb-delete key (llrb-node-right node) test))))))
      ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
      (llrb-fixup node))))

;;; ===========================================================================
;;;  Traversal & retrieval

(with-full-optimization ()
  (defun llrb-get-node (key node test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (while node
      (let ((result (funcall test key (llrb-node-key node))))
        (cond
         ((zerop& result) (return node))
         ((minusp& result) (setf node (llrb-node-left node)))
         (t (setf node (llrb-node-right node))))))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-min-node (node &optional and-stack)
    (declare (type (or llrb-node null) node))
    (let ((result node))
      (if and-stack
	  (let (stack)
	    (while (setf node (llrb-node-left node))
	      (when result
		(push result stack))
	      (setf result node))
	    (values result stack))
	  (progn
	    (while (setf node (llrb-node-left node))
	      (setf result node))
	    result)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-max-node (node &optional and-stack)
    (declare (type (or llrb-node null) node))
    (let ((result node))
      (if and-stack
	  (let (stack)
	    (while (setf node (llrb-node-right node))
	      (when result
		(push result stack))
	      (setf result node))
	    (values result stack))
	  (progn
	    (while (setf node (llrb-node-right node))
	      (setf result node))
	    result)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-map (fn node)
    (declare (type (or llrb-node null) node)
             (type function fn))
    (when node
      (let ((*%llrb-current-node%* node))
        (llrb-map fn (llrb-node-left node))
        (funcall fn node)
        (llrb-map fn (llrb-node-right node))))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-map-with-conditional-descent (left right node)
    (declare (type (or llrb-node null) node)
	     (type function left right))
    (let ((*%llrb-current-node%* node))
      (when node
	(when (funcall left node)
	  (llrb-map-with-conditional-descent left 
					     right 
					     (llrb-node-left node)))
	(when (funcall right node)
	  (llrb-map-with-conditional-descent left 
					     right 
					     (llrb-node-right node)))))))

;;; ---------------------------------------------------------------------------

(defun llrb-current-left-child ()
  (if *%llrb-current-node%*
      (let ((left (llrb-node-left *%llrb-current-node%*)))
	(when left
	  (values (llrb-node-key left)
		  (llrb-node-value left))))
      (error "No current node is active.")))

(defun llrb-current-right-child ()
  (if *%llrb-current-node%*
      (let ((right (llrb-node-right *%llrb-current-node%*)))
	(when right
	  (values (llrb-node-key right)
		  (llrb-node-value right))))
      (error "No current node is active.")))

(defun set-current-node-value (value)
  (if *%llrb-current-node%*
      (setf (llrb-node-value *%llrb-current-node%*)
	    value)
      (error "No current node is active.")))

(defun override-setf-value ()
  (if *%llrb-in-setf%*
      (setf *%llrb-set-override%* t)
      (error "No setf to override.")))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-prefix-map (fn node)
    (declare (type (or llrb-node null) node)
             (type function fn))
    (when node
      (funcall fn node)
      (llrb-prefix-map fn (llrb-node-left node))
      (llrb-prefix-map fn (llrb-node-right node)))))

;;; ===========================================================================
;;;  Public interface

(defun make-llrb-tree (&optional (comparison-test #'compare))
  (%make-llrb-tree (coerce comparison-test 'function)))

;;; ---------------------------------------------------------------------------

(defun llrb-compare (key1 key2)
  (if *%llrb-current-test%*
      (funcall *%llrb-current-test%*
	       key1
	       key2)
      (error "llrb-compare must be called in the context of an llrb-map function.")))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-node (key llrb-tree)
  (let ((root-node (llrb-tree-root llrb-tree)))
    (llrb-get-node key root-node (llrb-tree-test llrb-tree))))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-value (key llrb-tree &optional default)
  (let ((tree-node (llrb-tree-node key llrb-tree)))
    (if tree-node
        (values (llrb-node-value tree-node) 't)
        default)))

;;; ---------------------------------------------------------------------------

(defun (setf llrb-tree-value) (value key llrb-tree &optional default)
  (declare (ignore default))
  (let ((root-node (llrb-tree-root llrb-tree))
        (*%llrb-insert-succeeded%* nil))
    (setf (llrb-tree-root llrb-tree)
          (if root-node
              (llrb-insert key root-node value (llrb-tree-test llrb-tree))
              (make-llrb-root key value)))
    (when *%llrb-insert-succeeded%* (incf (llrb-tree-count llrb-tree))))
  value)

;;; ---------------------------------------------------------------------------

(defun llrb-tree-delete (key llrb-tree)
  (let ((root-node (llrb-tree-root llrb-tree))
        (*%llrb-delete-succeeded%* nil))
    (setf (llrb-tree-root llrb-tree)
          (llrb-delete key root-node (llrb-tree-test llrb-tree)))
    (when *%llrb-delete-succeeded%*
      (decf (llrb-tree-count llrb-tree))
      ;; Return success
      't)))

;;; ---------------------------------------------------------------------------

(defun map-llrb-tree (fn llrb-tree)
  (let ((*%llrb-current-test%* (llrb-tree-test llrb-tree)))
    (flet ((do-fn (node) 
             (funcall fn (llrb-node-key node) (llrb-node-value node))))
      (declare (dynamic-extent #'do-fn))
      (llrb-map #'do-fn (llrb-tree-root llrb-tree)))))

;;; ---------------------------------------------------------------------------

(defun map-llrb-tree-with-conditional-descent (left right llrb-tree)
  (let ((*%llrb-current-test%* (llrb-tree-test llrb-tree)))
    (flet ((left-fn (node)
             (funcall left 
                      (and node
                           (llrb-node-key node))
                      (and node
                           (llrb-node-value node))))
           (right-fn (node)
             (funcall right 
                      (and node
                           (llrb-node-key node))
                      (and node
                           (llrb-node-value node)))))
      (declare (dynamic-extent #'left-fn #'right-fn))
      (llrb-map-with-conditional-descent
       #'left-fn #'right-fn (llrb-tree-root llrb-tree)))))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-find (search-key llrb-tree &optional (criterion '=) fn)
  ;;; Criterion options: '= '< '> '<= or '>=
  (let (best best-key best-value)
    (flet ((left-fn (key value)
             (declare (ignore value))
             (let ((compare (llrb-compare key search-key)))
               (ecase criterion
                 ((<= = >) (plusp& compare))
                 ((>= <) (not (minusp& compare))))))
           (right-fn (key value)
             (let ((compare (llrb-compare key search-key)))
               (flet ((set-best ()
                        (setf best *%llrb-current-node%*)
                        (setf best-key key)
                        (setf best-value value)))
                 (ecase criterion 
                   (= (when (zerop& compare)
                        (set-best))
                      (minusp& compare))
                   (< (when (and (minusp& compare)
                                 (or (not best-key)
                                     (plusp& (llrb-compare key best-key))))
                        (set-best))
                      (minusp& compare))
                   (<= (when (and (not (plusp& compare))
                                  (or (not best-key)
                                      (plusp& (llrb-compare key best-key))))
                         (set-best))
                       (not (plusp& compare)))
                   (>= (when (and (not (minusp& compare))
                                  (or (not best-key)
                                      (minusp& (llrb-compare key best-key))))
                         (set-best))
                       (minusp& compare))
                   (> (when (and (plusp& compare)
                                 (or (not best-key)
                                     (minusp& (llrb-compare key best-key))))
                        (set-best))
                      (not (plusp& compare))))))))
      (declare (dynamic-extent #'left-fn #'right-fn))
      (map-llrb-tree-with-conditional-descent #'left-fn #'right-fn llrb-tree))
    (when best-key
      (if (functionp fn)
	  (let ((*%llrb-current-node%* best))
	    (funcall fn best-key best-value))
	  (values best-key best-value)))))

;;; ---------------------------------------------------------------------------

(defun (setf llrb-tree-find) (set-value search-key llrb-tree &optional criterion fn)
  (let ((*%llrb-in-setf%* t)
	(*%llrb-set-override%* nil))
    (multiple-value-bind (returned-value set?)
        (flet ((fn (key value)
                 (when key
                   (when fn
                     (funcall fn key value))
                   (values (if (not *%llrb-set-override%*)
                               (set-current-node-value set-value)
                               value)
                           t))))
          (declare (dynamic-extent #'fn))
          (llrb-tree-find search-key llrb-tree criterion #'fn))
      (if (not set?)
	  (if (memq criterion '(= <= >=))
	      (setf (llrb-tree-value search-key llrb-tree) set-value)
	      (error (with-output-to-string (string) 
		       (format string "No node to set ~a ~a" criterion search-key))))
	  returned-value))))

;;; ===========================================================================
;;;  Iterator interface

(defstruct (llrb-iterator
	     (:constructor %make-llrb-iterator (node parent-stack tree))
	     (:copier))
  tree
  node
  parent-stack)

(defmethod print-object ((object llrb-iterator) stream)
  (cond
   (*print-readably* (call-next-method))
   (t 
    (let ((node (llrb-iterator-node object)))
      (print-unreadable-object (object stream)
        (format stream "iterator @~a on level ~a"
		node
		(length (llrb-iterator-parent-stack object))))
      object))))

;;; ---------------------------------------------------------------------------

(defun make-llrb-iterator (llrb-tree)
  (%make-llrb-iterator (llrb-tree-root llrb-tree)
		       nil
		       llrb-tree))

;;; ---------------------------------------------------------------------------

(defun llrb-iterator-helper (iterator parent parent-stack child child-stack increment)
  (let ((node (llrb-iterator-node iterator)))
    (setf (llrb-iterator-node iterator)
	  (cond
           ((and parent (not child))
            (setf (llrb-iterator-parent-stack iterator) parent-stack)
            parent)
           ((not parent)
            (when node
              (setf (llrb-iterator-parent-stack iterator)
                    (nconc child-stack 
                           (llrb-iterator-parent-stack iterator))))
            child)
           (t
            (cond
             ((and (and parent child)
                   (funcall (if increment
                                #'minusp&
                                #'plusp&)
                            (funcall (llrb-tree-test
                                      (llrb-iterator-tree iterator))
                                     (llrb-node-key parent) 
                                     (llrb-node-key child))))
              (setf (llrb-iterator-parent-stack iterator) parent-stack)
              parent)
             (t
              (setf (llrb-iterator-parent-stack iterator)
                    (nconc child-stack
                           (llrb-iterator-parent-stack iterator)))
              child)))))
    iterator))

;;; ---------------------------------------------------------------------------

(defun llrb-iterator-increment (iterator)
  (let ((node (llrb-iterator-node iterator)))
    (case node 
      (max (return-from llrb-iterator-increment iterator))
      (min
       (setf (llrb-iterator-node iterator) (pop (llrb-iterator-parent-stack iterator)))
       iterator)
      (otherwise
       (let* ((parent-stack (llrb-iterator-parent-stack iterator))
              (child-stack (list node))
              (parent (loop with i = nil
                          do (setf i (car parent-stack))
                             (setf parent-stack (cdr parent-stack))
                          when (or (not i)
                                   (minusp 
                                    (funcall (llrb-tree-test
                                              (llrb-iterator-tree iterator))
                                             (llrb-node-key node)
                                             (llrb-node-key i))))
                          do (return i)))
              (child (and node
                          (llrb-node-right node)
                          (multiple-value-bind (child-node new-stack)
                              (llrb-min-node (llrb-node-right node) t)
                            (setf child-stack (nconc new-stack child-stack))
                            child-node))))
         (if (or child parent)
             (llrb-iterator-helper iterator parent parent-stack child child-stack t)
             (progn
               (setf (llrb-iterator-node iterator) 'max)
               (push node (llrb-iterator-parent-stack iterator))
               iterator)))))))

;;; ---------------------------------------------------------------------------

(defun llrb-iterator-decrement (iterator)
  (let ((node (llrb-iterator-node iterator)))
    (case node 
      (min (return-from llrb-iterator-decrement iterator))
      (max
       (setf (llrb-iterator-node iterator) (pop (llrb-iterator-parent-stack iterator)))
       iterator)
      (otherwise
       (let* ((parent-stack (llrb-iterator-parent-stack iterator))
              (child-stack (list node))
              (parent (loop with i = nil
                          do (setf i (car parent-stack))
                             (setf parent-stack (cdr parent-stack))
                          when (or (not i)
                                   (plusp& 
                                    (funcall (llrb-tree-test
                                              (llrb-iterator-tree iterator))
                                             (llrb-node-key node)
                                             (llrb-node-key i))))
                          do (return i)))
              (child (and node
                          (llrb-node-left node)
                          (multiple-value-bind (child-node new-stack)
                              (llrb-max-node (llrb-node-left node) t)
                            (setf child-stack (nconc new-stack child-stack))
                            child-node))))
         (if (or child parent)
             (llrb-iterator-helper iterator parent parent-stack child child-stack nil)
             (progn
               (setf (llrb-iterator-node iterator) 'min)
               (push node (llrb-iterator-parent-stack iterator))
               iterator)))))))

;;; ---------------------------------------------------------------------------

(defun llrb-iterator-value (iterator)
  (let ((node (llrb-iterator-node iterator)))
    (when (llrb-node-p node)
      (values (llrb-node-value node)
	      (and node t)))))

;;; ---------------------------------------------------------------------------

(defun llrb-iterator-get (iterator)
  (let ((node (llrb-iterator-node iterator)))
    (when (llrb-node-p node)
      (values (llrb-node-key node)
	      (llrb-node-value node)))))

;;; ---------------------------------------------------------------------------

(defmacro with-llrb-iterator ((llrb-tree &optional 
					 (decrement 'decrement)
					 (increment 'increment)
					 (value 'value))
					 &body body)
  (with-gensyms (iterator)
    `(let* ((,iterator (make-llrb-iterator ,llrb-tree))
	    (*%llrb-current-node%* (llrb-iterator-node ,iterator)))
       (flet ((,decrement ()
		(llrb-iterator-decrement ,iterator)
		(setf *%llrb-current-node%* (llrb-iterator-node ,iterator))
		(llrb-iterator-get ,iterator))
	      (,increment ()
		(llrb-iterator-increment ,iterator)
		(setf *%llrb-current-node%* (llrb-iterator-node ,iterator))
		(llrb-iterator-get ,iterator))
	      (,value ()
		(llrb-iterator-value ,iterator)))
	 ,@body))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

