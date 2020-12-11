;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/pseudo-probabilities.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Nov  4 06:19:22 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Pseudo-Probability Entities
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
;;;  06-29-08 File created.  (Corkill)
;;;  11-03-12 Added REM%.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Generated fully-fixnum-equivalent operators are automatically exported by
  ;; DEFDN%
  (export '(%
            *%                          ; normalizing *&
            /%                          ; normalizing /&
            ceiling%                    ; normalizing ceiling&
            decf%                       ; the same as decf&
            decf%-after                 ; the same as decf&-after
            decf%/delete-acons          ; the same as decf&/delete-acons
            exp%                        ; normalizing exp
            floor%                      ; normalizing floor&
            fceiling%                   ; normalizing fceiling&
            ffloor%                     ; normalizing ffloor&
            fround%                     ; normalizing fround&
            ftruncate%                  ; normalizing ftruncate&
            incf%                       ; the same as incf&
            incf%-after                 ; the same as incf&-after
            ln%                         ; normalizing (log n)
            pprob2prob
            prob2pprob
            pseudo-probability
            pseudo-probability-p
            pushnew/incf%-acons         ; the same as pushnew/incf&-acons
            round%                      ; normalizing round&
            truncate%                   ; normalizing truncate&
            )))

;;; ===========================================================================
;;;   Pseudo-Probabilities
;;;
;;;  A "pseudo probability" represents a [0.0..1.0] value as a fixnum between
;;;  0 and MAX-PPROB (normally [0..1000]).  This provides fast, no allocation,
;;;  computations with discretized probability values.
;;;
;;;  CLISP provides the fewest fixnum bits (24).  In order to keep the value
;;;  of (ln% 1) a fixnum we use 1000 as MAX-PPROB.  (1513 is the largest
;;;  MAX-PPROB value where (ln% 1) can be represented as a 24-bit fixnum, but
;;;  we want to keep MAX-PPROB a power of ten to allow easy visual
;;;  transposition of the decimal point for human translation between real and
;;;  pseudo-probability values.  7743 is the largest MAX-PPROB value where (ln% 1)
;;;  is a 29-bit fixnum.)  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-pprob 1000)
  (defconstant max-pprob$ (float max-pprob))
  (deftype pseudo-probability () `(integer 0 ,max-pprob)))

(defun pseudo-probability-p (obj)
  (typep obj 'pseudo-probability))

(defcm pseudo-probability-p (obj)
  `(typep ,obj 'pseudo-probability))

;;; ---------------------------------------------------------------------------
;;;  Generated fully-fixnum-equivalent pseudo-probability operators

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defdn% (op &optional result values-types)
    ;; We keep the same signature as declared-numeric.lisp's DEFDN, but ignore
    ;; the optional arguments here:
    (declare (ignore result values-types))
    (let ((pprob-op (intern (concatenate 'simple-string
                              (symbol-name op) "%")
                            ':gbbopen-tools))
          (fixnum-op (intern (concatenate 'simple-string
                               (symbol-name op) "&")
                             ':gbbopen-tools)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (export '(,pprob-op)))
         
         (defun ,pprob-op (&rest args)
           (declare (dynamic-extent args))
           (apply #',fixnum-op args))
         
         (defcm ,pprob-op (&rest args)
           `(,',fixnum-op ,@args))))))

(defdn% + t)
(defdn% 1+  t)
(defdn% - t)
(defdn% 1- t)
(defdn% =)
(defdn% /=)
(defdn% <)
(defdn% <=)
(defdn% >)
(defdn% >=)
(defdn% min t)
(defdn% max t)
(defdn% zerop)
(defdn% plusp)
(defdn% minusp)
(defdn% evenp)
(defdn% oddp)
(defdn% abs t)
(defdn% mod t)
(defdn% rem t)
(defdn% bounded-value t)

;;; ---------------------------------------------------------------------------

(defmacro % (arg)
  ;;; Wraps (the pseudo-probability ...) around `arg'
  (if  (feature-present-p ':full-safety)
      `,arg
      `(the pseudo-probability ,arg)))

(define-modify-macro incf% (&optional (increment 1)) +&)
(define-modify-macro decf% (&optional (increment 1)) -&)

(defmacro incf%-after (place &optional (increment 1))
  ;;; Returns the current value of `place' (before the incf is done)
  (with-gensyms (old-value)
    `(let ((,old-value ,place))
       (setf ,place (+& ,old-value ,increment))
       ,old-value)))

(defmacro decf%-after (place &optional (increment 1))
  ;;; Returns the current value of `place' (before the decf is done)
  (with-gensyms (old-value)
    `(let ((,old-value ,place))
       (setf ,place (-& ,old-value ,increment))
       ,old-value)))
  
;;; ---------------------------------------------------------------------------

(defun prob2pprob (prob)
  (round$ (*$ (float prob) max-pprob$)))

(defcm prob2pprob (prob)
  `(round$ (*$ (float ,prob) ,max-pprob$)))

;;; ---------------------------------------------------------------------------

(defun pprob2prob (pprob)
  (/$ (float pprob) max-pprob$))

(defcm pprob2prob (pprob)
  `(/$ (float ,pprob) ,max-pprob$))

;;; ---------------------------------------------------------------------------

(defun *%-2op (pprob1 pprob2)
  (truncate& (*& pprob1 pprob2) max-pprob))

(defcm *%-2op (pprob1 pprob2)
  `(truncate& (*& ,pprob1 ,pprob2) ,max-pprob))

(defun *% (&rest args)
  (let ((result max-pprob))
    (while args
      (setf result (*%-2op result (pop args))))
    result))

(defcm *% (&rest args)
  (if args
      (let ((first-arg (pop args)))
        (if args
            ;; two or more args:
            `(*%-2op ,first-arg 
                     ,(labels 
                          ((another (args)
                             (if (rest args)
                                 `(*%-2op ,(first args)
                                          ,(another (rest args)))
                                 (first args))))
                        (another args)))
            ;; one arg:
            first-arg))
      ;; no args:
      max-pprob))

;;; ---------------------------------------------------------------------------

(defun /%-2op (pprob1 pprob2)
  (truncate& (*& pprob1 max-pprob) pprob2))

(defcm /%-2op (pprob1 pprob2)
  `(truncate& (*& ,pprob1 ,max-pprob) ,pprob2))

(defun /% (pprob1 &rest args)
  (if args
      ;; not unary:
      (let ((result pprob1))
        (while args
          (setf result (/%-2op result (pop args))))
        result)
      ;; Unary /% doesn't really make sense, as only (/ MAX-PPROB) returns a
      ;; legal pseudo-probabilty value, but we'll give the user some rope to
      ;; hang themselves with:
      (/%-2op max-pprob pprob1)))

(defcm /% (first-arg &rest args)
  (if args
      ;; two or more args:
      `(/%-2op ,first-arg 
               ,(labels 
                    ((another (args)
                       (if (rest args)
                           `(/%-2op ,(first args)
                                    ,(another (rest args)))
                           (first args))))
                  (another args)))
      ;; one arg:
      `(/%-2op max-pprob ,first-arg)))

;;; ---------------------------------------------------------------------------

(defvar *ln%-vector* 
    ;; The ln% lookup table
    (let ((vector (make-array (list (1+& max-pprob)))))
      (loop for i fixnum from 1 to max-pprob
          do (setf (svref vector i) 
                   (prob2pprob
                    (*$ max-pprob$ (log (pprob2prob i))))))
      vector))

;;; ---------------------------------------------------------------------------

(defun ln%-error (pprob)
  (error "Unable to take the ~s of ~s"
         'ln%         
         (/$ (float pprob) #.(float max-pprob))))

(defun ln% (pprob)
  ;;; Return the pprob-ln fixnum of pprob:
  (unless (<& 0 pprob #.(1+ max-pprob))
    (ln%-error pprob))
  (svref (the (simple-array t (*)) *ln%-vector*) pprob))

(defcm ln% (pprob)  
  (with-once-only-bindings (pprob)
    `(with-full-optimization ()
         (unless (<& 0 ,pprob #.(1+ max-pprob))
           (ln%-error ,pprob))
         (svref (the (simple-array t (*)) (locally (declare (special *ln%-vector*))
                                            *ln%-vector*))
                ,pprob))))

;;; ---------------------------------------------------------------------------

(defun exp% (pprob-ln)
  ;;; Return e**pprob-ln where pprob-ln is a fixnum between (ln% 1) and 0
  (with-full-optimization ()
  (unless (<=& (load-time-value (ln% 1)) pprob-ln 0)
    (error "Unable to represent the ~s of ~s as a pseudo-probability"
           'exp%
           pprob-ln))
  ;; Perform reverse lookup in *ln%-vector* using binary search:
  (let ((min 1)
        (max #.(1+ max-pprob))
        i
        result)
    (loop 
      (setf i (+& min (truncate& (-& max min) 2)))
      (setf result (svref (the (simple-array t (*)) *ln%-vector*) i))
      (cond 
       ;; We found the index, return it as its the e**pprob-ln value:
       ((or (>=& i max) 
            (<=& i min))
        (return i))
       ;; Search in lower half:
       ((<& pprob-ln result)
        (setf max i))
       ;; Search in upper half:
       ((>=& pprob-ln result)
        (setf min i)))))))

;;; ---------------------------------------------------------------------------

(defun ceiling% (pprob &optional (pprob-divisor 1000))
  (multiple-value-bind (quotient remainder)
      (ceiling& pprob pprob-divisor)
    (values (*& quotient max-pprob) remainder)))

(defcm ceiling% (pprob &optional (pprob-divisor 1000))
  (with-gensyms (quotient remainder)
    `(multiple-value-bind (,quotient ,remainder)
         (ceiling& ,pprob ,pprob-divisor)
       (values (*& ,quotient ,max-pprob) ,remainder))))

;;; ---------------------------------------------------------------------------

(defun floor% (pprob &optional (pprob-divisor 1000))
  (multiple-value-bind (quotient remainder)
      (floor& pprob pprob-divisor)
    (values (*& quotient max-pprob) remainder)))

(defcm floor% (pprob &optional (pprob-divisor 1000))
  (with-gensyms (quotient remainder)
    `(multiple-value-bind (,quotient ,remainder)
         (floor& ,pprob ,pprob-divisor)
       (values (*& ,quotient ,max-pprob) ,remainder))))

;;; ---------------------------------------------------------------------------

(defun round% (pprob &optional (pprob-divisor 1000))
  (multiple-value-bind (quotient remainder)
      (round& pprob pprob-divisor)
    (values (*& quotient max-pprob) remainder)))

(defcm round% (pprob &optional (pprob-divisor 1000))
  (with-gensyms (quotient remainder)
    `(multiple-value-bind (,quotient ,remainder)
         (round& ,pprob ,pprob-divisor)
       (values (*& ,quotient ,max-pprob) ,remainder))))

;;; ---------------------------------------------------------------------------

(defun truncate% (pprob &optional (pprob-divisor 1000))
  (multiple-value-bind (quotient remainder)
      (truncate& pprob pprob-divisor)
    (values (*& quotient max-pprob) remainder)))

(defcm truncate% (pprob &optional (pprob-divisor 1000))
  (with-gensyms (quotient remainder)
    `(multiple-value-bind (,quotient ,remainder)
         (truncate& ,pprob ,pprob-divisor)
       (values (*& ,quotient ,max-pprob) ,remainder))))

;;; ---------------------------------------------------------------------------

(defun fceiling% (pprob &optional (pprob-divisor 1000))
  (fceiling& pprob pprob-divisor))

(defcm fceiling% (pprob &optional (pprob-divisor 1000))
  `(fceiling& ,pprob ,pprob-divisor))

;;; ---------------------------------------------------------------------------

(defun ffloor% (pprob &optional (pprob-divisor 1000))
  (ffloor& pprob pprob-divisor))

(defcm ffloor% (pprob &optional (pprob-divisor 1000))
  `(ffloor& ,pprob ,pprob-divisor))

;;; ---------------------------------------------------------------------------

(defun fround% (pprob &optional (pprob-divisor 1000))
  (fround& pprob pprob-divisor))

(defcm fround% (pprob &optional (pprob-divisor 1000))
  `(fround& ,pprob ,pprob-divisor))

;;; ---------------------------------------------------------------------------

(defun ftruncate% (pprob &optional (pprob-divisor 1000))
  (ftruncate& pprob pprob-divisor))

(defcm ftruncate% (pprob &optional (pprob-divisor 1000))
  `(ftruncate& ,pprob ,pprob-divisor))

;;; ---------------------------------------------------------------------------
;;;  Using expanders defined in tools.lisp...

(defmacro decf%/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-& key decr place keys env))

(defmacro pushnew/incf%-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+& key incr place keys env))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================

