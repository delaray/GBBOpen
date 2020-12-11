;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/declared-numerics.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Nov  3 20:48:51 2012 *-*
;;;; *-* Machine: phoenix.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *            Shorthand Declared Numeric Operations and Values
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2012, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;  The implementation-specific numeric type predicates fixnump, 
;;;  single-float-p, and double-float-p need to be imported or defined.
;;;
;;;  Infinity and -infinity values are implementation dependent and must be
;;;  defined for each new port.  These are generally IEEE 754 number-format
;;;  values for infinity (INF) and negative infinity (-INF).  Many
;;;  implementations use read-time evaluation (#.) notation to represent these
;;;  (a non-evaluating, standard reader syntax is sorely needed!).  Lispworks
;;;  uses an extended number syntax with a doubled exponent sign (with the
;;;  second sign positive) to encode infinite values:
;;;     1e++0  positive infinity
;;;    -1e++0  negative infinity
;;;  Lispworks also uses a similar extension (but with a negative second 
;;;  exponent sign) to encode not-a-numbers (NaNs):
;;;     1e+-0  NaN
;;;  The sign of the first exponent sign (+ or -) doesn't matter in
;;;  determining the value (so 1e-+0 is also infinity and 1d--0 is also NaN).
;;;  The type of the value can be encoded just as with any other float
;;;  (1s++0, 1d++0, etc.).  
;;;
;;;  We proposed wider adoption of the Lispworks encoding scheme in other
;;;  Common Lisp implementations and provided patches for Clozure CL, CMUCL,
;;;  and SBCL to use the Lispworks representation.  Our proposal met with
;;;  mixed opinions, and Nikodemus Siivola suggested an alternative approach
;;;  using PRINT-OBJECT and a #@ dispatch macro.  This is the approach that is
;;;  currently being used in GBBopen, but it has three issues:
;;;    1. Printing infinite values using #@ does not work in Lispworks, as we
;;;       are unable to use PRINT-OBJECT.
;;;    2. Digitool MCL has an existing #@ dispatch that we have to work around
;;;    3. Other packages might also want to use the #@ dispatch for other
;;;       purposes
;;;  Until CL implementations "standardize" on a portable, non-read-eval-based, 
;;;  mechanism for infinite values, this is our best attempt.
;;;
;;;  Most Common Lisp implementations map double-float numbers to the the
;;;  64-bit IEEE 754 double format and single-float numbers to the 32-bit IEEE
;;;  754 single format.
;;;
;;;  Digitool's Macintosh Common Lisp (MCL) maps both the double-float and
;;;  single-float types to the 64-bit IEEE 754 double format (*only* the
;;;  short-float type maps to the IEEE 754 single format).
;;;
;;;  Prior to their 5.0 release, Lispworks supported only the IEEE 754 double
;;;  format, so double-float and single-float type declarations were
;;;  equivalent.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-04-02 File created.  (Corkill)
;;;  02-20-04 Added full-safety escape feature.  (Corkill)
;;;  03-13-04 Added infinity values.  (Corkill)
;;;  04-13-04 Added SBCL & CMUCL infinity values.  (Corkill)
;;;  05-21-04 Export FIXNUMP, SINGLE-FLOAT-P, and DOUBLE-FLOAT-P.  (Corkill)
;;;  01-11-05 Added fixnum infinity values.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  08-23-05 Added COERCE&, COERCE$&, COERCE$, COERCE$$, and COERCE$$.  (Corkill)
;;;  10-31-05 Add non-*read-eval*, *print-readably* printing for infinity and 
;;;           NaN values for SBCL, Clozure CL, & CMUCL.  (Corkill)
;;;  11-02-05 Added CormanLisp support (faking infinity for now).  (Corkill)
;;;  11-27-05 Changed infinity reading/printing to #@ dispatching macro (as
;;;           suggested by Nikodemus Siivola); no special NaN I/O.  (Corkill)
;;;  01-18-06 Added declared-numeric ABS functions.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  03-24-06 Added infinity-not-available feature.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  11-15-06 Added short-float support.  (Corkill)
;;;  06-29-08 Changed most operators from macros to functions with 
;;;           compiler-macros.  (Corkill)
;;;  08-03-09 Added declared-numeric COMPARE functions.  (Corkill)
;;;  11-03-12 Added REM&, REM$&, REM$, REM$$, REM$$$.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   #+abcl
   '(ext:fixnump)
   #+allegro
   '(excl:fixnump excl:single-float-p excl:double-float-p)
   #+clisp
   '(sys::fixnump sys::short-float-p sys::single-float-p sys::double-float-p
     sys::long-float-p)
   #+clozure
   '(ccl:fixnump ccl::short-float-p ccl::double-float-p)
   #+cmu 
   '(extensions:fixnump lisp::short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
   #+cormanlisp
   '(lisp::fixnump lisp::short-float-p lisp::single-float-p
     lisp::double-float-p lisp::long-float-p)
   #+digitool-mcl
   '(ccl:fixnump ccl::double-float-p)
   #+ecl
   '(si:fixnump)
   #+gcl
   '(system:fixnump)
   #+lispworks
   '(lispworks:fixnump lispworks:short-float-p hcl:single-float-p
     hcl:double-float-p lispworks:long-float-p)
   #+sbcl
   '(sb-int:fixnump sb-int:short-float-p sb-int:single-float-p
     sb-int:double-float-p sb-int:long-float-p)
   #+scl
   '(ext:fixnump lisp::short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
   #-(or abcl allegro clisp clozure cmu cormanlisp digitool-mcl ecl gcl 
         lispworks sbcl scl)
   (need-to-port (fixnump short-float-p single-float-p double-float-p
                          long-float-p))))

;;; CLs that don't have short-float-p predicates:
#+(or abcl allegro ecl gcl)
(defun short-float-p (obj)
  (typep obj 'short-float))

#+(or abcl allegro ecl gcl)
(defcm short-float-p (obj)
  `(typep ,obj 'short-float))

;;; CLs that don't have single-float-p predicates:
#+(or abcl clozure digitool-mcl ecl gcl)
(defun single-float-p (obj)
  (typep obj 'single-float))

#+(or abcl clozure digitool-mcl ecl gcl)
(defcm single-float-p (obj)
  `(typep ,obj 'single-float))

;;; CLs that don't have double-float-p predicates:
#+(or abcl ecl gcl)
(defun double-float-p (obj)
  (typep obj 'double-float))

#+(or abcl ecl gcl)
(defcm double-float-p (obj)
  `(typep ,obj 'double-float))

;;; CLs that don't have long-float-p predicates:
#+(or abcl allegro clozure ecl gcl)
(defun long-float-p (obj)
  (typep obj 'long-float))

#+(or abcl allegro clozure ecl gcl)
(defcm long-float-p (obj)
  `(typep ,obj 'long-float))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(;; Numeric types
            fixnump short-float-p single-float-p double-float-p long-float-p
            ;; Coercion:
            coerce& coerce$& coerce$ coerce$$ coerce$$$
            ;; Declared fixnum ops:
            & /& *& +& -& 1+& 1-& 
            /=& <& <=& =& >& >=& 
            bounded-value& ceiling& compare& decf& decf&-after evenp& 
            floor& fceiling& ffloor& fround& ftruncate& 
            incf& incf&-after max& min& minusp& 
            abs& mod& oddp& plusp& rem& round& truncate& zerop&
            ;; Declared short-float ops:
            $& /$& *$& +$& -$& 1+$& 1-$&
            /=$& <$& <=$& =$& >$& >=$& 
            bounded-value$& ceiling$& compare$& decf$& decf$&-after evenp$& 
            floor$& fceiling$& ffloor$& fround$& ftruncate$& 
            incf$& incf$&-after max$& min$& minusp$& 
            abs$& mod$& oddp$& plusp$& rem$& round$& truncate$& zerop$&
            ;; Declared single-float ops:
            $ /$ *$ +$ -$ 1+$ 1-$
            /=$ <$ <=$ =$ >$ >=$ 
            bounded-value$ ceiling$ compare$ decf$ decf$-after evenp$ 
            floor$ fceiling$ ffloor$ fround$ ftruncate$
            incf$ incf$-after max$ min$ minusp$ 
            abs$ mod$ oddp$ plusp$ rem$ round$ truncate$ zerop$
            ;; Declared double-float ops:
            $$ /$$ *$$ +$$ -$$ 1+$$ 1-$$ 
            /=$$ <$$ <=$$ =$$ >$$ >=$$ 
            bounded-value$$ ceiling$$ compare$$ decf$$ decf$$-after evenp$$ 
            floor$$ fceiling$$ ffloor$$ fround$$ ftruncate$$
            incf$$ incf$$-after max$$ min$$ minusp$$ 
            abs$$ mod$$ oddp$$ plusp$$ rem$$ round$$ truncate$$ zerop$$
            ;; Declared long-float ops:
            $$$ /$$$ *$$$ +$$$ -$$$ 1+$$$ 1-$$$ 
            /=$$$ <$$$ <=$$$ =$$$ >$$$ >=$$$ 
            bounded-value$$$ ceiling$$$ compare$$$ decf$$$ decf$$$-after evenp$$$ 
            floor$$$ fceiling$$$ ffloor$$$ fround$$$ ftruncate$$$
            incf$$$ incf$$$-after max$$$ min$$$ minusp$$$ 
            abs$$$ mod$$$ oddp$$$ plusp$$$ rem$$$ round$$$ truncate$$$ zerop$$$
            ;; Infinite values:
            infinity -infinity infinity& -infinity&
            infinity$ -infinity$ infinity$& -infinity$&
            infinity$$ -infinity$$ infinity$$$ -infinity$$$
            ;; Infinity reader escape hook (undocumented):            
            *inf-reader-escape-hook*
            ;; Infinity reader macro-character checker (undocumented):
            check-for-inf-reader
            ;; Infinity reader macro-character setting (undocumented):
            set-inf-reader-dispatch-macro-character)))

;;; ---------------------------------------------------------------------------
;;;  The fastest (/ fixnum fixnum) => fixnum [completely-fixnum division]
;;;  operator for each CL (as determined by :cl-timing tests) -- either
;;;  /&, floor&, or truncate&.
;;;
;;; Programmers must take care in using /& only where fixnum results will be
;;; created.

(defconstant fastest-fixnum-div-operator
    ;; When timings are very close, truncate& is preferred.  Tested on x86 and
    ;; PPC architectures (could vary on others--reports welcomed!).
    (or #+abcl 'truncate&
        #+allegro 'truncate&
        #+clisp 'floor&
        #+clozure 'truncate&
        #+cmu 'truncate&
        #+digitool-mcl 'truncate&
        #+ecl 'floor&
        #+lispworks 'truncate&
        #+sbcl 'truncate&
        #+scl 'truncate&
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
        (need-to-port fastest-fixnum-div-operator)))

;;; ---------------------------------------------------------------------------
;;; Warn if the CL implementation doesn't have at least 29-bit fixnums and
;;; note if the fixnum size supports unsigned-byte 32 values, pushing
;;; features accordingly:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((fixnum-size (1+ (integer-length most-positive-fixnum))))
    (cond 
     ((> fixnum-size 32)
      (pushnew :fixnum-size-supports-unsigned-byte-32 *features*))
     ((< fixnum-size 29)
      (pushnew :fixnum-size-below-29 *features*)))))

#+fixnum-size-below-29
(defun small-fixnum-warning ()
  (let ((fixnum-size #.(1+ (integer-length most-positive-fixnum))))
    (warn "Fixnums on ~a (~a) are only ~s bits long."
          fixnum-size
          (lisp-implementation-type) 
          (machine-type))))

#+fixnum-size-below-29
(small-fixnum-warning)

;;; ---------------------------------------------------------------------------
;;;  Check if various floats are not implemented distinctly (also run at
;;;  compile time in order to push features during compilation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-for-numeric-type (declared-type feature)
    (let ((actual-type (type-of (coerce 1.0l0 declared-type))))
      (when (eq actual-type declared-type)
        (pushnew feature *features*))))
  (check-for-numeric-type 'short-float ':has-short-float)
  (check-for-numeric-type 'single-float ':has-single-float)
  (check-for-numeric-type 'double-float ':has-double-float)
  (check-for-numeric-type 'long-float ':has-long-float))

;;; ---------------------------------------------------------------------------
;;;  Warn if various floats are not implemented distinctly

(defun warn-numeric-type (declared-type feature)
  (unless (member feature *features*)
    (let ((actual-type (type-of (coerce 1.0l0 declared-type))))
      (warn "~s is equivalent to ~s on ~a~@[ running on ~a~]."
            declared-type
            actual-type
            (lisp-implementation-type) 
            (machine-type)))))
  
(warn-numeric-type 'short-float ':has-short-float)
(warn-numeric-type 'single-float ':has-single-float)
(warn-numeric-type 'double-float ':has-double-float)
(warn-numeric-type 'long-float ':has-long-float)

;;; ---------------------------------------------------------------------------
;;; Allows a function to be called from the #@ inf-reader function, if the
;;; object read is not one of the legal infinite-value names:

(declaim (special *inf-reader-escape-hook*))
(unless (boundp '*inf-reader-escape-hook*)
  (setf *inf-reader-escape-hook* nil))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dn-defcm-expander (type op args result values-types)
    ;;; Builds a form declaring all the arguments to `op' to be `type.'  If
    ;;; `result' is true then the type of the result of the operation is also
    ;;; declared.
    (let ((form `(,op ,.(flet ((fn (x) `(the ,type ,x)))
                          (declare (dynamic-extent #'fn))
                          (mapcar #'fn args)))))
      (if (and result
               (not (member ':full-safety *features* :test #'eq)))
          `(the ,(if values-types
                     (cons 'values values-types)
                     type) 
             ,form)
          form)))

  (defmacro defdn (dn-symbol op &optional result values-types)
    (let ((type (ecase dn-symbol
                  (& 'fixnum)
                  ($& 'short-float)
                  ($ 'single-float)
                  ($$ 'double-float)
                  ($$$ 'long-float)))
          (dn-op (intern (concatenate 'simple-string
                           (symbol-name op)
                           (symbol-name dn-symbol)))))
      `(progn
         (defcm ,dn-op (&rest args)            
           (dn-defcm-expander ',type ',op  args ',result ',values-types))
         (defun ,dn-op (&rest args) 
           (declare (dynamic-extent args))
           ;; optimize this someday
           (apply ',op args))))))

;;; ===========================================================================
;;;   Fixnum Operations

(defmacro & (arg)
  ;;; Wraps (the fixnum ...) around `arg'
  (if  (feature-present-p ':full-safety)
      `,arg
      `(the fixnum ,arg)))
  
(defun unable-to-coerce-to-fixnum-error (value)
  (error "Unable to coerce ~s to a fixnum" value))

(defun coerce& (arg) 
  ;; avoid truncate call if not required:
  (if (typep arg 'fixnum)
      arg
      ;; Allow (coerce& 1.0) for symmetry with other declared-numeric coerce
      ;; operators, even though CL doesn't allow either (coerce 1.0 'integer)
      ;; or (coerce 1.0 'fixnum):
      (multiple-value-bind (result remainder)
          (truncate arg)
        (unless (and (zerop remainder)
                     (typep result 'fixnum))
          (unable-to-coerce-to-fixnum-error arg))
        result)))

(defcm coerce& (arg)
  (with-once-only-bindings (arg)
    (with-gensyms (result remainder)
      ;; avoid truncate call if not required:
      `(if (typep ,arg 'fixnum)
           ,arg
           (multiple-value-bind (,result ,remainder)
               (truncate ,arg)
             (unless (and (zerop ,remainder)
                          (typep ,result 'fixnum))
               (unable-to-coerce-to-fixnum-error ,arg))
             ,result)))))

(defdn & + t)
(defdn & 1+  t)
(defdn & - t)
(defdn & 1- t)
(defdn & * t)
(defdn & / t) ;; avoid using this one!!!
(defdn & =)
(defdn & /=)
(defdn & <)
(defdn & <=)
(defdn & >)
(defdn & >=)
(defdn & min t)
(defdn & max t)
(defdn & zerop)
(defdn & plusp)
(defdn & minusp)
(defdn & evenp)
(defdn & oddp)
(defdn & abs t)
(defdn & mod t)
(defdn & rem t)
(defdn & floor t (fixnum fixnum))
(defdn & ceiling t (fixnum fixnum))
(defdn & truncate t (fixnum fixnum))
(defdn & round t (fixnum fixnum))
(defdn & ffloor t (float fixnum))
(defdn & fceiling t (float fixnum))
(defdn & ftruncate t (float fixnum))
(defdn & fround t (float fixnum))

(define-modify-macro incf& (&optional (increment 1)) +&)
(define-modify-macro decf& (&optional (increment 1)) -&)

(defmacro incf&-after (place &optional (increment 1) &environment env)
  ;;; Like incf&, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+& 'incf&))

(defmacro decf&-after (place &optional (increment 1) &environment env)
  ;;; Like decf&, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '-& 'decf&))

(defun bounded-value& (min n max)
  (max& min (min& n max)))

(defcm bounded-value& (min n max)
  `(max& ,min (min& ,n ,max)))

(defun compare& (a b)
  ;; Users must be careful that the result is a fixnum!
  (-& a b))

;;; ===========================================================================
;;;   Short-Float Operations

(defmacro $& (arg)
  ;;; Wraps (the short-float ...) around `arg'
  (if (feature-present-p ':full-safety)
      `,arg
      `(the short-float ,arg)))

(defun coerce$& (arg) 
  ;; avoid coercion if not required (some CLs will coerce anyway):
  (if (typep arg 'short-float)
      arg
      (coerce arg 'short-float)))

(defcm coerce$& (arg) 
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'short-float)
         ,arg
         (coerce ,arg 'short-float))))

(defdn $& + t)
(defdn $& 1+  t)
(defdn $& - t)
(defdn $& 1- t)
(defdn $& * t)
(defdn $& / t)
(defdn $& =)
(defdn $& /=)
(defdn $& <)
(defdn $& <=)
(defdn $& >)
(defdn $& >=)
(defdn $& min t)
(defdn $& max t)
(defdn $& zerop)
(defdn $& plusp)
(defdn $& minusp)
(defdn $& evenp)
(defdn $& oddp)
(defdn $& abs t)
(defdn $& mod t)
(defdn $& rem t)
(defdn $& floor t (fixnum short-float))
(defdn $& ceiling t (fixnum short-float))
(defdn $& truncate t (fixnum short-float))
(defdn $& round t (fixnum short-float))
(defdn $& ffloor t (short-float short-float))
(defdn $& fceiling t (short-float short-float))
(defdn $& ftruncate t (short-float short-float))
(defdn $& fround t (short-float short-float))

(define-modify-macro incf$& (&optional (increment 1.0s0)) +$&)
(define-modify-macro decf$& (&optional (increment 1.0s0)) -$&)

(defmacro incf$&-after (place &optional (increment 1.0s0) &environment env)
  ;;; Like incf$&, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+$& 'incf$&))

(defmacro decf$&-after (place &optional (increment 1.0s0) &environment env)
  ;;; Like decf$&, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '-$& 'decf$&))
  
(defun bounded-value$& (min n max)
  (max$& min (min$& n max)))

(defcm bounded-value$& (min n max)
  `(max$& ,min (min$& ,n ,max)))

(defun compare$& (a b)
  (cond ((<$& a b) -1)
        ((>$& a b) 1)
        (t 0)))

;;; ===========================================================================
;;;   Single-Float Operations

(defmacro $ (arg)
  ;;; Wraps (the single-float ...) around `arg'
  (if (feature-present-p ':full-safety)
      `,arg
      `(the single-float ,arg)))

(defun coerce$ (arg) (coerce arg 'single-float))

(defcm coerce$ (arg) 
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'single-float)
         ,arg
         (coerce ,arg 'single-float))))

(defdn $ + t)
(defdn $ 1+  t)
(defdn $ - t)
(defdn $ 1- t)
(defdn $ * t)
(defdn $ / t)
(defdn $ =)
(defdn $ /=)
(defdn $ <)
(defdn $ <=)
(defdn $ >)
(defdn $ >=)
(defdn $ min t)
(defdn $ max t)
(defdn $ zerop)
(defdn $ plusp)
(defdn $ minusp)
(defdn $ evenp)
(defdn $ oddp)
(defdn $ abs t)
(defdn $ mod t)
(defdn $ rem t)
(defdn $ floor t (fixnum single-float))
(defdn $ ceiling t (fixnum single-float))
(defdn $ truncate t (fixnum single-float))
(defdn $ round t (fixnum single-float))
(defdn $ ffloor t (single-float single-float))
(defdn $ fceiling t (single-float single-float))
(defdn $ ftruncate t (single-float single-float))
(defdn $ fround t (single-float single-float))

(define-modify-macro incf$ (&optional (increment 1.0f0)) +$)
(define-modify-macro decf$ (&optional (increment 1.0f0)) -$)

(defmacro incf$-after (place &optional (increment 1.0f0) &environment env)
  ;;; Like incf$, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+$ 'incf$))

(defmacro decf$-after (place &optional (increment 1.0f0) &environment env)
  ;;; Like decf$, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '-$ 'decf$))

(defun bounded-value$ (min n max)
  (max$ min (min$ n max)))

(defcm bounded-value$ (min n max)
  `(max$ ,min (min$ ,n ,max)))

(defun compare$ (a b)
  (cond ((<$ a b) -1)
        ((>$ a b) 1)
        (t 0)))

;;; ===========================================================================
;;;   Double-Float Operations

(defmacro $% (arg)
  ;;; Wraps (the double-float ...) around `arg'
  (if (feature-present-p ':full-safety)
      `,arg
      `(the double-float ,arg)))

(defun coerce$$ (arg) (coerce arg 'double-float))

(defcm coerce$$ (arg)
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'double-float)
         ,arg
         (coerce ,arg 'double-float))))

(defdn $$ + t)
(defdn $$ 1+  t)
(defdn $$ - t)
(defdn $$ 1- t)
(defdn $$ * t)
(defdn $$ / t)
(defdn $$ =)
(defdn $$ /=)
(defdn $$ <)
(defdn $$ <=)
(defdn $$ >)
(defdn $$ >=)
(defdn $$ min t)
(defdn $$ max t)
(defdn $$ zerop)
(defdn $$ plusp)
(defdn $$ minusp)
(defdn $$ evenp)
(defdn $$ oddp)
(defdn $$ abs t)
(defdn $$ mod t)
(defdn $$ rem t)
(defdn $$ floor t (fixnum double-float))
(defdn $$ ceiling t (fixnum double-float))
(defdn $$ truncate t (fixnum double-float))
(defdn $$ round t (fixnum double-float))
(defdn $$ ffloor t (double-float double-float))
(defdn $$ fceiling t (double-float double-float))
(defdn $$ ftruncate t (double-float double-float))
(defdn $$ fround t (double-float double-float))

(define-modify-macro incf$$ (&optional (increment 1.0d0)) +$$)
(define-modify-macro decf$$ (&optional (increment 1.0d0)) -$$)
 
(defmacro incf$$-after (place &optional (increment 1.0d0) &environment env)
  ;;; Like incf$$, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+$$ 'incf$$))

(defmacro decf$$-after (place &optional (increment 1.0d0) &environment env)
  ;;; Like decf$$, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '-$$ 'decf$$))

(defun bounded-value$$ (min n max)
  (max$$ min (min$$ n max)))

(defcm bounded-value$$ (min n max)
  `(max$$ ,min (min$$ ,n ,max)))

(defun compare$$ (a b)
  (cond ((<$$ a b) -1)
        ((>$$ a b) 1)
        (t 0)))

;;; ===========================================================================
;;;   Long-Float Operations

(defmacro $$$ (arg)
  ;;; Wraps (the long-float ...) around `arg'
  (if (feature-present-p ':full-safety)
      `,arg
      `(the long-float ,arg)))

(defun coerce$$$ (arg) (coerce arg 'long-float))

(defcm coerce$$$ (arg)
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'long-float)
         ,arg
         (coerce ,arg 'long-float))))

(defdn $$$ + t)
(defdn $$$ 1+  t)
(defdn $$$ - t)
(defdn $$$ 1- t)
(defdn $$$ * t)
(defdn $$$ / t)
(defdn $$$ =)
(defdn $$$ /=)
(defdn $$$ <)
(defdn $$$ <=)
(defdn $$$ >)
(defdn $$$ >=)
(defdn $$$ min t)
(defdn $$$ max t)
(defdn $$$ zerop)
(defdn $$$ plusp)
(defdn $$$ minusp)
(defdn $$$ evenp)
(defdn $$$ oddp)
(defdn $$$ abs t)
(defdn $$$ mod t)
(defdn $$$ rem t)
(defdn $$$ floor t (fixnum long-float))
(defdn $$$ ceiling t (fixnum long-float))
(defdn $$$ truncate t (fixnum long-float))
(defdn $$$ round t (fixnum long-float))
(defdn $$$ ffloor t (long-float long-float))
(defdn $$$ fceiling t (long-float long-float))
(defdn $$$ ftruncate t (long-float long-float))
(defdn $$$ fround t (long-float long-float))
  
(define-modify-macro incf$$$ (&optional (increment 1.0l0)) +$$$)
(define-modify-macro decf$$$ (&optional (increment 1.0l0)) -$$$)

(defmacro incf$$$-after (place &optional (increment 1.0l0) &environment env)
  ;;; Like incf$$$, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+$$$ 'incf$$$))

(defmacro decf$$$-after (place &optional (increment 1.0l0) &environment env)
  ;;; Like decf$$$, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '-$$$ 'decf$$$))
  
(defun bounded-value$$$ (min n max)
  (max$$$ min (min$$$ n max)))

(defcm bounded-value$$$ (min n max)
  `(max$$$ ,min (min$$$ ,n ,max)))
 
(defun compare$$$ (a b)
  (cond ((<$$$ a b) -1)
        ((>$$$ a b) 1)
        (t 0)))

;;; ===========================================================================
;;;   Infinity Values
;;;
;;;  Infinity values are not required by the CL standard, but most they are
;;;  provided in most CL implementations.  We also want to be able to save and
;;;  communicate infinity values, and using implementation-specific #.
;;;  representations won't work between implementations or if *READ-EVAL*
;;;  is turned off.  We like the text-representation approach used by 
;;;  Lispworks:
;;;     1e++0 => positive infinity
;;;    -1e++0 => negative infinity
;;;     1e+-0 => not-a-number
;;;  but we have been unable to influence a broader adoption in other CL
;;;  implementations.  So, in GBBopen, we resort to a #@ dispatch macro
;;;  mechanism (see below), which has its own problems.  This is an area
;;;  where some defacto standardization is sorely needed.
;;;
;;;  Note: 
;;;    * CLISP, ECL (if configured without ieee-fp), and GCL do not support 
;;;      IEEE 754 infinity representations

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or clisp 
        cormanlisp
        (and ecl (not ieee-floating-point))
        gcl)
  (pushnew ':infinity-not-available *features*))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
    
  ;; --------------------------------------------------------------------------
  ;; Fixnum infinity approximations:
  
  (defconstant infinity& most-positive-fixnum)
  (defconstant -infinity& most-negative-fixnum)
  
  ;; --------------------------------------------------------------------------
  ;; Double-float infinities (defined first, so that we can use them in
  ;;                          defining other types, when needed)
  
  (defconstant infinity$$
      #+abcl ext:double-float-positive-infinity
      #+allegro excl::*infinity-double*
      #+clozure #.(unwind-protect
                      (progn
                        (ccl:set-fpu-mode :division-by-zero nil)
                        (/ 0d0))
                    (ccl:set-fpu-mode :division-by-zero t))
      #+cmu ext:double-float-positive-infinity
      #+digitool-mcl #.(unwind-protect
                           (progn
                             (ccl:set-fpu-mode :division-by-zero nil)
                             (/ 0d0))
                         (ccl:set-fpu-mode :division-by-zero t))
      #+(and ecl (not infinity-not-available)) si:double-float-positive-infinity
      #+lispworks #.(read-from-string "10E999")
      #+sbcl sb-ext:double-float-positive-infinity
      #+scl ext:double-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-double-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port infinity$$))

  (defconstant -infinity$$
      #+abcl ext:double-float-negative-infinity
      #+allegro excl::*negative-infinity-double*
      #+clozure #.(unwind-protect
                      (progn
                        (ccl:set-fpu-mode :division-by-zero nil)
                        (/ -0d0))
                    (ccl:set-fpu-mode :division-by-zero t))
      #+cmu ext:double-float-negative-infinity
      #+digitool-mcl #.(unwind-protect
                           (progn
                             (ccl:set-fpu-mode :division-by-zero nil)
                             (/ -0d0))
                         (ccl:set-fpu-mode :division-by-zero t))
      #+(and ecl (not infinity-not-available)) si:double-float-negative-infinity
      #+lispworks #.(read-from-string "-10E999")
      #+sbcl sb-ext:double-float-negative-infinity
      #+scl ext:double-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-double-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port -infinity$$))

  ;; --------------------------------------------------------------------------
  ;; Single-float infinities:
  
  (defconstant infinity$
      #+abcl ext:single-float-positive-infinity
      #+allegro excl::*infinity-single*
      #+clozure (coerce infinity$$ 'single-float)
      #+cmu ext:single-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'single-float)
      #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
      #+lispworks (coerce infinity$$ 'single-float)
      #+sbcl sb-ext:single-float-positive-infinity
      #+scl ext:single-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-single-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port infinity$))
  
  (defconstant -infinity$
      #+abcl ext:single-float-negative-infinity
      #+allegro excl::*negative-infinity-single*
      #+clozure (coerce -infinity$$ 'single-float)
      #+cmu ext:single-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'single-float)
      #+(and ecl (not infinity-not-available)) si:single-float-negative-infinity
      #+lispworks (coerce -infinity$$ 'single-float)
      #+sbcl sb-ext:single-float-negative-infinity
      #+scl ext:single-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-single-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port -infinity$))
  
  ;; --------------------------------------------------------------------------
  ;; Short-float infinities:
  
  (defconstant infinity$&
      #+abcl infinity$
      #+allegro infinity$
      #+clozure (coerce infinity$$ 'short-float)
      #+cmu ext:short-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'short-float)
      #+(and ecl (not infinity-not-available)) si:short-float-positive-infinity
      #+lispworks (coerce infinity$$ 'short-float)
      #+sbcl sb-ext:short-float-positive-infinity
      #+scl ext:short-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-short-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port infinity$&))
  
  (defconstant -infinity$&
      #+abcl -infinity$
      #+allegro -infinity$
      #+clozure (coerce -infinity$$ 'short-float)
      #+cmu ext:short-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'short-float)
      #+(and ecl (not infinity-not-available)) si:short-float-negative-infinity
      #+lispworks (coerce -infinity$$ 'short-float)
      #+sbcl sb-ext:short-float-negative-infinity
      #+scl ext:short-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-short-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port -infinity$&))

  ;; --------------------------------------------------------------------------
  ;; Long-float infinities:
  
  (defconstant infinity$$$
      #+abcl infinity$$
      #+allegro infinity$$
      #+clozure (coerce infinity$$ 'long-float)
      #+cmu ext:long-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'long-float)
      #+(and ecl (not infinity-not-available)) si:long-float-positive-infinity
      #+lispworks (coerce infinity$$ 'long-float)
      #+sbcl sb-ext:long-float-positive-infinity
      #+scl ext:long-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-long-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port infinity$$$))
  
  (defconstant -infinity$$$
      #+abcl -infinity$$
      #+allegro -infinity$$
      #+clozure (coerce -infinity$$ 'long-float)
      #+cmu ext:long-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'long-float)
      #+(and ecl (not infinity-not-available)) si:long-float-negative-infinity
      #+lispworks (coerce -infinity$$ 'long-float)
      #+sbcl sb-ext:long-float-negative-infinity
      #+scl ext:long-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-positive-long-float
      #-(or abcl allegro clozure cmu digitool-mcl 
            (and ecl (not infinity-not-available)) 
            lispworks sbcl scl infinity-not-available)
      (need-to-port -infinity$$$))
  
  ;; --------------------------------------------------------------------------
  ;; Generic infinities:
  
  (defconstant infinity infinity$)
  (defconstant -infinity -infinity$))
  
;;; ---------------------------------------------------------------------------
;;;   INF input & output 
;;;
;;;   Until CL implementations "standardize" on a portable,
;;;   non-read-eval-based, mechanism for infinite values, we roll our own
;;;   using PRINT-OBJECT and a #@ dispatch macro.  (Thanks to Nikodemus
;;;   Siivola for suggesting this approach.)
;;;
;;;   The Lispworks printer does not call our PRINT-OBJECT methods, because
;;;   the methods violate the conforming program rules that state that the
;;;   consequences are undefined for a method on a standardized generic
;;;   function which is applicable when all of the arguments are direct
;;;   instances of standardized classes.  We've yet to figure out a work
;;;   around.
;;;
;;;   Loading Clozure's COCOA-bridge after loading GBBopen Tools, will
;;;   overwrite the #@ inf-reader, requiring a call to
;;;   SET-INF-READER-DISPATCH-MACRO-CHARACTER to restore infinite-value
;;;   reading.

(defun inf-reader (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (let ((what (with-standard-io-syntax  ; protect against foolery
                (read stream t nil 't))))
    (flet ((illegal-value-error (what)
             (error "Illegal infinite-value specifier: #@~s" what)))     
      (typecase what
        (symbol 
         (flet ((symbol-equal (a b)
                  (string-equal (symbol-name a) (symbol-name b))))
           (cond
            ((symbol-equal what 'short-float-infinity) infinity$&)
            ((symbol-equal what 'single-float-infinity) infinity$)
            ((symbol-equal what 'double-float-infinity) infinity$$)
            ((symbol-equal what 'long-float-infinity) infinity$$$)
            ((symbol-equal what 'short-float-negative-infinity) -infinity$&)
            ((symbol-equal what 'single-float-negative-infinity) -infinity$)
            ((symbol-equal what 'double-float-negative-infinity) -infinity$$)
            ((symbol-equal what 'long-float-negative-infinity) -infinity$$$)
            (*inf-reader-escape-hook*
             (funcall *inf-reader-escape-hook* what))
            (t (illegal-value-error what)))))
        ;; Maintain COCOA-bridge #@string extension:
        #+clozure
        (string
         (if (fboundp (find-symbol "objc-#@-reader" 
                                   (load-time-value (find-package :ccl))))
             (funcall (macro-function 'ccl:@) what)
             (illegal-value-error what)))        
        ;; Maintain Digitool MCL's #@(<big point>) extension:      
        #+digitool-mcl
        (cons (apply #'ccl::make-big-point what))
        ;; 
        (otherwise
         (if *inf-reader-escape-hook*
             (funcall *inf-reader-escape-hook* what)
             (illegal-value-error what)))))))

;;; ---------------------------------------------------------------------------

(defun set-inf-reader-dispatch-macro-character (&optional 
                                                (readtable *readtable*))
  "Set #@ dispatch macro-character to GBBopen Tools INF-READER function"
  (safely-set-dispatch-macro-character #\# #\@ 
                                       #-cormanlisp 'inf-reader
                                       #+cormanlisp #'inf-reader
                                       readtable))

(set-inf-reader-dispatch-macro-character)

;;; ---------------------------------------------------------------------------

(defun check-for-inf-reader ()
  (unless (eq 'inf-reader (get-dispatch-macro-character #\# #\@))
    (warn "No inf-reader dispatch set in ~s" *readtable*)))

;;; ---------------------------------------------------------------------------

;; We don't define infinite-value print-object methods on CLs with faked
;; infinities:
#-infinity-not-available
(let (#+lispworks          ;; Lispworks doesn't like such print-object methods
      (lispworks:*handle-warn-on-redefinition* nil))
  #+has-short-float
  (defmethod print-object ((obj (eql infinity$&)) stream)
    (format stream "#@short-float-infinity")
    obj) 
  #+has-single-float
  (defmethod print-object ((obj (eql infinity$)) stream)
    (format stream "#@single-float-infinity")
    obj) 
  #+has-double-float
  (defmethod print-object ((obj (eql infinity$$)) stream)
    (format stream "#@double-float-infinity")
    obj)
  #+has-long-float
  (defmethod print-object ((obj (eql infinity$$$)) stream)
    (format stream "#@long-float-infinity")
    obj)
  #+has-short-float
  (defmethod print-object ((obj (eql -infinity$&)) stream)
    (format stream "#@short-float-negative-infinity")
    obj)  
  #+has-single-float
  (defmethod print-object ((obj (eql -infinity$)) stream)
    (format stream "#@single-float-negative-infinity")
    obj)  
  #+has-double-float
  (defmethod print-object ((obj (eql -infinity$$)) stream)
    (format stream "#@double-float-negative-infinity")
    obj)
  #+has-long-float
  (defmethod print-object ((obj (eql -infinity$$$)) stream)
    (format stream "#@long-float-negative-infinity")
    obj))

;;; ---------------------------------------------------------------------------

;; From CMUCL's print.lisp:
#+cmu
(lisp::without-package-locks
 (defun lisp::output-float-infinity (x stream)
   (print-object x stream)))

;; Check and record if running an older ECL:
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((old-ecl-out-float-infinity-symbol
         (find-symbol "OUTPUT-FLOAT-INFINITY" :ext)))
    (when (and old-ecl-out-float-infinity-symbol
               (fboundp old-ecl-out-float-infinity-symbol))
      (pushnew ':old-ecl-with-output-float-infinity *features*))))

;; Older ECL verison:
#+(and ecl old-ecl-with-output-float-infinity)
(progn
  ;; Old ECL allows redefinition of its EXT:OUTPUT-FLOAT-INFINITY function, but its
  ;; compiler/loader uses different readtables so we can't use #@ format unless
  ;; the INF-READER dispatch is in place:
  (defvar *%saved-ecl-output-float-infinity-function%*
      (symbol-function 'ext:output-float-infinity))
  (defun ext:output-float-infinity (x stream)
    ;; Redefine ECL's EXT:OUTPUT-FLOAT-INFINITY to call PRINT-OBJECT unless 
    ;; INF-READER isn't established:
    (if (eq 'inf-reader (get-dispatch-macro-character #\# #\@))
        (print-object x stream)
        (funcall *%saved-ecl-output-float-infinity-function%* x stream))))

;; Later ECL versions (not as nice an interface):
#+(and ecl (not old-ecl-with-output-float-infinity))
(progn
  (defvar *%saved-ecl-float-infinity-string-function%*
      (symbol-function 'ext::float-infinity-string))
  ;; Redefine ECL's EXT::FLOAT-INFINITY-STRING to use #@format:
  (defun modified-ecl-float-infinity-string (x)
    (when (and *print-readably* (null *read-eval*))
      (error 'ext::print-not-readable :object x))
    (let* ((negative-infinities 
            '((single-float . "#@single-float-negative-infinity") 
              (double-float . "#@double-float-negative-infinity")
              (long-float . "#@long-float-negative-infinity")
              (short-float . "#@short-float-negative-infinity")))
           (positive-infinities 
            '((single-float . "#@single-float-infinity") 
              (double-float . "#@double-float-infinity")
              (long-float . "#@long-float-infinity")
              (short-float . "#@short-float-infinity")))
           (record (assoc (type-of x)
                          (if (plusp x) 
                              positive-infinities 
                              negative-infinities))))
      (unless record
        (error "Not an infinity"))
      (cdr record)))
  
  (defun ext::float-infinity-string (x)
    ;; Redefine ECL's EXT::FLOAT-INFINITY-STRING to use #@format unless
    ;; INF-READER isn't established:
    (funcall 
     (if (eq 'inf-reader (get-dispatch-macro-character #\# #\@))
         'modified-ecl-float-infinity-string
         *%saved-ecl-float-infinity-string-function%*)
     x)))

;; From SBCL's print.lisp:
#+sbcl
(sb-ext::without-package-locks
 (defun sb-impl::output-float-infinity (x stream)
   (print-object x stream)))

#+scl
(defun lisp::output-float-infinity (x stream)
  (print-object x stream))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
