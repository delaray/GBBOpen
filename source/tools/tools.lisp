;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/tools.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul  5 15:50:19 2012 *-*
;;;; *-* Machine: phoenix *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          Useful Lisp Extensions
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
;;;   MOP class-finalization functions used in ensure-finalized-class must be
;;;   imported. The MOP specializer-name extraction function and eql handling
;;;   used in undefmethod must also be addressed.
;;;
;;;   Print-pretty-function-object can be customized for a CL implementation.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-04-02 File created.  (Corkill)
;;;  03-10-04 Added PUSHNEW/INCF-ACONS.  (Corkill)
;;;  03-21-04 Added REMOVE-PROPERTIES.  (Corkill)
;;;  04-30-04 Added SET-EQUAL.  (Corkill)
;;;  05-10-04 Added DO-UNTIL.  (Corkill)
;;;  05-24-04 Added MACROLET-DEBUG.  (Corkill)
;;;  05-31-04 Improve COUNTED-DELETE and SET-EQUAL.  (Corkill)
;;;  06-06-04 Added ENSURE-LIST-OF-LISTS.  (Corkill)
;;;  07-08-04 Added XOR.  (Corkill)
;;;  07-15-04 Added READ-CHAR-IMMEDIATELY.  (Corkill)
;;;  05-27-04 Added SETS-OVERLAP-P.  (Corkill)
;;;  06-01-05 Added PRINT-PRETTY-FUNCTION-OBJECT.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  11-02-05 Added CormanLisp support.  (Corkill)
;;;  11-30-05 Rewrote LIST-LENGTH=1 as LIST-LENGTH-1-P.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  03-12-06 Added LIST-LENGTH-2-P.  (Corkill)
;;;  03-18-06 Added DOSEQUENCE.  (Corkill)
;;;  04-07-06 Added SHUFFLE-LIST.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  08-20-06 Added EXTRACT-DECLARATIONS.  (Corkill)
;;;  09-22-06 Added CormanLisp 3.0 support.  (Corkill)
;;;  12-05-07 Added SHRINK-VECTOR.  (Corkill)
;;;  01-06-08 Added LIST-LENGTH>1.  (Corkill)
;;;  01-09-08 Added LIST-LENGTH> and TRIMMED-SUBSTRING.  (Corkill)
;;;  02-29-08 Added handler-forms and error-condition lexical function to 
;;;           WITH-ERROR-HANDLING.  (Corkill)
;;;  02-09-08 Added NICER-Y-OR-N-P and NICER-YES-OR-NO-P.  (Corkill)
;;;  05-01-08 Added DECF/DELETE-ACONS.  (Corkill)
;;;  05-25-08 Added MULITPLE-VALUE-SETF.  (Corkill)
;;;  06-01-08 Added COMPILER-MACROEXPAND-1 and COMPILER-MACROEXPAND.  (Corkill)
;;;  06-25-08 Added :conditions option to WITH-ERROR-HANDLING and exclude
;;;           handling EXCL::INTERRUPT-SIGNAL on Allegro by default.  (Corkill) 
;;;  07-20-08 Added CASE-USING, CCASE-USING, and ECASE-USING macros.  (Corkill)
;;;  03-02-09 Added LIST-LENGTH>2.  (Corkill)
;;;  04-14-09 Added DOSUBLISTS.  (Corkill)
;;;  07-22-08 Added NICER-TIME.  (Corkill)
;;;  08-03-09 Added COMPARE and COMPARE-STRINGS.  (Corkill)
;;;  03-16-10 Added ASSQ.  (Corkill)
;;;  04-26-10 Added SORTF & STABLE-SORTF.  (Corkill)
;;;  09-09-10 Added WHITESPACE-CHAR-P.  (Corkill)
;;;  09-13-10 Added SORTED-MAPHASH.  (Corkill)
;;;  08-20-11 Added MAKE-HASH-VALUES-VECTOR.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------
;;;  Import routinely available entities, whenever possible

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   #+abcl
   '(system::simple-array-p)
   #+allegro 
   '(excl::assq 
     excl:case-failure
     excl::extract-declarations
     excl:interrupt-signal
     excl::memq
     excl::simple-array-p
     excl:until
     excl:while
     excl::whitespace-char-p
     excl::xor
     sys:copy-file)
   #+clisp
   '(posix::copy-file
     system::memq
     system::simple-array-p)
   ;; Note: Clozure's ccl:while doesn't include a NIL block, so we can't use it
   #+clozure
   '(ccl:compiler-macroexpand
     ccl:compiler-macroexpand-1
     ccl:copy-file
     ccl:delq     
     ccl:memq
     ccl::simple-array-p)
   #+cmu
   '(conditions::case-failure
     ext:compiler-macroexpand
     ext:compiler-macroexpand-1
     ext:assq
     ext:delq
     ext:memq
     kernel:simple-array-p
     lisp::whitespace-char-p)
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:compiler-macroexpand
     ccl:compiler-macroexpand-1
     ccl:copy-file
     ccl:delq
     ccl:memq
     ccl::simple-array-p)
   ;; Note: ECL's si:while doesn't include a NIL block, so we can't use it
   #+ecl
   '(si::case-failure
     si:copy-file
     si:memq
     si::simple-array-p)
   #+gcl
   '()
   #+lispworks
   '(conditions:case-failure
     harlequin-common-lisp:compiler-macroexpand
     harlequin-common-lisp:compiler-macroexpand-1
     harlequin-common-lisp:simple-array-p
     lispworks:whitespace-char-p
     system::copy-file
     system:assq
     system:memq
     system:delq)
   #+sbcl
   '(sb-int:assq
     sb-int:memq
     sb-int:delq
     sb-kernel:case-failure
     sb-kernel:simple-array-p)
   #+scl
   '(conditions::case-failure
     ext:compiler-macroexpand
     ext:compiler-macroexpand-1
     ext:assq
     ext:memq
     ext:delq
     kernel:simple-array-p
     lisp::whitespace-char-p)
   #-(or abcl allegro clisp clozure cmu cormanlisp digitool-mcl ecl gcl 
         lispworks sbcl scl)
   '()))

;;; ---------------------------------------------------------------------------
;;;  Exported tools entities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*disable-with-error-handling*
            assq
            bounded-value
            case-using
            case-using-failure
            ccase-using
            compare                     ; not yet documented
            compare-strings             ; not yet documented
            copy-file                   ; not yet documented
            compiler-macroexpand
            compiler-macroexpand-1
            counted-delete
            decf-after
            decf/delete-acons
            ;; --- declared-type operators:
            decf&/delete-acons
            decf$/delete-acons
            decf$&/delete-acons
            decf$$/delete-acons
            decf$$$/delete-acons
            define-directory            ; in module-manager, but part of tools
            delq
            delq-one
            do-until
            do-while
            dosequence
            dosublists
            dotted-conc-name            ; in module-manager, but part of tools;
                                        ; not documented
            dotted-length
            ecase-using
            error-condition             ; lexical fn in WITH-ERROR-HANDLING
            error-message               ; lexical fn in WITH-ERROR-HANDLING
            ensure-finalized-class
            ensure-list
            ensure-list-of-lists        ; not yet documented
            extract-declarations        ; not documented
            incf-after
            interrupt-signal
            list-length-1-p
            list-length-2-p
            list-length>
            list-length>1
            list-length>2
            macrolet-debug              ; not documented
            make-keyword
            make-hash-values-vector
            memq
            multiple-value-setf
            nicer-time                  ; not yet documented
            nicer-y-or-n-p              ; not yet documented
            nicer-yes-or-no-p           ; not yet documented
            nsorted-insert
            print-pretty-function-object ; not yet documented
            push-acons
            pushnew-acons
            pushnew/incf-acons
            ;; --- declared-type operators:
            pushnew/incf&-acons
            pushnew/incf$-acons
            pushnew/incf$&-acons
            pushnew/incf$$-acons
            pushnew/incf$$$-acons
            pushnew-elements
            read-char-immediately       ; not yet documented
            remove-property
            remove-properties
            resize-hash-table           ; not yet documented
            set-equal
            sets-overlap-p
            shuffle-list
            shrink-vector
            simple-array-p              ; not yet documented
            sole-element
            sorted-maphash
            sortf
            splitting-butlast
            stable-sortf
            trimmed-substring
            undefmethod
            until
            while
            whitespace-char-p           ; not yet documented
            with-error-handling
            with-opened-file            ; not yet documented
            xor)))

;;; ===========================================================================
;;;  Basic while and until macros

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro while (test &body body)
    `(loop (unless ,test (return))
       ,@body)))

#-allegro
(defmacro until (test &body body)
  `(loop (when ,test (return))
     ,@body))

(defmacro do-until (form test)
  `(loop ,form (when ,test (return))))

(defmacro do-while (form test)
  `(loop ,form (unless ,test (return))))

;;; ===========================================================================
;;;  With-error-handling
;;;
;;;  Evaluates body if an error occurs while evaluating form

(defvar *disable-with-error-handling* nil)

(defun error-message-string (condition)                            
  (let ((*print-readably* nil))
    (handler-case (format nil "~a" condition)
      (error () "<error: unprintable condition>"))))

;;; ---------------------------------------------------------------------------

(defmacro with-error-handling (form-and-handler &body error-body)
  ;;; Full signature:
  ;;;  (with-error-handling {form | 
  ;;;                        (form [(:conditions condition*)] handler-form*)}
  ;;;      error-form*)
  (let ((conditions 
         #+allegro '(and error (not interrupt-signal))
         #-allegro 'error))
    ;; Determine if form-and-handler is form or (form &body handler-body):
    (unless (and (consp form-and-handler)
                 (consp (car form-and-handler))
                 ;; Support CLHS 3.1.2.1.2.4 lambda form:
                 (not (eq (caar form-and-handler) 'lambda)))
      ;; Convert a simple form to a form with (null) handler-body:
      (setf form-and-handler (list form-and-handler)))
    (destructuring-bind (form &body handler-body)
        form-and-handler
      ;; Check handler-body for :conditions option:
      (when (and handler-body
                 (consp (first handler-body))
                 (eq ':conditions (first (first handler-body))))
        (setf conditions (sole-element (rest (first handler-body))))
        (setf handler-body (rest handler-body)))
      ;; Now generate the handler:
      (let ((block (gensym))
            (condition/tag (when error-body (gensym))))
        `(block ,block
           (let (,.(when error-body (list condition/tag)))
             (tagbody
               (flet
                   ((.conditioner. (condition)
                      ,@(if handler-body
                            `((flet ((error-message ()
                                       (error-message-string condition))
                                     (error-condition ()
                                       condition))
                                (declare (ignorable #'error-message
                                                    #'error-condition)
                                         (dynamic-extent #'error-message
                                                         #'error-condition))
                                ,@(if error-body
                                      `(,@handler-body
                                        (when *disable-with-error-handling*
                                          (error condition))
                                        ;; Save the condition for use
                                        ;; by (error-message) in error-body:
                                        (setf ,condition/tag condition)
                                        (go ,condition/tag))
                                      `((return-from ,block 
                                          (progn ,@handler-body))))))
                            `(,@(if error-body
                                    `(,@handler-body
                                      (when *disable-with-error-handling*
                                        (error condition))
                                      ;; Save the condition for use by
                                      ;; (error-message) in error-body:
                                      (setf ,condition/tag condition)
                                      (go ,condition/tag))
                                    `((declare (ignore condition))
                                      (return-from ,block (values))))))))
                 (declare (dynamic-extent #'.conditioner.))
                 (handler-bind
                     ((,conditions #'.conditioner.))
                   (return-from ,block ,form)))
               ,.(when error-body (list condition/tag))
               ,@(when error-body
                   `((flet ((error-message ()
                              (error-message-string ,condition/tag))
                            (error-condition ()
                              ,condition/tag))
                       (declare (ignorable #'error-message 
                                           #'error-condition)
                                (dynamic-extent #'error-message 
                                                #'error-condition))
                       (return-from ,block (progn ,@error-body))))))))))))
  
;;; ===========================================================================
;;;  With-opened-file 
;;;
;;;   Like WITH-OPEN-FILE, but sets stream to nil if a file-error occurs when
;;;   opening the file.

(defmacro with-opened-file ((stream filespec &rest options) &body body)
  (with-gensyms (abort-on-close?)
    `(let ((,stream (with-error-handling ((open ,filespec ,@options)
                                          (:conditions file-error))))
           (,abort-on-close? 't))
       (unwind-protect
           (multiple-value-prog1 
               (progn ,@body)
             (setq ,abort-on-close? nil))
         (when (streamp ,stream) (close ,stream :abort ,abort-on-close?))))))

;;; ===========================================================================
;;;  Compiler-macroexpand (for those CL's that don't provide it)

#-(or clozure cmu digitool-mcl lispworks scl)
(defun compiler-macroexpand-1 (form &optional env)
  (let ((compiler-macro-function 
         (and (consp form)
              (symbolp (car form))
              (compiler-macro-function (car form)))))
    (if compiler-macro-function
        (let ((expansion (funcall compiler-macro-function form env)))
          (values expansion (not (eq form expansion))))
        (values form nil))))

;;; ---------------------------------------------------------------------------

#-(or clozure cmu digitool-mcl lispworks scl)
(defun compiler-macroexpand (form &optional env)
  (multiple-value-bind (expansion expanded-p)
      (compiler-macroexpand-1 form env)
    (let ((expanded-at-least-once expanded-p))
      (while expanded-p
        (multiple-value-setq (expansion expanded-p)
          (compiler-macroexpand-1 expansion env)))
      (values expansion expanded-at-least-once))))

;;; ===========================================================================
;;;  Multiple-value-setf

(defmacro multiple-value-setf (places form)
  ;;; Like multiple-value-setq, but works with places.  A "place" of nil means
  ;;; to ignore the corresponding value from `form'.  Returns the primarly
  ;;; value of evaluating `form'.
  (loop 
      for place in places
      for name = (gensym)
      collect name into bindings
      if (eql 'nil place)
        unless (eq place (first places))
          collect `(declare (ignore ,name)) into ignores
        end                                         
      else
        collect `(setf ,place ,name) into body
      finally (return `(multiple-value-bind ,bindings ,form
                         ,@ignores
                         ,@body
                         ;; Return the primary value (like multiple-value-setq)
                         ,(first bindings)))))

;;; ===========================================================================
;;;  Memq and assq (lists only)

#-(or allegro
      clisp
      clozure
      cmu
      digitool-mcl
      ecl
      lispworks
      sbcl 
      scl)
(progn
  (defun memq (item list)
    (declare (list list))
    (member item list :test #'eq))
  
  (defcm memq (item list)
    `(member ,item (the list ,list) :test #'eq)))

#-(or allegro
      cmu
      lispworks
      sbcl
      scl)
(progn
  (defun assq (item list)
    (declare (list list))
    (assoc item list :test #'eq))
  
  (defcm assq (item list)
    `(assoc ,item (the list ,list) :test #'eq)))

;;; ===========================================================================
;;;  Delq and delq-one (lists only)

#+allegro
(progn
  (defun delq (item list)
    (excl::list-delete-eq item list))
  
  (defcm delq (item list)
    `(excl::list-delete-eq ,item ,list)))

#-(or allegro
      clozure
      cmu 
      digitool-mcl
      lispworks
      sbcl
      scl)
(progn
  (defun delq (item list)
    (declare (list list))
    (delete item list :test #'eq))
  
  (defcm delq (item list)
    `(delete ,item (the list ,list) :test #'eq)))

;;; ---------------------------------------------------------------------------

(defun delq-one (item list)
  (declare (list list))
  (with-full-optimization ()
    (cond
     ;; Deleting the first element:
     ((eq item (first list))
      (rest list))
     (t (let ((ptr list)
              next-ptr)
          (declare (list ptr next-ptr))
          (loop
            (unless (consp (setf next-ptr (cdr ptr)))
              (return list))
            (when (eq item (car next-ptr))
              (setf (cdr ptr) (cdr next-ptr))
              (return-from delq-one list))
            (setf ptr next-ptr)))))))

;;; ===========================================================================
;;;  Sortf & stable-sortf

(define-modify-macro sortf (place &rest args) sort)
(define-modify-macro stable-sortf (place &rest args) stable-sort)

;;; ===========================================================================
;;;  Make-hash-values-vector & sorted-maphash

(defun make-hash-values-vector (hash-table)
  ;; Return a newly allocated vector containing all values in `hash-table'.
  (let ((vector (make-array  (list (hash-table-size hash-table))
                             :fill-pointer 0)))
    (flet ((push-value (key value)
             (declare (ignore key))
             (vector-push value vector)))
      (declare (dynamic-extent #'push-value))
      (maphash #'push-value hash-table))
    vector))

;;; ---------------------------------------------------------------------------

(defun sorted-maphash (function hash-table predicate &key key)
  (let ((function (coerce function 'function))
        (vector (make-array (hash-table-count hash-table)
                            :fill-pointer 0)))
    #-sbcl
    (declare (dynamic-extent vector))
    (flet ((push-entry (key value)
             (vector-push (cons key value) vector)))
      (declare (dynamic-extent #'push-entry))
      (maphash #'push-entry hash-table)
      (flet ((do-fn (cons)
               (funcall function (car cons) (cdr cons))))
        (declare (dynamic-extent #'do-fn))
        (map nil #'do-fn
             (if key 
                 (let ((key (coerce key 'function)))
                   (declare (function key))
                   (flet ((pred (a b)
                            (funcall (the function predicate) 
                                     (funcall key (car a))
                                     (funcall key (car b)))))
                     (declare (dynamic-extent #'pred))
                     (sort vector #'pred)))
                 (flet ((pred (a b)
                          (funcall (the function predicate) (car a) (car b))))
                   (declare (dynamic-extent #'pred))
                   (sort vector #'pred))))))))

;;; ===========================================================================
;;;  Copy-file (for CLs that don't provide their own version)

#-(or allegro clisp clozure digitool-mcl ecl lispworks)
(defun copy-file (from to)
  (with-open-file (output to
                   :element-type 'unsigned-byte
                   :direction ':output
                   :if-exists ':supersede)
    (with-open-file (input from
                     :element-type 'unsigned-byte
                     :direction ':input)
      (with-full-optimization ()
        (let (byte)
          (loop (setf byte (read-byte input nil nil))
            (unless byte (return))
            (write-byte byte output)))))))

;;; ===========================================================================
;;;  Case-using

;;; We define our own CASE-USING-FAILURE condition as a subclass of the CL's
;;; own condition for CCASE and ECASE failures (all are subclasses of
;;; TYPE-ERROR):
(define-condition case-using-failure (#+allegro case-failure
                                      #+clisp simple-type-error
                                      #+clozure type-error
                                      #+cmu case-failure
                                      #+digitool-mcl type-error
                                      #+ecl case-failure
                                      #+lispworks case-failure
                                      #+sbcl case-failure
                                      #+scl case-failure
                                      #-(or allegro 
                                            clisp
                                            clozure
                                            cmu
                                            digitool-mcl
                                            ecl
                                            lispworks
                                            sbcl
                                            scl) type-error)
  ((case-using-form :reader case-using-form :initarg :case-using-form)
   (case-using-test :reader case-using-test :initarg :case-using-test))
  (:report
   (lambda (condition stream)
     (let ((case-using-form (case-using-form condition))
           (keys (rest (type-error-expected-type condition))))
       (format stream "~s fell through ~:[a~;an~] ~s ~s form; ~
                       ~[there are no valid keys~;~
                         the only valid key is~{ ~s~}~;~
                         the valid keys are~{ ~s~^ and~}~:;~
                         the valid keys are~{~#[~; and~] ~s~^,~}~]."
               (type-error-datum condition)
               (eq case-using-form 'ecase-using)
               case-using-form
               (case-using-test condition)
               (length keys)
               keys)))))

;;; ---------------------------------------------------------------------------

(defun case-using-failure (case-form exp test keys)
  ;; We add initialization for NAME and POSSIBILITIES slots for CLs that
  ;; use them in their own CASE-FAILURE condition:
  (error 'case-using-failure 
         :datum exp 
         :expected-type (cons 'member keys)
         ;; Allegro's CASE-USING slots:
         #+allegro :name #+allegro case-form
         #+allegro :possibilities #+allegro keys
         ;; CMU's CASE-USING slots:
         #+cmu :name #+cmu case-form
         #+cmu :possibilities #+cmu keys
         ;; ECL's CASE-USING slots:
         #+ecl :name #+ecl case-form
         #+ecl :possibilities #+ecl keys
         ;; Lispworks's CASE-USING slots:
         #+lispworks :name #+lispworks case-form
         #+lispworks :possibilities #+lispworks keys
         ;; SBCL's CASE-USING slots:
         #+sbcl :name #+sbcl case-form
         #+sbcl :possibilities #+sbcl keys
         ;; SCL's CASE-USING slots:
         #+scl :name #+scl case-form
         #+scl :possibilities #+scl keys
         ;; Our CASE-USING-FAILURE slots: 
         :case-using-test test
         :case-using-form case-form))

;;; ---------------------------------------------------------------------------

(defun ccase-using-failure (exp-form exp test keys)
  (restart-case  (case-using-failure 'ccase-using exp test keys)
    (store-value (value)
        :report (lambda (stream)
                  (format stream "Supply a new value for ~s." exp-form))
        :interactive (lambda ()
                       (format *query-io*
                               "~&Enter a form to evaluate as the new value for ~s: " 
                               exp-form)
                       (multiple-value-list (eval (read *query-io*))))
      (format *query-io* "~&~s is now ~s" exp-form value)
      value)))

;;; ---------------------------------------------------------------------------

(defun case-using-expander (test exp clauses ecase? ccase-tag exp-form)
  (let ((all-keys nil))
    (flet ((maybe-downgrade-test (key)
             ;; If key is a symbol, use eq when reasonable:
             (if (and (symbolp key)
                      (memq test '(eql equal equalp)))
                 'eq 
                 test)))
      (with-once-only-bindings (exp)
        `(progn
           ;; Reference exp, just in case it is never referenced in a clause:
           ,exp
           ;; Generate the clauses:
           (cond 
            ,@(flet ((do-clause (clause)
                       (destructuring-bind (keys . clause-forms) clause
                         (cond 
                          ;; no keys
                          ((not keys) 
                           `((nil ,@clause-forms)))
                          ;; otherwise clause:
                          ((eq keys 'otherwise)
                           `((t ,@clause-forms)))
                          ;; normal clause (including t clause):
                          (t `((,(cond
                                  ((atom keys)
                                   (pushnew keys all-keys :test test)
                                   `(,(maybe-downgrade-test keys) ,exp ',keys))
                                  (t `(or ,.(flet ((fn (key)
                                                     (pushnew key all-keys :test test)
                                                     `(,(maybe-downgrade-test key)
                                                       ,exp ',key)))
                                              (declare (dynamic-extent #'fn))
                                              (mapcar #'fn keys)))))
                                ,@clause-forms)))))))
                (declare (dynamic-extent #'do-clause))
                (mapcan #'do-clause clauses))
            ;; ccase error-and-go form:
            ,@(when ccase-tag
                `((t (setf ,exp-form 
                           (ccase-using-failure 
                            ',exp-form ,exp ',test ',(nreverse all-keys)))
                     (go ,ccase-tag))))
            ;; ecase error form:
            ,@(when ecase?
                `((t (case-using-failure 
                      'ecase-using ,exp ',test ',(nreverse all-keys)))))))))))
  
;;; ---------------------------------------------------------------------------

(defmacro case-using (test keyform &body clauses)
  (case-using-expander test keyform clauses nil nil nil))

(defmacro ccase-using (test keyplace &body clauses)
  (let ((tag (gensym "CUTag")))
    `(block ,tag
       (tagbody ,tag
         (return-from ,tag 
           ,(case-using-expander test keyplace clauses nil tag keyplace))))))

(defmacro ecase-using (test keyform &body clauses)
  (case-using-expander test keyform clauses 't nil nil))

;;; ===========================================================================
;;;  Extract-declarations (for CLs that don't provide their own version)

#-(or allegro)
(defun extract-declarations (body)
  ;; Return three values: 
  ;;  1. doc string, if present
  ;;  2. list of declarations
  ;;  3. remaining body
  (let ((doc-string nil)
        (declarations nil))
    (loop
      (let ((elt (first body)))
        (cond ((and (consp elt) (eq (first elt) 'declare))
               (push (pop body) declarations))
              ((and (null doc-string)
                    (stringp elt)
                    ;; to be a doc string, there must be another form in body:
                    (cdr body))
               (setf doc-string (pop body)))
              (t (return)))))
    (values doc-string (nreverse declarations) body)))

;;; ===========================================================================
;;;  Dosequence (based on James Anderson's mod of Thomas Burdick's version):

(defmacro dosequence ((var sequence &optional result) &body forms)
  (with-gensyms (end-p fun)
    `(block nil
       (flet ((,fun (,var ,end-p)
                (tagbody
                  (when ,end-p (go ,end-p))
                  ,@forms
                  (return-from ,fun nil)
                  ,end-p
                  (return-from ,fun ,result))))
         (flet ((fn (element) (,fun element nil)))
           (declare (dynamic-extent #'fn))
           (map nil #'fn ,sequence))
         ,@(when result `((,fun nil t))))))) 

;;; ===========================================================================
;;;   Dosublists (mapl-style dolist variant)

(defmacro dosublists ((var listform &optional result) &body body)
  `(do ((,var ,listform (cdr ,var)))
       ((endp ,var) ,result)
     (declare (list ,var))
     ,@body))

;;; ===========================================================================
;;;  Nicer y-or-n-p and yes-or-no-p (add initial help to Allegro & CMUCL
;;;  versions:

(defun nicer-y-or-n-p (&optional control-string &rest args)
  (declare (dynamic-extent args))
  (apply #'y-or-n-p 
         #+(or allegro cmu)
         (when control-string
           (format nil "~a[y or n] " control-string))
         #-(or allegro cmu)
         control-string
         args))

;;; ---------------------------------------------------------------------------

(defun nicer-yes-or-no-p (&optional control-string &rest args)
  (declare (dynamic-extent args))
  (apply #'yes-or-no-p 
         #+(or allegro cmu)
         (when control-string
           (format nil "~a[yes or no] " control-string))
         #-(or allegro cmu)
         control-string
         args))

;;; ===========================================================================
;;;  Nicer time macro (limit `form' display under Clozure CL & Lispworks):

(defmacro nicer-time (form)
  #+(or clozure lispworks)
  `(let ((*print-length* 1)
         (*print-level* 1))
     (time ,form))
  #-(or clozure lispworks)
  `(time ,form))

;;; ===========================================================================
;;;  Ensure-finalized-class

(defun ensure-finalized-class (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

(defcm ensure-finalized-class (class)
  (with-once-only-bindings (class)
    `(progn
       (unless (class-finalized-p ,class)
         (finalize-inheritance ,class))
       ,class)))

;;; ===========================================================================
;;;  Make-keyword

(defun make-keyword (string-symbol-or-character)
  (intern (string string-symbol-or-character)  
          (load-time-value (find-package 'keyword))))

(defcm make-keyword (string-symbol-or-character)
  `(intern (string ,string-symbol-or-character) 
           (load-time-value (find-package 'keyword))))

;;; =========================================================================== 
;;;  Ensure-list 

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defcm ensure-list (x)
  (with-once-only-bindings (x)
    `(if (listp ,x) ,x (list ,x))))

;;; ===========================================================================
;;;  Ensure-list-of-lists

(defun ensure-list-of-lists (x)
  (let ((x (ensure-list x)))
    (if (listp (car x)) x (list x))))

(defcm ensure-list-of-lists (x)
  (with-once-only-bindings (x)
    `(if (listp (car ,x)) ,x (list ,x))))

;;; ===========================================================================
;;;   Sole-element
;;;
;;;   Like first, but signals an error of more than 1 element is present
;;;   in the list.

(defun sole-element-violation (list)
  (cerror "Ignore the remaining elements."
          "The list ~s contains more than 1 element."
          list))

(defun sole-element (list)
  (prog1 (first list)
    (when (rest list)
      (sole-element-violation list))))

(defcm sole-element (list)
  (with-once-only-bindings (list)
    `(prog1 (first ,list)
       (when (rest ,list)
         (sole-element-violation ,list)))))
  
;;; ===========================================================================
;;;   Resize-hash-table
;;;
;;;   Resizes (grows) a hash table, based on `new-size' when supported by the 
;;;   CL.  As with the size argument to MAKE-HASH-TABLE, the actual new size may
;;;   be larger than the supplied value (implementation dependent).

(defun resize-hash-table (hash-table new-size)
  #+allegro
  (excl::do-rehash hash-table (excl::hash-primify new-size))
  ;; Clozure CL doesn't provide a direct interface for resizing a hash table,
  ;; so we fake such an interface by temporarily setting the REHASH-SIZE of
  ;; the hash table to `new-size' and the internal grow-threshold to zero
  ;; (allowing the resize to be performed by %GROW-HASH-TABLE).
  #+clozure
  (ccl::with-lock-context
      (ccl::without-interrupts
        (when (> new-size (hash-table-size hash-table))
          (let ((lock-free-ht? (ccl::hash-lock-free-p hash-table)))
            ;; CCL::WRITE-LOCK-HASH-TABLE doesn't work with lock-free HTs
            ;; (even though they DO have a lock), so the locking has to be
            ;; handled directly (thanks to Bill St.Clair for reporting):
            (if lock-free-ht?
                (ccl::%lock-recursive-lock-object 
                 (ccl::nhash.exclusion-lock hash-table))
                (ccl::write-lock-hash-table hash-table))
            (let ((old-rehash-size (ccl::nhash.rehash-size hash-table)))
              (unwind-protect
                  (progn
                    (setf (ccl::nhash.rehash-size hash-table) new-size)
                    (setf (ccl::nhash.grow-threshold hash-table) 0)
                    (ccl::%grow-hash-table hash-table))
                (setf (ccl::nhash.rehash-size hash-table) old-rehash-size)))
            (if lock-free-ht?
                ;; CCL::UNLOCK-HASH-TABLE doesn't work with lock-free HTs
                ;; (even though they DO have a lock), so the unlocking has to
                ;; be handled directly (thanks to Bill St.Clair for
                ;; reporting):
                (ccl::%unlock-recursive-lock-object 
                 (ccl::nhash.exclusion-lock hash-table))
                (ccl::unlock-hash-table hash-table nil)))
          't)))
  #+lispworks
  (#-lispworks6
   system:with-hash-table-locked
   #+lispworks6
   hcl:with-hash-table-locked 
   hash-table
   (when (> new-size (hash-table-size hash-table))
     (system::rehash hash-table (system::almost-primify new-size))
     't))
  ;; CMUCL doesn't provide a direct interface for resizing a hash table, so we
  ;; fake such an interface by temporarily setting the REHASH-SIZE of the hash
  ;; table to (- `new-size' old-size). 
  #+cmu
  (system:without-gcing
   (when (> new-size (hash-table-size hash-table))
     (let ((old-rehash-size (hash-table-rehash-size hash-table))
           (old-size (length (lisp::hash-table-next-vector hash-table))))
       (unwind-protect
           (progn
             (setf (slot-value hash-table 'lisp::rehash-size)
                   ;; Compute the incremental value (to be added back to
                   ;; old-size in REHASH):
                   (-& new-size old-size))
             (lisp::rehash hash-table))
        (setf (slot-value hash-table 'lisp::rehash-size) old-rehash-size)))
     't))
  ;; SBCL doesn't provide a direct interface for resizing a hash table, so we
  ;; fake such an interface by temporarily setting the REHASH-SIZE of the hash
  ;; table to (- `new-size' old-size).
  #+sbcl
  (sb-ext:with-locked-hash-table (hash-table)
    (when (> new-size (hash-table-size hash-table))
      (let ((old-rehash-size (hash-table-rehash-size hash-table))
            (old-size (length (sb-impl::hash-table-next-vector hash-table))))
        ;; SBCL's compiler (starting ~1.0.35 and continuing through at least
        ;; 1.0.54) has problems compiling the (setf slot-value) with the
        ;; UNWIND-PROTECT.  This FLET addresses that:
        (flet ((set-rehash-size (hash-table size)
                 (setf (slot-value hash-table 'sb-impl::rehash-size) size)))
          (unwind-protect
              (progn (set-rehash-size
                      hash-table 
                      ;; Compute the incremental value (to be added back to
                      ;; old-size in MAYBE-REHASH):
                      (-& new-size old-size))
                     ;; Calling REHASH directly causes problems, so we call
                     ;; MAYBE-REHASH instead with 0 free KV's:
                     (setf (sb-impl::hash-table-next-free-kv hash-table) 0)
                     (sb-impl::maybe-rehash hash-table 't))
            (set-rehash-size hash-table old-rehash-size))))
      't))
  #+scl
  (flet ((%resize ()
	   (when (> new-size (hash-table-size hash-table))
	     (let ((old-rehash-size (hash-table-rehash-size hash-table))
		   (old-size (length (lisp::hash-table-next-vector hash-table))))
	       (unwind-protect
                   (progn
                     (setf (slot-value hash-table 'lisp::rehash-size)
                           ;; Compute the incremental value (to be added back to
                           ;; old-size in REHASH):
                           (-& new-size old-size))
                     (lisp::rehash hash-table))
		 (setf (slot-value hash-table 'lisp::rehash-size) old-rehash-size)))
	     t)))
    (let ((lock (lisp::hash-table-lock hash-table)))
      (system:without-interrupts
	  (if lock
	      (thread:with-lock-held (lock "Resize hash table")
		(%resize))
	      (%resize)))))
  #-(or allegro clozure cmu lispworks sbcl scl)
  (declare (ignore hash-table new-size))
  #-(or allegro clozure cmu lispworks sbcl scl)
  (need-to-port resize-hash-table t))

;;; ===========================================================================
;;;   Shrink-vector
;;;
;;;   Destructively truncates a simple vector (when the CL implementation
;;;   supports it; CCL, CLISP, CMUCL, and SBCL return a new vector, rather
;;;   than changing the original)

(defun shrink-vector (vector length)
  #+abcl
  (system:shrink-vector vector length)
  #+allegro
  (excl::.primcall 'sys::shrink-svector vector length)
  ;; Can we do better on CLISP?
  #+clisp
  (if (=& length (length vector))
      vector
      (subseq vector 0 length))
  #+clozure
  (ccl::%shrink-vector vector length)
  #+cmu
  (lisp::shrink-vector vector length)
  #+digitool
  (ccl::%shrink-vector vector length)
  #+ecl
  (si::shrink-vector vector length)
  #+lispworks
  (system::shrink-vector$vector vector length)
  #+sbcl
  (sb-kernel:shrink-vector vector length)
  #+scl
  (lisp::shrink-vector vector length))

(defcm shrink-vector (vector length)
  #+abcl
  `(system:shrink-vector ,vector ,length)
  #+allegro
  `(excl::.primcall 'sys::shrink-svector ,vector ,length)
  #+clisp
  (with-once-only-bindings (vector length)
    `(if (=& ,length (length ,vector))
         ,vector
         (subseq ,vector 0 ,length)))
  #+clozure
  `(ccl::%shrink-vector ,vector ,length)
  #+cmu
  `(lisp::shrink-vector ,vector ,length)
  #+digitool
  `(ccl::%shrink-vector ,vector ,length)
  #+ecl
  `(si::shrink-vector ,vector ,length)
  #+lispworks
  `(system::shrink-vector$vector ,vector ,length)
  #+sbcl
  `(sb-kernel:shrink-vector ,vector ,length)
  #+scl
  `(lisp::shrink-vector ,vector ,length))

;;; ===========================================================================
;;;  Trimmed-substring

(defun trimmed-substring (character-bag string 
                          &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  ;; Allow string-designator:
  (unless (stringp string)
    (setf string (string string)))
  ;; Return extracted substring with `char-bag' trimming:
  (while (and (<& start end)
              (find (char (the simple-string string) start) character-bag))
    (incf& start))
  (decf& end)
  (while (and (<& start end)
              (find (char (the simple-string string) end) character-bag))
    (decf& end))
  (subseq string start (1+& end)))

;;; ===========================================================================
;;;  Specialized length checkers

(defun list-length-1-p (list)
  (and (consp list) (null (cdr list))))

(defcm list-length-1-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list) (null (cdr ,list)))))

;;; ---------------------------------------------------------------------------

(defun list-length-2-p (list)
  (and (consp list)
       (let ((rest (cdr list)))
         (and (consp rest)
              (null (cdr rest))))))

(defcm list-length-2-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list)
          (let ((rest (cdr, list)))
            (and (consp rest)
                 (null (cdr rest)))))))

;;; ---------------------------------------------------------------------------

(defun list-length> (n list)
  (dotimes (i (1+& n) 't)
    (declare (fixnum i))
    (unless (consp list)
      (return nil))
    (setf list (cdr (the cons list)))))

;;; ---------------------------------------------------------------------------

(defun list-length>1 (list)
  (and (consp list) (consp (cdr list))))

(defcm list-length>1 (list)
  (with-once-only-bindings (list)
    `(and (consp ,list) (consp (cdr ,list)))))

;;; ---------------------------------------------------------------------------

(defun list-length>2 (list)
  (and (consp list)
       (let ((rest (cdr list)))
         (and (consp rest)
              (consp (cdr rest))))))

(defcm list-length>2 (list)
  (with-once-only-bindings (list)
    `(and (consp ,list)
          (let ((rest (cdr, list)))
            (and (consp rest)
                 (consp (cdr rest)))))))

;;; ===========================================================================
;;;  Shuffle-list

(defun shuffle-list (list)
  (when list
    (let ((random-bound 1)
          (result (list (pop list))))
      (dolist (item list)
        (let ((position (random (incf& random-bound))))
          (if (zerop& position)
              (push item result)
              (let ((tail (nthcdr (1-& position) result)))
                (setf (cdr tail) (cons item (cdr tail)))))))
      result)))

;;; ===========================================================================
;;;  Set-equal

(defun set-equal (list1 list2 &key key
                                   (test #'eql test-supplied-p)
                                   (test-not nil test-not-supplied-p))
  ;;; Return 't if all elements in `list1' appear in `list2' (and vice
  ;;; versa).  Does not worry about duplicates in either list.
  (when (and test-supplied-p test-not-supplied-p)
    (error "Both ~s and ~s were supplied." ':test ':test-not))
  (let ((key (when key (coerce key 'function)))
        (test (if test-not 
                  (complement (coerce test-not 'function))
                  (coerce test 'function))))
    (declare (type (or function null) key)
             (type function test))
    (dolist (element list1)
      (unless (member (if key (funcall key element) element)
                      list2 :key key :test test)
        (return-from set-equal nil)))
    (dolist (element list2)
      (unless (member (if key (funcall key element) element)
                      list1 :key key :test test)
        (return-from set-equal nil)))
    ;; return success:
    't))

;;; ===========================================================================
;;;  Sets-overlap-p

(defun sets-overlap-p (list1 list2 &key key
                                        (test #'eql test-supplied-p)
                                        (test-not nil test-not-supplied-p))
  ;;; Return 't if any element in `list1' appears in `list2'. 
  ;;; Does not worry about duplicates in either list.
  (when (and test-supplied-p test-not-supplied-p)
    (error "Both ~s and ~s were supplied." ':test ':test-not))
  (let ((key (when key (coerce key 'function)))
        (test (if test-not 
                  (complement (coerce test-not 'function))
                  (coerce test 'function))))
    (declare (type (or function null) key)
             (type function test))
    (dolist (element list1)
      (when (member (if key (funcall key element) element)
                    list2 :key key :test test)
        (return-from sets-overlap-p 't)))
    ;; return failure:
    nil))

;;; ===========================================================================
;;;   XOR (imported/exported from some CL implementations)

#-allegro
(defun xor (&rest args)
  (declare (dynamic-extent args))
  (let ((result nil))
    (dolist (arg args result)
      (when arg (setf result (not result))))))

;;; ===========================================================================
;;;   Association-list extensions

(defmacro push-acons (key datum place &environment env)
  ;;; Pushes an acons of key and datum onto the place alist (whether or not a
  ;;; matching key exists in the place alist.  Returns the updated alist.
  (if (symbolp place)
      `(setf ,place (acons ,key ,datum ,place))
      (with-once-only-bindings (key datum)
        (multiple-value-bind (vars vals store-vars writer-form reader-form)
            (get-setf-expansion place env)
          `(let* (,.(mapcar #'list vars vals)
                  (,(first store-vars)
                   (acons ,key ,datum ,reader-form)))
             ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defmacro pushnew-acons (key datum place &rest keys &environment env)
  ;;; Performs a push-acons of place, key, and datum only if (assoc key place)
  ;;; returns nil.  Otherwise, datum replaces the old datum of key.  Returns
  ;;; the updated alist."
  (with-once-only-bindings (key datum)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      (with-gensyms (assoc-result)
        `(let* (,.(mapcar #'list vars vals)
                (,(first store-vars) ,reader-form)
                (,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
           (cond (,assoc-result
                  (rplacd ,assoc-result ,datum)
                  ,(first store-vars))
                 (t (setf ,(first store-vars)
                      (acons ,key ,datum ,(first store-vars)))))
           ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defun pushnew/incf-acons-expander (incf-fn-sym key incr place keys env)
  (with-once-only-bindings (key incr)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      (with-gensyms (assoc-result)
        `(let* (,.(mapcar #'list vars vals)
                (,(first store-vars) ,reader-form)
                (,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
           (cond (,assoc-result
                  (rplacd ,assoc-result 
                          (,incf-fn-sym (cdr ,assoc-result) ,incr))
                  ,(first store-vars))
                 (t (setf ,(first store-vars)
                      (acons ,key ,incr ,(first store-vars)))))
           ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf-acons (key incr place &rest keys &environment env)
  ;;; Increments the value of key by incr, if it is present; otherwise
  ;;; performs a push-acons of place, key, and incr.  Returns the updated
  ;;; alist."
  (pushnew/incf-acons-expander '+ key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf&-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+& key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$ key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf$$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$$ key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf$$$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$$$ key incr place keys env))

;;; ---------------------------------------------------------------------------
;;;  decf/delete-acons (inverse of pushnew/incf-acons)

(defun acons-not-found-error (key place)
  (error "Item ~s was not found in the alist in ~w." key place))

;;; ---------------------------------------------------------------------------

(defun decf/delete-acons-expander (decf-fn-sym key decr place keys env)
  (let ((keyword-key-value (getf keys ':key)))
    (with-once-only-bindings (key decr)
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion place env)
        (with-gensyms (assoc-result new-value)
          `(let* (,.(mapcar #'list vars vals)
                  (,(first store-vars) ,reader-form)
                  (,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
             (cond (,assoc-result
                    (let ((,new-value 
                           (,decf-fn-sym (cdr ,assoc-result) ,decr)))
                      (if (zerop ,new-value)
                          ;; Remove the acons:
                          (setf ,(first store-vars)
                                ,(if keyword-key-value
                                     `(flet ((fn (,new-value)
                                               (funcall
                                                ,keyword-key-value
                                                (car ,new-value))))
                                        (declare (dynamic-extent #'fn))
                                        (delete ,key ,(first store-vars)
                                                :key #'fn
                                                ,@(remove-property keys ':key)))
                                     `(delete ,key ,(first store-vars)
                                          :key #'car
                                          ,@(remove-property keys ':key))))
                          ;; Update the value:
                          (rplacd ,assoc-result ,new-value))))
                   (t (acons-not-found-error ,key ',place)))
             ,writer-form))))))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete-acons (key decr place &rest keys &environment env)
  ;;; Decrements the value of key by decr, if it is present; otherwise
  ;;; performs a push-acons of place, key, and decr.  Returns the updated
  ;;; alist."
  (decf/delete-acons-expander '- key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf&/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-& key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf$&/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$ key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf$/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$ key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf$$/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$$ key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf$$$/delete-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$$$ key decr place keys env))

;;; ===========================================================================
;;;   Pushnew-elements
;;;
;;;   Does a pushnew of each element in its list argument onto place

(defmacro pushnew-elements (list place &rest keys &environment env)
  (with-once-only-bindings (list)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,(first store-vars) ,reader-form))
         (dolist (element ,list)
           (pushnew element ,(first store-vars) ,@keys))
         ,writer-form))))

;;; ===========================================================================
;;;   Incf-after and decf-after
;;;
;;;   Like incf & decf, but returns the original value.

(defun incf/decf-after-builder (place inc/dec env 
                                add/sub-function incf/decf-function)
  (with-gensyms (old-value)
    (if (symbolp place)
        `(let ((,old-value ,place))
           (setf ,place (,add/sub-function ,old-value ,inc/dec))
           ,old-value)     
        (multiple-value-bind (vars vals store-vars writer-form reader-form)
            (get-setf-expansion place env)
          `(let* (,.(mapcar #'list vars vals)
                  (,(first store-vars) ,reader-form)
                  (,old-value ,(first store-vars)))
             (,incf/decf-function ,(first store-vars) ,inc/dec)
             ,writer-form
             ,old-value)))))

(defmacro incf-after (place &optional (increment 1) &environment env)
  ;;; Like incf, but returns the original value of `place' (the value before
  ;;; the incf was done)
  (incf/decf-after-builder place increment env '+ 'incf))

(defmacro decf-after (place &optional (increment 1) &environment env)
  ;;; Like decf, but returns the original value of `place' (the value before
  ;;; the decf was done)
  (incf/decf-after-builder place increment env '- 'decf))

;;; ===========================================================================
;;;   Bounded-value
;;;
;;;   Returns n bounded by min and max (type-declared versions are defined in
;;;   declared-numerics.lisp, so this definition is rarely used).

(defun bounded-value (min n max)
  (cond ((< n min) min)
        ((> n max) max)
        (t n)))

(defcm bounded-value (min n max)
  (with-once-only-bindings (min n max)
    `(cond ((< ,n ,min) ,min)
           ((> ,n ,max) ,max)
           (t ,n))))

;;; ===========================================================================
;;;   Compare and compare-strings
;;;
;;;   Three-way numeric comparison.  Returns negative fixnum (-1) if a<b;
;;;   zero if a=b; positive fixnum (1) if a>b.

(defun compare (a b)
  (cond ((< a b) -1)
        ((> a b) 1)
        (t 0)))

(defun compare-strings (a b)
  ;; We assume that the system string comparison functions are optimized
  ;; sufficiently that comparing a with b character by character is a win.
  (cond ((string< a b) -1)
        ((string> a b) 1)
        (t 0)))

;;; ===========================================================================
;;; Counted-delete
;;;
;;;   This is what DELETE should have been (and was on the LispMs).  Returns
;;;   the number of items that were deleted as a second value.

(defun counted-delete (item seq &rest args 
                       &key (test #'eql) 
                            (test-not nil test-not-supplied-p)
                       &allow-other-keys)
  (declare (dynamic-extent args))
  ;; no need to check for both test and test-not, delete should do it for us
  ;; (but doesn't in most implementations...):
  (let ((items-deleted 0)
        (test (if test-not 
                  (coerce test-not 'function) 
                  (coerce test 'function))))
    (declare (type function test))
    (flet ((new-test (a b)
             (when (funcall test a b)
               (incf& items-deleted))))
      #-gcl
      (declare (dynamic-extent #'new-test))
      (values (apply #'delete item seq 
                     (if test-not-supplied-p ':test-not ':test)
                     #'new-test 
                     args)
              items-deleted))))

;;; ===========================================================================
;;;   Dotted-length
;;;
;;;   Length function primitive for dotted lists

(defun dotted-length (list)
  (declare (list list))
  (do ((list list (cdr list))
       (i 0 (1+& i)))
      ((atom list) i)
    (declare (fixnum i))))

;;; ===========================================================================
;;;   Splitting-butlast
;;;
;;;   Butlast that returns the unused tail of the list as a second value

(defun splitting-butlast (list &optional (n 1))
  (declare (list list) (fixnum n))
  (unless (null list)
    (let ((length (dotted-length list)))
      (unless (<& length n)
        (let ((result nil))
          (dotimes (i (-& length n))
            (declare (fixnum i))
            (push (pop list) result))
          (values (nreverse result) list))))))             

;;; ===========================================================================
;;;   Remove-property
;;;
;;;   Non-destructive removal of property from a generalized plist

(defun remove-property (plist indicator)
  (do* ((ptr plist (cddr ptr))
        (ind (car ptr) (car ptr))
        (result nil))
      ;; Only when nothing was found:
      ((null ptr) plist)
    (cond ((atom (cdr ptr))
           (error "~s is a malformed property list." plist))
          ((eq ind indicator)
           (return (nreconc result (cddr ptr)))))
    (setf result (list* (second ptr) ind result))))
        
;;; ===========================================================================
;;;   Remove-properties
;;;
;;;   Non-destructive removal of properties from a generalized plist

(defun remove-properties (plist indicators)
  (cond ((null plist) nil)
        ((memq (first plist) indicators)
         (remove-properties (cddr plist) indicators))
        (t (list* (first plist) (second plist)
                  (remove-properties (cddr plist) indicators)))))
        
;;; ===========================================================================
;;;   NSorted-insert
;;;
;;;   Inserts item in list based on predicate and sort-key functions

(defun nsorted-insert (item list &optional (predicate #'<)
                                           (key #'identity))
  (let ((predicate (coerce predicate 'function))
        (key (coerce key 'function)))
    (declare (type function predicate key))
    (cond
     ;; empty list
     ((null list) (list item))
     ;; destructive insert
     (t (let ((item-key (funcall key item)))
          (cond 
           ;; handle front insertion specially
           ((funcall predicate item-key (funcall key (car list)))
            (cons item list))
           (t (do ((sublist list (cdr sublist)))
                  ((null (cdr sublist))
                   (setf (cdr sublist) (list item))
                   list)
                (when (funcall predicate 
                               item-key
                               (funcall key (cadr sublist)))
                  (setf (cdr sublist) (cons item (cdr sublist)))
                  (return list))))))))))

;;; ===========================================================================
;;;   Print-pretty-function-object

(defun print-pretty-function-object (fn &optional (stream *standard-output*))
  (let ((name (nth-value 2 (function-lambda-expression fn))))
    #+allegro
    (when (consp name) (setf name (second name)))
    #+lispworks
    (when (consp name) (setf name (third name)))
    (if name
        (print-unreadable-object (fn stream)
          (format stream "~s ~s" 'function name))
        (prin1 name stream))))

;;; ===========================================================================
;;;   Whitespace-char-p (based on standard readtable semantics)

#-(or allegro
      cmu
      lispworks
      scl)
(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\LineFeed #\Return #\Page)))

;;; ===========================================================================
;;;   Read-char immediately

(defun read-char-immediately (&optional (stream *standard-input*))
  ;;; Returns a single character keystroke from the user, unbuffered if
  ;;; possible
  
  ;; <implementation-specific versions>

  ;; for CLs without unbuffered read-char capability, throw away all but
  ;; the first character of the line (requires a <Return> by the user):
  (let ((line (handler-case (read-line stream)
                (stream-error () nil))))
    (if (plusp& (length line))
        (elt line 0)
        #\SPACE)))

;;; ===========================================================================
;;;   Add missing extract-specializer-names

#+ecl
(defun extract-specializer-names (arglist)
  ;;; Extracted from si::c-local'ed parse-specialized-lambda-list -- better
  ;;; would be to simply include extract-specializer-names in method.lsp
  (let* (parameters lambda-list specializers)
    (do ((arg (first arglist) (first arglist)))
        ((or (null arglist)
             (memq arg '(&optional &rest &key &allow-other-keys &aux))))
      (pop arglist)
      (push (if (listp arg) (first arg) arg) parameters)
      (push (if (listp arg) (first arg) arg) lambda-list)
      (push (if (listp arg) 
                (if (consp (second arg))
                    `(eql ,(eval (cadadr arg)))
                    (second arg))
                ())
            specializers))
    (when (eq (first arglist) '&optional)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
          ((or (null arglist)
               (memq arg '(&optional &rest &key &allow-other-keys &aux))))
        (pop arglist)
        (push (if (listp arg) (first arg) arg) parameters)
        (push arg lambda-list)))
    (when (eq (first arglist) '&rest)
      (push (pop arglist) lambda-list)
      (when (not (symbolp (first arglist)))
        (error "~s in the lambda-list is not a symbol."
               (first arglist)))
      (push (pop arglist) lambda-list))
    (when (eq (first arglist) '&key)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
          ((or (null arglist)
               (memq arg '(&optional &rest &key &aux))))
        (pop arglist)
        (when (eq arg '&allow-other-keys)
          (push arg lambda-list)
          (return))
        (push (if (listp arg) (first arg) arg) parameters)
        (push arg lambda-list)))
    (when (eq (first arglist) '&aux)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
          ((or (null arglist)
               (memq arg '(&optional &rest &key &allow-other-keys &aux))))
        (pop arglist)
        (push (if (listp arg) (first arg) arg) parameters)
        (push arg lambda-list)))
    (when arglist (error "The position of the lambda-list keyword ~s~%~
                          is not correct."
                         (first arglist)))
    (nreverse specializers)))

;;; ===========================================================================
;;;  Individual method removal (based on Zack Rubinstein's original version)
;;;
;;;  Note: this does not work well with some env-specific eql specializers

(defun find-and-remove-method (generic-function method-qualifiers
                               specialized-lambda-list)
  (flet ((make-specializer (name)
           (if (and (consp name) (eq (first name) 'eql))
               (intern-eql-specializer (eval (second name)))
               (find-class name))))           
    #-gcl
    (declare (dynamic-extent #'make-specializer))
    (let* ((specializer-names
            (extract-specializer-names specialized-lambda-list))
           (method-object
            (find-method generic-function 
                         (ensure-list method-qualifiers)
                         (mapcar #'make-specializer specializer-names)
                         ;; don't signal errors
                         nil)))
      (if method-object
          (remove-method generic-function method-object)
          (warn "Unable to locate method ~s ~s ~s"
                generic-function
                method-qualifiers
                specializer-names)))))

;;; ---------------------------------------------------------------------------

(flet ((method-qualifiers-p (spec)
         (or (null spec)
             (keywordp spec)
             (and (consp spec)
                  (every #'keywordp spec)))))
  (defmacro undefmethod (method-name maybe-qualifiers &rest args)
    (if (method-qualifiers-p maybe-qualifiers)
        `(find-and-remove-method 
          #',method-name ',maybe-qualifiers ',(first args))
        `(find-and-remove-method 
          #',method-name nil ',maybe-qualifiers))))

;;; ===========================================================================
;;;   Macrolet-debug

(defmacro macrolet-debug ((&rest macrobindings) &body body)
  ;;; This handy macro can help with top-level macrolet debugging.  It defines
  ;;; the local macro definitions as global macros instead (allowing quick
  ;;; macroexpansion of uses the `body' forms)
  `(progn
     ,.(flet ((fn (macro)
                `(defmacro ,@macro)))
         (declare (dynamic-extent #'fn))
         (mapcar #'fn macrobindings))
     ,@body))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


