;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/double-metaphone.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jun 22 20:12:04 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Double Metaphone (with extensions)
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Original algorithm Copyright (C) 1998, 1999 by Lawrence Philips
;;; Common Lisp version Copyright (C) 2007-2010 by Dan Corkill
;;;     <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Based on CPANs Text-DoubleMetaphone-0.07 implementation by Maurice Aubrey
;;; and a C++ version with modifications/bug fixes by Kevin Atkinson.  
;;;
;;; Dan Corkill augmented this Common Lisp version with the `extended-p'
;;; extensions.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-04-07 File Created.  (Corkill)
;;;  04-02-08 Added non-strict B/T separation extention.  (Corkill)
;;;  02-16-10 Added more extended-p extensions.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(double-metaphone)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun vowelp (string pos)
    (declare (type simple-base-string string)
             (fixnum pos))
    (when (<=& 0 pos (length string))
      ;; optimized (member (char string pos) '(#\E #\A #\O #\I #\U #\Y)):
      (let ((char (char string pos)))
        (or (eql char #\E)
            (eql char #\A)
            (eql char #\O)
            (eql char #\I)
            (eql char #\U)
            (eql char #\Y))))))

;;; ---------------------------------------------------------------------------

(defun char-at (string index candidate-char)
  (declare (type simple-base-string string))
  (eql (char string index) candidate-char))

(defcm char-at (string index candidate-char)
  `(eql (char ,string ,index) ,candidate-char))

;;; ---------------------------------------------------------------------------

(defun char-member-at (string index &rest candidate-chars)
  (declare (type simple-base-string string)
           (dynamic-extent candidate-chars))
  (member (char string index) candidate-chars))

(defcm char-member-at (string index &rest candidate-chars)
  `(let ((.char. (char ,string ,index)))
     (or ,@(loop for candidate-char in candidate-chars 
               collect `(eql .char. ,candidate-char)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun string-at (string start &rest candidate-strings)
    (declare (type simple-base-string string)
             (fixnum start)
             (dynamic-extent candidate-strings))
    (when (>=& start 0)
      (dolist (candidate candidate-strings)
        (declare (type simple-base-string candidate))
        (when (string= string candidate 
                       :start1 start
                       :end1 (min& (length string)
                                   (+& start (length candidate))))
          (return-from string-at candidate))))))

;;; ===========================================================================
;;;  Double-metaphone (string [extended-p]) => primary-key; [secondary-key]
;;;
;;; When matching string1 & string2:
;;;   Strong:
;;;     primary-key1 = primary-key2
;;;   Normal:
;;;     primary-key1 = secondary-key2
;;;     secondary-key1 = primary-key2
;;;   Weak:
;;;     secondary-key1 = secondary-key2

(defun double-metaphone (string &optional extended-p)
  (let* ((length (length string))
         (ustring (make-string (+& length 2) ; include room for 2 pad spaces
                               :element-type 'base-char))) 
    ;; Stack-allocate ustring, where possible (avoid SBCL's warning about not
    ;; being able to stack allocate):
    #-sbcl
    (declare (dynamic-extent ustring))
    ;; Upcase `string' into USTRING and add two ending spaces:
    (map-into ustring #'char-upcase string)
    (setf (char ustring length) #\Space)
    (setf (char ustring (1+& length)) #\Space)
    ;; Do the real work:
    (%double-metaphone ustring (length string) extended-p)))

;;; ---------------------------------------------------------------------------

(defun %double-metaphone (ustring length extended-p)
  ;; Do the real work on the upcased & padded ustring:
  (let ((current 0)
         (last (1-& length))
         (primary (make-array '(4) 
                              :fill-pointer 0 
                              :element-type 'base-char))
         (secondary (make-array '(4) 
                                :fill-pointer 0 
                                :element-type 'base-char))
	 (slavo-germanic-value ':needed))
    ;; Stack-allocate primary & secondary, where possible (avoid SBCL's
    ;; warning about not being able to stack allocate):
    #-sbcl
    (declare (dynamic-extent primary secondary))
    (labels ((add-to (array add)
               (etypecase add
                 (null)
                 (character (vector-push add array))
                 (string (loop for char across add
                             do (vector-push char array)))))
             (metaph-add (primary-add 
                          &optional (secondary-add 
                                     primary-add
                                     secondary-supplied-p))
               (cond
                ;; Adding both a primary and secondary:
                (secondary-supplied-p
                 ;; Copy primary, if this is the first secondary being added
                 ;; and we've not exceeded the encoded-string length:
                 (let ((fill-pointer-primary (fill-pointer primary)))
                   (unless (=& fill-pointer-primary
                               (array-total-size primary))
                     (when (and (zerop& (fill-pointer secondary))
                                (plusp& fill-pointer-primary))
                       (loop for char across primary do
                             (vector-push char secondary)))
                     ;; Add to each (unless primary is nil):
                     (when primary 
                       (add-to primary primary-add))
                     (add-to secondary secondary-add))))
                ;; Only a primary was supplied:
                (t (add-to primary primary-add)
                   ;; also add primary to secondary, if we have one:
                   (when (plusp& (fill-pointer secondary))
                     (add-to secondary primary-add)))))
	     (slavo-germanic-p ()
	       (if (eq slavo-germanic-value ':needed)
  		   ;; determine and save the value:
  		   (setf slavo-germanic-value
		     ;; Search for "W", "K", "CZ", or "WITZ":
		     (or (find #\W ustring)
			 (find #\K ustring)
			 (search "CZ" ustring)
			 (search "WITZ" ustring)))
		 ;; use saved value:
		 slavo-germanic-value)))

      ;; Skip "silent" first-character of these if at start of word:
      (when (string-at ustring 0 "GN" "KN" "PN" "WR" "PS")
        (incf& current))
      
      ;; Initial "X" is pronounced "Z" (e.g., "XAVIER"), mapped to "S":
      (when (char-at ustring 0 #\X)
        (metaph-add #\S)
        (incf& current))
      
      ;; Main loop:
      (while (or (<& (length primary) 4)
                 (<& (length secondary) 4))
        (when (>=& current length)
          (return))
        
        (case (char ustring current)

          ;; The following case clauses are ordered by approximate English occurance
          ;; frequencies, although some CLs (Allegro, CMUCL, SBCL, and CLISP?)
          ;; perform offset dispatch case optimizations that might make the
          ;; integer difference of the char code from #\A's slightly faster.

          ;; ------------------------------------------------------------------
          ;; Approximate frequency ordering, handling all vowels first:

          ((#\E #\A #\O #\I #\U #\Y)
           ;; All initial vowels map to "A":
           (when (zerop& current)
             (metaph-add #\A))
           (incf& current))

          ;; ------------------------------------------------------------------

          (#\T
           (cond
            ((string-at ustring current "TION" "TIA" "TCH")
             (metaph-add #\X)
             (incf& current 3))

            ((string-at ustring current "TH" "TTH")
             (cond
              ;; special case "THOMAS", "THAMES" or Germanic:
              ((or (string-at ustring (+& current 2) "OM" "AM")
                   (string-at ustring 0 "VAN " "VON " "SCH"))
               (metaph-add #\T))
              (t (metaph-add #\0 #\T)))	; yes, primary is zero! Some
					; implementations use asterisk in
					; place of zero ...
             (incf& current 2))

            (t (metaph-add #\T)
               (incf& current
                      (if (char-member-at ustring (+& current 1) #\T #\D) 
                          2 
                          1)))))
         
          ;; ------------------------------------------------------------------

          (#\N
           (metaph-add #\N)
           (incf& current (if (char-at ustring (+& current 1) #\N) 2 1)))
         
          ;; ------------------------------------------------------------------

          (#\S
           (cond
            ;; special cases: "ISLAND", "ISLE", "CARLISLE", "CARLYSLE":
            ((string-at ustring (-& current 1) "ISL" "YSL")
             (incf& current 1))
          
            ;; special case: "SUGAR-":
            ((and (zerop& current)
                  (string-at ustring current "SUGAR"))
             (metaph-add #\X #\S)
             (incf& current 1))
          
            ((string-at ustring current "SH")
             (cond
              ;; Germanic:
              ((string-at ustring (+& current 1) "HEIM" "HOEK" "HOLM" "HOLZ")
               (metaph-add #\S)
               (incf& current 2))
              (t (metaph-add #\X)
                 (incf& current 2))))
          
            ;; Italian & Armenian:
            ((string-at ustring current "SIO" "SIA") ; also covers "SIAN"
             (cond
              ((not (slavo-germanic-p))
               (metaph-add #\S #\X))
              (t (metaph-add #\S)))
             (incf& current 3))
          
            ;; German & Anglicisations: e.g., "SMITH" match "SCHMIDT",
            ;; "SNIDER" match "schneider"; also, "-SZ-" in Slavic language
            ;; although in Hungarian it is pronounced "S":
            ((or (and (zerop& current)
                      (char-member-at ustring (+& current 1) #\M #\N #\L #\W))
                 (char-at ustring (+& current 1) #\Z))
             (metaph-add #\S #\X)
             (incf& current (if (char-at ustring (+& current 1) #\Z) 2 1)))
          
            ((string-at ustring current "SC")
             (cond
              ;; Schlesinger's rule:
              ((char-at ustring (+& current 2) #\H)
               (cond
                ;; Dutch origin: e.g., "SCHOOL", "SCHOONER":
                ((string-at ustring (+& current 3) 
                            "OO" "ER" "EN" "UY" "ED" "EM")
                 (cond 
                  ;; "SCHERMERHORN", "SCHENKER":
                  ((string-at ustring (+& current 3) "ER" "EN")
                   (metaph-add #\X "SK"))
                  (t (metaph-add "SK")))
                 (incf& current 3))

                ((and (zerop& current)
                      (not (vowelp ustring 3))
                      (not (char-at ustring 3 #\W)))
                 (metaph-add #\X #\S)
                 (incf& current 3))
              
                (t (metaph-add #\X)
                   (incf& current 3))))
            
              ((char-member-at ustring (+& current 2) #\I #\E #\Y)
               (metaph-add #\S)
               (incf& current 3))
            
              (t (metaph-add "SK")
                 (incf& current 3))))
          
            ;; French: e.g., "RESNAIS", "ARTOIS":
            ((and (=& current last)
                  (string-at ustring (-& current 2) "AI" "OI"))
             (metaph-add nil #\S)
             (incf& current 
                    (if (char-member-at ustring (+& current 1) #\S #\Z) 2 1)))
          
            (t (metaph-add #\S)
               (incf& current 
                      (if (char-member-at ustring (+& current 1) #\S #\Z)
                          2 
                          1)))))

          ;; ------------------------------------------------------------------

          (#\H
           (cond
            ;; only keep if first & before vowel or between 2 vowels:
            ((and (or (zerop& current)
		      (vowelp ustring (-& current 1)))
		  (vowelp ustring (+& current 1)))
	     (metaph-add #\H)
             (incf& current 2))
            ;; ...also takes care of "HH":
            (t (incf& current 1))))

          ;; ------------------------------------------------------------------

          (#\R
           (cond 
            ;; French: e.g., "ROGIER", but exclude "HOCHMEIER":
            ((and (=& current last)
                  (not (slavo-germanic-p))
                  (string-at ustring (-& current 2) "IE")
                  (not (string-at ustring (-& current 4) "ME" "MA")))
             (metaph-add nil #\R))
            (t (metaph-add #\R)))
           (incf& current (if (char-at ustring (+& current 1) #\R) 2 1)))

          ;; ------------------------------------------------------------------

          (#\D
           (cond
            ((string-at ustring current "DG")
             (cond
              ;; E.g., "EDGE":
              ((char-member-at ustring (+& current 2) #\I #\E #\Y)
               (metaph-add #\J)
               (incf& current 3))
              ;; E.g., "EDGAR":
              (t (metaph-add "TK")
                 (incf& current 2))))
            ((string-at ustring current "DT" "DD")
             (if extended-p 
                 (metaph-add #\D)
                 (metaph-add #\T))
             (incf& current 2))          
            (t (if extended-p 
                   (metaph-add #\D)
                   (metaph-add #\T))
               (incf& current 1))))
        
          ;; ------------------------------------------------------------------

          (#\L
           (cond
            ;; Spanish: e.g., "CABRILLO", "GALLEGOS":
            ((char-at ustring (+& current 1) #\L)
             (cond
              ((or (and (=& current (-& length 3))
                        (string-at ustring (-& current 1)
                                   "ILLO" "ILLA" "ALLE"))
                   (and (or (string-at ustring (-& last 1) "AS" "OS")
                            (char-member-at ustring last #\A #\O))
                        (string-at ustring (-& current 1) "ALLE")))
               (metaph-add #\L nil)
               (incf& current 2))
              (t (metaph-add #\L)
                 (incf& current 2))))
            (t (metaph-add #\L)
               (incf& current 1))))

          ;; ------------------------------------------------------------------
          
          (#\C
           (cond
            ;; Various Germanic:
            ((or (and (>& current 1)
                      (not (vowelp ustring (-& current 2)))
                      (string-at ustring (-& current 1) "ACH")
                      (not (char-member-at ustring (+& current 2) #\I #\E)))
                 (string-at ustring (-& current 2) "BACHER" "MACHER"))
             (metaph-add #\K)
             (incf& current 2))
            
            ;; special case "CAESAR":
            ((and (zerop& current)
                  (string-at ustring current "CAESAR"))
             (metaph-add #\S)
             (incf& current 2))
            
            ;; Italian "CHIANTI":
            ((string-at ustring current "CHIA")
             (metaph-add #\K)
             (incf& current 2))
            
            ;; "CH" processing:
            ((string-at ustring current "CH")
             (cond 
              ;; "MICHAEL":
              ((and (plusp& current)
                    (string-at ustring current "CHAE"))
               (metaph-add #\X)
               (incf& current 2))
              
              ;; Greek roots (e.g., "CHEMISTRY", "CHORUS":
              ((and (zerop& current)
                    (or (string-at ustring (+& current 1) 
				   "HARAC" "HARIS" "HOR" "HYM" "HIA" "HEM"))
                    (not (string-at ustring 0 "CHORE")))
               (metaph-add #\K)
               (incf& current 2))
              
              ;; Germanic, Greek, or otherwise: "CH" for "KH" sound:
              ((or (string-at ustring 0 "VAN " "VON " "SCH")
                   ;; "ARCHITECT" but not "ARCH", "ORCHESTRA", "ORCHID":
                   (string-at ustring (-& current 2)
                              "ORCHES" "ARCHIT" "ORCHID")
                   (char-member-at ustring (+& current 2) #\T #\S)
                   ;; "WACHTLER", "WECHSLER", but not "TICHNER":
                   (and (or (zerop& current) 
			    (char-member-at ustring (-& current 1)  #\A #\O #\U #\E))
                        (char-member-at ustring (+& current 2)
                                        #\L #\R #\N #\M #\B #\H #\F #\V #\W #\Space)))
               (metaph-add #\K)
               (incf& current 2))
              
              ((plusp& current)
               (cond 
                ;; "MCHUGH":
                ((string-at ustring 0 "MC")
                 (metaph-add #\K)
                 (incf& current 2))
                (t
                 (metaph-add #\X #\K)
                 (incf& current 2))))
              
              (t 
               (metaph-add #\X)
               (incf& current 2))))
            
            ;; "CZERNY":
            ((and (string-at ustring current "CZ")
                  (not (string-at ustring (-& current 2) "WICZ"))
                  (metaph-add #\S #\X)
                  (incf& current 2)))
            
            ;; "FOCACCIA":
            ((string-at ustring (+& current 1) "CIA")
             (metaph-add #\X)
             (incf& current 3))
            
            ;; Double "C", but not if e.g., "MCCLELLAN":
            ((and (string-at ustring current "CC")
                  (not (and (/=& current 1)
                            (char-at ustring 0 #\M))))
             (cond
              ;; "BELLOCCHIO" but not "BACCHUS":
              ((and (char-member-at ustring (+& current 2) #\I #\E #\H)
                    (not (string-at ustring (+& current 2) "HU")))
               (cond 
                ;; "ACCIDENT", "ACCEDE", "SUCCEED":
                ((or (and (=& current 1)
                          (char-at ustring (-& current 1) #\A))
                     (string-at ustring (-& current 1) "UCCEE" "UCCES"))
                 (metaph-add "KS")
                 (incf& current 3))
                ;; "BACCI", "BERTUCCI", other Italian:
                (t 
                 (metaph-add #\X)
                 (incf& current 3))))
              ;; Pierce's rule:
              (t (metaph-add #\K)
                 (incf& current 2))))
          
            ((string-at ustring current "CK" "CG" "CQ")
             (metaph-add #\K)
             (incf& current 2))
          
            ((string-at ustring current "CI" "CE" "CY")
             (cond
              ;; Italian:
              ((string-at ustring current "CIO" "CIE" "CIA")
               (metaph-add #\S #\X)
               (incf& current 2))
              ;; English:
              (t (metaph-add #\S)
                 (incf& current 2))))
          
            ;; Everything else:
            (t (metaph-add #\K)
               (incf& current
                      (cond 
                       ;; Name such as "MAC CAFFREY", "MAC GREGOR":
                       ((string-at ustring (+& current 1) " C" " Q" " G")
                        3)
                       ((and (char-member-at ustring (+& current 1) #\C #\K #\Q)
                             (not (string-at ustring (+& current 1)
                                             "CE" "CI")))
                        2)
                       (t 1))))))
        
          ;; ------------------------------------------------------------------

          (#\M
           (metaph-add #\M)
           (incf& current
                  (cond
                   ;; "DUMB", "THUMB":
                   ((or (and (string-at ustring (-& current 1) "UMB")
                             (or (=& (+& current 1) last)
                                 (string-at ustring (+& current 2) "ER")))
                        (char-at ustring (+& current 1) #\M))
                    2)
                   (t 1))))

          ;; ------------------------------------------------------------------
        
          (#\W
           (cond
            ;; "W" can also be in the middle of a word:
            ((string-at ustring current "WR")
             (metaph-add #\R)
             (incf& current 2))
          
            ((or (and (zerop& current)
                      (vowelp ustring (+& current 1)))
                 (string-at ustring current "WH"))
             (cond
              ;; "WASSERMAN" should match "VASSERMAN":
              ((vowelp ustring (+& current 1))
               (metaph-add #\A #\F))
              ;; need "UOMO" to match "WOMO":
              (t (metaph-add #\A)))
             (incf& current 1))
          
            ;; "ARNOW" should match "ARNOFF":
            ((or (and (=& current last)
                      (vowelp ustring (-& current 1)))
                 (string-at ustring (-& current 1) 
                            "EWSKI" "EWSKY" "OWSKI" "OWSKY")
                 (string-at ustring 0 "SCH"))
             (metaph-add nil #\F)
             (incf& current 1))
          
            ;; Polish: e.g., "FILIPOWICZ":
            ((string-at ustring current "WICZ" "WITZ")
             (metaph-add "TS" "FX")
             (incf& current 4))
          
            ;; Otherwise, just skip the "W":
            ((incf& current 1))))
        
          ;; ------------------------------------------------------------------
        
          (#\F
           (metaph-add #\F)
           (incf& current (if (char-at ustring (+& current 1) #\F) 2 1)))
        
          ;; ------------------------------------------------------------------

          (#\G
           (cond
	    ;; "-GH-"
            ((char-at ustring (+& current 1) #\H)
             (cond
              ((and (plusp& current) 
                    (not (vowelp ustring (-& current 1))))
               (metaph-add #\K)
               (incf& current 2))
	      ;; "GHISLANE", "GHIRADELLI":
	      ((zerop& current)
	       (cond
		((char-at ustring (+& current 2) #\I)
		 (metaph-add #\J)
		 (incf& current 2))
		(t (metaph-add #\K)
		   (incf& current 2))))

	      ;; Parker's rule (with some further refinements); e.g., "HUGH":
              ((or (and (>& current 1)
                        (char-member-at ustring (-& current 2) #\B #\H #\D))
                   ;; E.g., "BOUGH":
                   (and (>& current 2)
                        (char-member-at ustring (-& current 3) #\B #\H #\D))
                   ;; E.g., "BROUGHTON":
                   (and (>& current 3)
                        (char-member-at ustring (-& current 4) #\B #\H)))
               (incf& current 2))

              ;; E.g., "LAUGH", "MCLAUGHLIN", "COUGH", "GOUGH", "ROUGH",
              ;; "TOUGH":
              ((and (>& current 2)
                    (char-at ustring (-& current 1) #\U)
                    (char-member-at ustring (-& current 3)
                                    #\C #\G #\L #\R #\T))
               (metaph-add #\F)
               (incf& current 2))
            
              ((and (plusp& current)
                    (not (char-at ustring (-& current 1) #\I)))
               (metaph-add #\K)
               (incf& current 2))
	      
	      (t (incf& current 2))))

            ;; "-GN-"
	    ((char-at ustring (+& current 1) #\N)
	     (cond
	      ((and (=& current 1)
		    (vowelp ustring 0)
		    (not (slavo-germanic-p)))
	       (metaph-add "KN" #\N)
	       (incf& current 2))
	      ;; Not e.g., "CAGNEY":
	      ((and (not (string-at ustring (+& current 2) "EY"))
		    (not (char-at ustring (+& current 1) #\Y))
		    (not (slavo-germanic-p)))
	       (metaph-add #\N "KN")
	       (incf& current 2))
	      (t (metaph-add "KN")
		 (incf& current 2))))
            
	    ;; "TAGLIARO":
	    ((and (string-at ustring (+& current 1) "LI")
		  (not (slavo-germanic-p)))
	     (metaph-add "KL" #\L)
	     (incf& current 2))

	    ;; -GES-, -GEP-, -GEL-, -GIE- at beginning:
	    ((and (zerop& current)
		  (or (char-at ustring (+& current 1) #\Y)
		      (string-at ustring (+& current 1) 
				 "ES" "EP" "EB" "EL" "EY" "IB" "IL" 
				 "IN" "IE" "EI" "ER")))
	     (metaph-add #\K #\J)
	     (incf& current 2))

	    ;; -GER-, -GY-:
	    ((and (or (string-at ustring (+& current 1) "ER")
		      (char-at ustring (+& current 1) #\Y))
		  (not (string-at ustring 0 "DANGER" "RANGER" "MANGER"))
		  (not (string-at ustring (-& current 1) "E" "I" "RGY" "OGY")))
	     (metaph-add #\K #\J)
	     (incf& current 2))

	    ;; Italian; e.g, "BIAGGI":
	    ((or (char-member-at ustring (+& current 1) #\E #\I #\Y)
		 (string-at ustring (-& current 1) "AGGI" "OGGI"))
	     (cond
	      ;;obvious Germanic:
	      ((or (string-at ustring 0 "VAN " "VON " "SCH")
		   (string-at ustring (+& current 1) "ET"))
	       (metaph-add #\K)
	       (incf& current 2))
	      ;; always soft if French ending:
	      ((string-at ustring (+& current 1) "IER ")
	       (metaph-add #\J)
	       (incf& current 2))
	      (t (metaph-add #\J #\K)
		 (incf& current 2))))

	    ;; Everything else:
            (t (metaph-add #\K)
               (incf& current
                      (if (char-at ustring (+& current 1) #\G) 2 1)))))
         
          ;; ------------------------------------------------------------------

          (#\P
           (cond
            ;; "-PH":
            ((char-at ustring (+& current 1) #\H)
             (metaph-add #\F)
             (incf& current 2))
          
            ;; Also account for "CAMPBELL", "RASPBERRY":
            ((char-member-at ustring (+& current 1) #\P #\B)
             (metaph-add (if (and extended-p
                                  (char-at ustring (+& current 1) #\B))
                             #\B 
                             #\P))
             (incf& current 2))
          
            (t (metaph-add #\P)
               (incf& current 1))))

          ;; ------------------------------------------------------------------
          
          (#\B
           ;; "-mb", e.g., "dumb", already skipped over...
           (if extended-p
               (metaph-add #\B)
               (metaph-add #\P))
           (incf& current (if (char-at ustring (+& current 1) #\B) 2 1)))
          
          ;; ------------------------------------------------------------------

          (#\V
           (if extended-p
               (metaph-add #\V)
               (metaph-add #\F))
           (incf& current (if (char-at ustring (+& current 1) #\V) 2 1)))

          ;; ------------------------------------------------------------------

          (#\K
           (metaph-add #\K)
           (incf& current (if (char-at ustring (+& current 1) #\K) 2 1)))

          ;; ------------------------------------------------------------------

          (#\J
           (cond
            ;; obvious Spanish: "JOSE", "SAN JACINTO":
            ((or (string-at ustring current "JOSE")
                 (string-at ustring 0 "SAN "))
             (cond
              ((or (and (zerop& current)
                        (char-at ustring (+& current 4) #\Space))
                   (string-at ustring 0 "SAN "))
               (metaph-add #\H))
              (t (metaph-add #\J #\H)))
             (incf& current 1))

            ((and (zerop& current)
                  (not (string-at ustring current "JOSE")))
             ;; "J" as in "YANKELOVICH"/"JANKELOWICZ":
             (metaph-add #\J #\A)
             (incf& current (if (char-at ustring (+& current 1) #\J) 2 1)))
          
            ;; Spanish pronounciation of e.g., "BAJADOR":
            ((and (vowelp ustring (-& current 1))
                  (not (slavo-germanic-p))
                  (char-member-at ustring (+& current 1) #\A #\O))
             (metaph-add #\J #\H)
             (incf& current (if (char-at ustring (+& current 1) #\J) 2 1)))

            ((=& current last)
             (metaph-add #\J nil)
             (incf& current (if (char-at ustring (+& current 1) #\J) 2 1)))

            ((and (not (char-member-at ustring (+& current 1)
                                       #\L #\T #\K #\S #\N #\M #\B #\Z))
                  (not (char-member-at ustring (-& current 1) #\S #\K #\L)))
             (metaph-add #\J)
             (incf& current (if (char-at ustring (+& current 1) #\J) 2 1)))
          
            ;; It could happen!
            (t (incf& current
                      (if (char-at ustring (+& current 1) #\J) 2 1)))))

          ;; ------------------------------------------------------------------
        
          (#\X
           ;; French: e.g., "BREAUX":
           (unless (and (=& current last)
                        (or
                          (string-at ustring (-& current 3) "IAU" "EAU")
                          (string-at ustring (-& current 2) "AU" "OU")))
             (metaph-add "KS"))
           (incf& current
                  (if (char-member-at ustring (+& current 1) #\C #\X) 2 1)))

          ;; ------------------------------------------------------------------

          (#\Q
           (metaph-add #\K)
           (incf& current (if (char-at ustring (+& current 1) #\Q) 2 1)))

          ;; ------------------------------------------------------------------

          (#\Z
           (cond
            ;; Chinese Pinyin: e.g., "ZHAO":          
          
            ((char-at ustring (+& current 1) #\H)
             (metaph-add #\J)
             (incf& current 2))
          
            ((or (string-at ustring (+& current 1) "ZO" "ZI" "ZA")
                 (and (plusp& current)
                      (slavo-germanic-p)
                      (not (char-at ustring (-& current 1) #\T))))
             (metaph-add #\S "TS")
             (incf& current (if (char-at ustring (+& current 1) #\Z) 2 1)))
          
            (t (if extended-p
                   (metaph-add #\Z)
                   (metaph-add #\S))
               (incf& current 
                      (if (char-at ustring (+& current 1) #\Z) 2 1)))))
        
          ;; ------------------------------------------------------------------
          ;; #\Ç is "converted" by Tortoise SVN on Windows, so we avoid that
          ;; by specifying it via CODE-CHAR
          
          (#.(code-char 199) 
           (metaph-add #\S)
           (incf& current))

          ;; ------------------------------------------------------------------

          (#.(code-char 209) ;; #\Ñ is "converted" by Tortoise SVN on Windows
           (metaph-add #\N)
           (incf& current 1))
         
          ;; ------------------------------------------------------------------
        
          (otherwise
           (incf& current 1))))
      
      (setf primary (coerce primary 'simple-base-string))
      (if (plusp& (fill-pointer secondary))
          (values primary (coerce secondary 'simple-base-string))
          primary))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
