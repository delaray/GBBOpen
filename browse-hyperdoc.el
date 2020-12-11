;;;; -*- Mode:Emacs-Lisp -*-
;;;; *-* File: /usr/local/gbbopen/browse-hyperdoc.el *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jan 28 05:24:48 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *             Emacs Interface to GBBopen HyperDoc Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; To enable on-line GBBopen hyperdoc access, the GBBopen hyperdoc
;;; files must be installed in <install-directory>/hyperdoc.  Note
;;; that the hyperdoc files are not included in a checkout from the
;;; GBBopen source code repository or in the GBBopen source-repository
;;; snapshot archive. A separate GBBopen hyperdoc archive is
;;; maintained at:
;;;    http://GBBopen.org/downloads/GBBopen-hyperdoc.tar.gz
;;; Download and extract the files in this archive into <install-directory>.
;;; 
;;; To maintain the lastest documentation additions and revisions, your copy
;;; of the hyperdoc files should be refreshed periodically, such as when
;;; updating your GBBopen sources.  (Using svn update will keep both the
;;; GBBopen sources and hyperdoc entries up-to-date and consistent.)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-28-05 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(require 'cl)
(require 'browse-url) ;; In Emacs 20 and beyond...
(require 'thingatpt)

;;; ---------------------------------------------------------------------------

(defconst hyperdoc-directory 
  (format "%shyperdoc" (file-name-directory load-file-name)))

;;; ---------------------------------------------------------------------------

(defun browse-hyperdoc (string)
  (interactive 
   (cond
    ;; Allegro CL's ELI grabber:
    ((fboundp 'fi::get-default-symbol)
     (fi::get-default-symbol "Entity: " t t))
    ;; Slime grabber:
    ((fboundp 'slime-symbol-name-at-point)
     (let ((default (slime-symbol-name-at-point)))
       (list (read-string "Entity: " default))))
    ;; Thing-at-point grabber:
    ((fboundp 'thing-at-point)
     (let ((default (thing-at-point 'symbol)))
       (list (read-string "Entity: " default))))
    ;; Just prompt:
    (t (list (read-string "Entity: ")))))
  ;; Handle GBBopen hyperdoc filename adjustments:
  (let ((basename (downcase string)))
    (setq basename
          (cond 
           ;; Special entity names:
           ((string= basename "*%")
            "multiply-pprob")
           ((string= basename "/%")
            "divide-pprob")
           ((string= basename "ln%")
            "ln-pprob")
           ((string= basename "exp%")
            "exp-pprob")
           ;; Global variables:
           ((char-equal ?* (elt basename 0))
            (format "%s-var" 
                    (substring basename 1 (1- (length basename)))))
           ;; Keyword variables:
           ((char-equal ?: (elt basename 0))
            (substring basename 1))
           ;; Prepare for REPLACE-REGEXP-IN-STRING:
           (t (copy-sequence basename))))
    ;; Convert basename for GBBopen entities with slash characters:
    (setq basename (replace-regexp-in-string "/" "-" basename t t))
    ;; and with > characters:
    (setq basename (replace-regexp-in-string ">" "-gt-" basename t t))
    ;; and with = characters:
    (setq basename (replace-regexp-in-string "=" "-eq-" basename t t))
    ;; and with & characters:
    (setq basename (replace-regexp-in-string "&" "-amp-" basename t t))
    ;; Fixup any trailing hyphen introduced above:
    (let ((last-pos (1- (length basename))))
      (when (char-equal ?- (elt basename last-pos))
        (setf basename (substring basename 0 last-pos))))
    ;; Construct GBBopen hyperdoc filename:
    (let ((filename
           (format "%s/ref-%s.html" hyperdoc-directory basename)))
      (cond
       ;; GBBopen entity:
       ((file-exists-p filename)
        (let ((browse-url-new-window-flag t))
          (browse-url (concat "file\:" filename))))
       ;; Defer to the Common Lisp Hyperspec, if available:
       ((fboundp 'common-lisp-hyperspec)
        (common-lisp-hyperspec string))
       ;; Failed to find either GBBopen or CL entity:
       (t (message "No hyperdoc found for %s" string))))))

;;; ---------------------------------------------------------------------------

(global-set-key "\M-\?" 'browse-hyperdoc)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
