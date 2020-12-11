;;;; -*- Mode:Common-Lisp; Package:PORTABLE-SOCKETS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/portable-sockets.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr  3 14:55:17 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Portable Socket Interface
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2011, Dan Corkill <corkill@GBBopen.org>
;;;
;;; Developed and supported by the GBBopen Project (http://GBBopen.org) and
;;; licenced under the Apache 2.0 license (see
;;; http://GBBopen.org/downloads/LICENSE for license details.)
;;;
;;; Bug reports, suggestions, enhancements, and extensions should be sent to
;;; corkill@GBBopen.org.
;;;
;;; On-line documentation for these portable sockets interface entities is
;;; available at http://gbbopen.org/hyperdoc/index.html
;;;
;;; This file requires the GBBopen Portable Threads Interface file
;;; (portable-threads.lisp), but is otherwise self-contained on the supported
;;; CLs.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  08-02-05 File created.  (Corkill)
;;;  10-20-05 Make open-connection a generic-function.  (Corkill)
;;;  01-02-06 Rename :address keyword to :interface.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':portable-sockets)
    (make-package ':portable-sockets
                  :use '(:common-lisp #-ecl :portable-threads))))

(in-package :portable-sockets)

;;; ---------------------------------------------------------------------------
;;; Handle older CLISP versions

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (= (length (ext:arglist 'socket:socket-server)) 2)
    (pushnew ':old-clisp-version *features*)))

;;; ---------------------------------------------------------------------------
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (both Digitool MCL and pre-1.2 Clozure CL include the
;;; feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':digitool-mcl *features*))

;;; ---------------------------------------------------------------------------
;;; Add clozure feature to legacy OpenMCL:

#+(and openmcl (not clozure))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':clozure *features*))

;;; ===========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro
  (require :sock)
  #+digitool-mcl
  (require :opentransport)
  ;; ECL must be built using ./configure --enable-threads:
  #+ecl
  (require 'sockets)
  #+lispworks
  (require "comm")
  #+sbcl
  (require :sb-bsd-sockets))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*localhost*                 ; not documented
            accept-connection
            close-passive-socket
            local-hostname-and-port
            make-passive-socket
            open-connection
            remote-hostname-and-port
            shutdown-socket-stream
            start-connection-server
            with-open-connection
            write-crlf)))               ; not yet documented

;;; ===========================================================================

(defun portable-sockets-implementation-version ()
  "1.0")

;;; Added to *features* at the end of this file:
(defparameter *portable-sockets-version-keyword* 
    ;; Support cross-case mode CLs:
    (read-from-string (format nil ":portable-sockets-~a" 
                              (portable-sockets-implementation-version))))

;;; ---------------------------------------------------------------------------

(defun print-portable-sockets-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  Portable Sockets Interface ~a
;;;
;;;    Developed and supported by the GBBopen Project (http:/GBBopen.org/)
;;;    (See http://GBBopen.org/downloads/LICENSE for license details.)
;;; ~72,,,'-<-~>~2%"
          (portable-sockets-implementation-version)))
  
(eval-when (:load-toplevel)
  (print-portable-sockets-herald))

;;; ===========================================================================
;;;  Occasionally "localhost" isn't configured properly on some machines,
;;;  so we will use dotted notation as the default value:

(defvar *localhost* "127.0.0.1")

;;; ---------------------------------------------------------------------------
;;;  Generic functions

(defgeneric open-connection (host port &key))

;;; ===========================================================================
;;;  Need-to-port reporting

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun need-to-port-warning/error (entity error)
    (funcall (if error 'error 'warn)
             "~s needs to be defined for ~a~@[ running on ~a~].~
              ~@[~%(Please send this error message and the result of ~
              ~% evaluating (pprint *features*) to bugs@gbbopen.org.)~]"
             entity
             (lisp-implementation-type) 
             (machine-type)
             error)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro need-to-port (entity)
    ;; Generate compile-time warnings of needed porting:
    (need-to-port-warning/error entity nil)
    ;; Error if called at run time:
    `(need-to-port-warning/error ',entity t)))

;;; ===========================================================================
;;;  Passive-socket class for CLs without one

#+(or cmu
      lispworks
      scl)
(defclass passive-socket ()
  ((fd :type fixnum
       :initarg :fd
       :accessor passive-socket.fd)
   (element-type :type (member signed-byte unsigned-byte base-char)
                 :initarg :element-type
                 :accessor passive-socket.element-type)
   (port :type fixnum
         :initarg :port
         :accessor passive-socket.port)))

#+(or cmu
      lispworks
      scl)
(defmethod print-object ((passive-socket passive-socket) stream)
  (print-unreadable-object (passive-socket stream :type nil)
    (format stream "passive socket at 0.0.0.0/~s"
            (passive-socket.port passive-socket)))
  ;; Print-object must return object:
  passive-socket)

;;; ---------------------------------------------------------------------------
;;;   Hack for SBCL to store socket in socket-stream (in name slot)

#+sbcl
(defmethod sb-bsd-sockets:socket-make-stream 
    :after ((socket sb-bsd-sockets::socket) &rest args)
  (declare (ignore args))
  (setf (sb-impl::fd-stream-name (slot-value socket 'sb-bsd-sockets::stream))
        socket))

;;; ---------------------------------------------------------------------------
;;;   Add missing shutdown to Lispworks
;;;
;;;   how: 0 = SHUT_RD    Disables further receive operations
;;;        1 = SHUT_WR    Disables further send operations
;;;        2 = SHUT_RDWR  Disables further send and receive operations
#+lispworks
(fli::define-foreign-function (shutdown "shutdown")
    ((socket :long)
     (how :long))
  :result-type :fixnum)
;;;
;;;   and for ECL
#+ecl
(uffi:def-function "shutdown"
    ((socket :int)
     (how :int))
  :returning :int)
;;;
;;;   and for SBCL
#+sbcl
(sb-alien:define-alien-routine shutdown sb-alien:int
    (socket sb-alien:int)
    (how sb-alien:int))

;;; ---------------------------------------------------------------------------
;;;   Add selct for ECL (used in accept-connection)

#+ecl
(progn
  (uffi:def-foreign-type nil (:struct timeval (tv-sec :long) (tv-usec :long)))
  (declaim (inline select))
  (uffi:def-function "select"
      ((nfds :int)
       (readfds (* (:struct nil (--fds-bits (:array :long 32)))))
       (writefds (* (:struct nil (--fds-bits (:array :long 32)))))
       (exceptfds (* (:struct nil (--fds-bits (:array :long 32)))))
       (timeout (* (:struct timeval (tv-sec :long) (tv-usec :long)))))
    :returning :int))

;;; ===========================================================================
;;;  Utilities

#+(or cmu
      scl)
(defun ipaddr-to-dotted (ipaddr)
  (declare (type (unsigned-byte 32) ipaddr))
  (format nil "~d.~d.~d.~d"
          (ldb (byte 8 24) ipaddr)
          (ldb (byte 8 16) ipaddr)
          (ldb (byte 8 8) ipaddr)
          (ldb (byte 8 0) ipaddr)))

#+(or cmu
      scl)
(defun ipaddr-to-hostname (ipaddr)
  (declare (optimize (ext:inhibit-warnings 3)))
  (alien:with-alien 
      ((hostent (* ext::hostent) 
                (ext::gethostbyaddr (ext:htonl ipaddr) 4 ext::af-inet)))
    (unless (zerop (sys:sap-int (alien:alien-sap hostent)))
      (alien:slot hostent 'ext::name))))

#+sbcl
(defun ipvector-to-dotted (ipvector)
  (format nil "~d.~d.~d.~d"
          (aref ipvector 0)
          (aref ipvector 1)
          (aref ipvector 2)
          (aref ipvector 3)))

#+sbcl
(defun ipvector-to-hostname (ipvector)
  (let ((hostent
         (handler-case (sb-bsd-sockets:get-host-by-address ipvector)
           (sb-bsd-sockets:name-service-error (condition)
             (values nil condition)))))
    (when hostent
      (sb-bsd-sockets::host-ent-name hostent))))

;;; ===========================================================================
;;;  Connections

(defun open-connection-to-host (host port &key 
                                          (input-timeout nil)
                                          (output-timeout nil)
                                          (keepalive nil))
  ;; The (currently undocumented) :input-timeout extension is only accepted by
  ;; CLISP, Clozure CL, and Lispworks:
  #+(or allegro
        cmu
        digitool-mcl
        ecl
        sbcl
        scl)
  (declare (ignore input-timeout))
  ;; The (currently undocumented) :keepalive extension needs attention on:
  #+(or allegro
        cmu
        digitool-mcl
        ecl
        sbcl
        scl)
  (declare (ignore output-timeout))
  ;; The (currently undocumented) :keepalive extension needs attention on:
  #+(or allegro
        clisp
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (declare (ignore keepalive))
  #+allegro
  (socket:make-socket :remote-host host :remote-port port)
  #+clisp
  (socket:socket-connect port host 
                         :external-format ':unix 
                         :timeout (or input-timeout output-timeout))
  #+clozure
  (ccl:make-socket :remote-host host :remote-port port 
                   :input-timeout input-timeout
                   :output-timeout output-timeout
                   :keepalive keepalive)
  #+cmu
  (let ((socket (extensions:connect-to-inet-socket host port)))
    (system:make-fd-stream 
     socket
     :input 't
     :output 't
     :element-type 'character
     :buffering ':full
     :auto-close 't))
  #+digitool-mcl
  (ccl::open-tcp-stream host port)
  #+ecl
  (si:open-client-stream host port)
  #+lispworks
  (comm:open-tcp-stream host port :timeout (or input-timeout output-timeout))
  #+sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                  :protocol ':tcp
                  :type ':stream)))
    (sb-bsd-sockets:socket-connect 
     socket 
     (sb-bsd-sockets:host-ent-address
      (sb-bsd-sockets:get-host-by-name host))
     port)
    (sb-bsd-sockets:socket-make-stream 
     socket
     :input 't :output 't
     :element-type 'character
     :buffering ':full))
  #+scl
  (let ((socket (extensions:connect-to-inet-socket host port)))
    (system:make-fd-stream 
     socket
     :input 't
     :output 't
     :element-type 'character
     :buffering ':full
     :auto-close 't))
  #-(or allegro
        clisp
        clozure
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (need-to-port open-connection-to-host))

;;; ---------------------------------------------------------------------------

(defmethod open-connection ((host string) port &key input-timeout 
                                                    output-timeout
                                                    keepalive)
  (open-connection-to-host host port 
                           :input-timeout input-timeout
                           :output-timeout output-timeout
                           :keepalive keepalive))

(defmethod open-connection ((host integer) port &key input-timeout
                                                     output-timeout
                                                     keepalive)
  (open-connection-to-host host port 
                           :input-timeout input-timeout
                           :output-timeout output-timeout
                           :keepalive keepalive))

 ;;; ---------------------------------------------------------------------------

(defmacro with-open-connection ((connection host port &key input-timeout
                                                           output-timeout
                                                           keepalive)
                                &body body)
  `(let ((,connection (open-connection ,host ,port
                                       :input-timeout ,input-timeout
                                       :output-timeout ,output-timeout
                                       :keepalive ,keepalive)))
     (unwind-protect
         (progn ,@body)
       (when ,connection
         (close ,connection)))))

;;; ---------------------------------------------------------------------------

(defun make-passive-socket (port &key (backlog 5)
                                      interface
                                      keepalive
                                      reuse-address
                                      input-timeout
                                      output-timeout)
  ;; The (currently undocumented) :keepalive extension needs attention on:
  #+(or allegro
        clisp
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (declare (ignore keepalive))
  ;; The (currently undocumented) :input-timeout extension needs attention on:
  #+(or allegro
        clisp
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (declare (ignore input-timeout))
  ;; The (currently undocumented) :output-timeout extension needs attention on:
  #+(or allegro
        clisp
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (declare (ignore output-timeout))
  #+old-clisp-version
  (declare (ignore backlog interface))
  #+allegro 
  (socket:make-socket :connect ':passive 
                      :local-port port
                      :local-host interface
                      :backlog backlog
                      :reuse-address reuse-address)
  #+clisp
  (let ((passive-socket 
         #-old-clisp-version
         (socket:socket-server port
                               :interface interface
                               :backlog backlog)
         #+old-clisp-version
         (socket:socket-server port)))
    (socket:socket-options passive-socket
                           :so-reuseaddr reuse-address)
    passive-socket)
  #+clozure
  (ccl:make-socket :connect ':passive
                   :type ':stream
                   :local-port port
                   :local-host interface
                   :backlog backlog
                   :keepalive keepalive
                   :reuse-address reuse-address
                   :input-timeout input-timeout
                   :output-timeout output-timeout)
  #+cmu
  (let ((fd (ext:create-inet-listener 
             port ':stream 
             :backlog backlog
             :host (or interface 0)
             :reuse-address reuse-address)))
    (make-instance 'passive-socket
      :fd fd
      :element-type 'base-char
      :port port))
  #+digitool-mcl
  (ccl::open-tcp-stream interface port
                        :reuse-local-port-p reuse-address)
  #+ecl
  (let ((passive-socket (make-instance 'sb-bsd-sockets:inet-socket
                          :protocol ':tcp
                          :type ':stream)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address passive-socket) 't))
    (sb-bsd-sockets:socket-bind 
     passive-socket
     (if interface
         (sb-bsd-sockets:host-ent-address
          (sb-bsd-sockets:get-host-by-name interface))
         #(0 0 0 0))
     port)
    (sb-bsd-sockets:socket-listen passive-socket backlog)
    passive-socket)
  #+lispworks
  (let ((comm::*use_so_reuseaddr* reuse-address))
    (let ((fd (comm::create-tcp-socket-for-service 
               port 
               :address (or interface 0)
               :backlog backlog)))
      (unless fd
        (error "Local socket address already in use on port ~s."
               port))
      (make-instance 'passive-socket
        :fd fd
        :element-type 'base-char
        :port port)))
  #+sbcl
  (let ((passive-socket (make-instance 'sb-bsd-sockets:inet-socket
                          :protocol ':tcp
                          :type ':stream)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address passive-socket) 't))
    (sb-bsd-sockets:socket-bind 
     passive-socket
     (if interface
         (sb-bsd-sockets:host-ent-address
          (sb-bsd-sockets:get-host-by-name interface))
         #(0 0 0 0))
     port)
    (sb-bsd-sockets:socket-listen passive-socket backlog)
    passive-socket)
  #+scl
  (let ((fd (ext:create-inet-listener
             port ':stream 
             :backlog backlog
             :host (or interface 0)
             :reuse-address reuse-address)))
    (make-instance 'passive-socket
      :fd fd
      :element-type 'base-char
      :port port))
  #-(or allegro
        clisp
        clozure
        cmu
        digitool-mcl
        ecl
        lispworks
        sbcl
        scl)
  (need-to-port make-passive-socket))

;;; ---------------------------------------------------------------------------

(defun close-passive-socket (passive-socket)
  #+clisp
  (socket:socket-server-close passive-socket)
  #+cmu
  (unix:unix-close (passive-socket.fd passive-socket))
  #+ecl
  (sb-bsd-sockets:socket-close passive-socket)
  #+sbcl
  (sb-bsd-sockets:socket-close passive-socket)
  #+scl
  (unix:unix-close (passive-socket.fd passive-socket))
  #-(or clisp
        cmu
        ecl
        sbcl
        scl)
  (close passive-socket))

;; Close is a method in Lispworks, so we extend it for passive
;; sockets:
#+lispworks
(defmethod close ((passive-socket passive-socket) &key abort)
  (declare (ignore abort))
  (comm::close-socket (passive-socket.fd passive-socket)))

;;; ---------------------------------------------------------------------------

(defun shutdown-socket-stream (socket-stream direction)
  ;;; Note: Allegro, CLISP, Clozure, and Digitool-MCL only support :input and
  ;;;       :output direction, so that is all that we document as providing...
  #-(or allegro
        clisp
        clozure
        cmu
        lispworks
        sbcl
        scl)
  (declare (ignore socket-stream direction))
  #+allegro
  (socket:shutdown socket-stream :direction direction)
  #+clisp
  (socket:socket-stream-shutdown socket-stream direction)
  #+clozure
  (ccl:shutdown socket-stream :direction direction)
  #+cmu
  (ext:inet-shutdown (sys:fd-stream-fd socket-stream)
                     (ecase direction
                       (:input ext:shut-rd)
                       (:output ext:shut-wr)
                       #+not-supported
                       (:input-output ext:shut-rdwr)))
  #+lispworks
  (shutdown (comm:socket-stream-socket socket-stream)
            (ecase direction
              (:input 0)
              (:output 1)
              #+not-supported
              (:input-output 2)))
  #+sbcl
  (shutdown (sb-sys::fd-stream-fd socket-stream)
            (ecase direction
              (:input 0)
              (:output 1)
              #+not-supported
              (:input-output 2)))
  #+scl
  (ext:inet-shutdown (sys:fd-stream-fd socket-stream)
                     (ecase direction
                       (:input ext:shut-rd)
                       (:output ext:shut-wr)
                       #+not-supported
                       (:input-output ext:shut-rdwr)))
  #-(or allegro 
        clisp 
        clozure
        cmu
        lispworks
        sbcl
        scl)
  (need-to-port shutdown-socket-stream))
  
;;; ---------------------------------------------------------------------------

(defun accept-connection (passive-socket &key (wait 't))
  #+allegro
  (socket:accept-connection passive-socket :wait wait)
  #+clisp
  (when (cond ((numberp wait)
               (socket:socket-wait passive-socket wait))
              (wait 
               (socket:socket-wait passive-socket))
              (t (socket:socket-wait passive-socket 0)))
    (socket:socket-accept passive-socket :external-format ':unix))
  #+clozure
  (ccl:accept-connection passive-socket :wait wait)
  #+cmu
  (let ((fd (passive-socket.fd passive-socket)))
    (when (sys:wait-until-fd-usable 
           fd ':input
           ;; convert :wait to timeout:
           (cond ((eq wait 't) nil)
                 ((not wait) 0)
                 (t wait)))
      (sys:make-fd-stream
       (ext:accept-tcp-connection fd)
       :input 't
       :output 't
       :element-type (passive-socket.element-type passive-socket)
       :buffering ':full
       :auto-close 't)))
  #+ecl
  (when (progn ; need something like wait-until-fd-usable 
         (sb-bsd-sockets:socket-file-descriptor passive-socket) 
         ':input
         ;; convert :wait to timeout:
         (cond ((eq wait 't) nil)
               ((not wait) 0)
               (t wait)))
    (sb-bsd-sockets:socket-make-stream 
     (sb-bsd-sockets:socket-accept passive-socket)
     :input 't :output 't
     :element-type 'character
     :buffering ':full))
  #+lispworks
  (let ((socket-fd (passive-socket.fd passive-socket)))
    (when (or wait (comm::socket-listen socket-fd))
      (let ((socket (comm::get-fd-from-socket socket-fd)))
        (make-instance 'comm:socket-stream
          :socket socket
          :direction ':io
          :element-type (passive-socket.element-type passive-socket)))))
  #+sbcl
  (when (sb-sys:wait-until-fd-usable 
         (sb-bsd-sockets:socket-file-descriptor passive-socket) 
         ':input
         ;; convert :wait to timeout:
         (cond ((eq wait 't) nil)
               ((not wait) 0)
               (t wait)))
    (sb-bsd-sockets:socket-make-stream 
     (sb-bsd-sockets:socket-accept passive-socket)
     :input 't :output 't
     :element-type 'character
     :buffering ':full))
  #+scl
  (let ((fd (passive-socket.fd passive-socket)))
    (when (sys:wait-until-fd-usable 
           fd ':input
           ;; convert :wait to timeout:
           (cond ((eq wait 't) nil)
                 ((not wait) 0)
                 (t wait)))
      (sys:make-fd-stream
       (ext:accept-tcp-connection fd)
       :input 't
       :output 't
       :element-type (passive-socket.element-type passive-socket)
       :buffering ':full
       :auto-close 't)))
  #-(or allegro 
        clisp
        clozure
        cmu
        ecl 
        lispworks 
        sbcl
        scl)
  (need-to-port accept-connection))

;;; ===========================================================================
;;;  Connection Server
  
(defun start-connection-server (function port 
                                &key (name "Connection Server") 
                                     (backlog 5)
                                     interface
                                     keepalive
                                     reuse-address
                                     input-timeout
                                     output-timeout)
  #+threads-not-available
  (declare (ignore function port name backlog interface reuse-address
                   keepalive input-timout output-timeout))
  #-threads-not-available
  (spawn-thread
   name
   #'(lambda (function port interface backlog reuse-address)
       (let ((passive-socket 
              (make-passive-socket port 
                                   :backlog backlog
                                   :interface interface
                                   :keepalive keepalive
                                   :reuse-address reuse-address
                                   :input-timeout input-timeout
                                   :output-timeout output-timeout)))
         (unwind-protect
             (loop
               (let ((connection (accept-connection passive-socket)))
                 (funcall function connection)))
           (close-passive-socket passive-socket))))
   function port interface backlog reuse-address)
  #+threads-not-available
  (threads-not-available 'start-connection-server))

;;; ===========================================================================
;;;  Socket Attribute Readers

(defun local-hostname-and-port (connection &optional do-not-resolve)
  #-(or allegro
        clisp 
        clozure
        cmu
        lispworks
        sbcl
        scl)
  (declare (ignore connection do-not-resolve))
  #+allegro
  (let* ((ipaddr (socket:local-host connection))
         (dotted (socket:ipaddr-to-dotted ipaddr)))
    (values (if do-not-resolve
                dotted
                (let ((resolved (socket:ipaddr-to-hostname ipaddr)))
                  (if resolved
                      (format nil "~a (~a)" dotted resolved)
                      dotted)))
            (socket:local-port connection)))
  #+clisp
  (socket:socket-stream-local connection (not do-not-resolve))
  #+clozure
  (let* ((ipaddr (ccl:local-host connection))
         (dotted (ccl:ipaddr-to-dotted ipaddr)))
    (values (if do-not-resolve
                dotted
                (let ((resolved 
                       ;; CCL can error here:
                       (ignore-errors (ccl:ipaddr-to-hostname ipaddr))))
                  (if resolved
                      (format nil "~a (~a)" dotted resolved)
                      dotted)))
            (ccl:local-port connection)))
  #+cmu
  (let ((fd (sys:fd-stream-fd connection)))
    (multiple-value-bind (ipaddr port)
        (ext:get-socket-host-and-port fd)
      (let ((dotted (ipaddr-to-dotted ipaddr)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipaddr-to-hostname ipaddr)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))   
  #+lispworks
  (multiple-value-bind (ipaddr port)
      (comm:socket-stream-address connection)
    (let ((dotted (comm:ip-address-string ipaddr)))
      (values (if do-not-resolve
                  dotted
                  (let ((resolved (comm::get-host-entry ipaddr
                                                        :fields '(:name))))
                    (if resolved
                        (format nil "~a (~a)" dotted resolved)
                        dotted)))
            port)))
  #+sbcl
  (let ((socket (sb-impl::fd-stream-name connection)))
    (multiple-value-bind (ipvector port)
        (sb-bsd-sockets:socket-name socket)
      (let ((dotted (ipvector-to-dotted ipvector)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipvector-to-hostname ipvector)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))        
  #+scl
  (let ((fd (sys:fd-stream-fd connection)))
    (multiple-value-bind (ipaddr port)
        (ext:get-socket-host-and-port fd)
      (let ((dotted (ipaddr-to-dotted ipaddr)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipaddr-to-hostname ipaddr)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))   
  #-(or allegro
        clisp 
        clozure
        cmu
        lispworks
        sbcl
        scl)
  (need-to-port local-hostname-and-port))

;;; ---------------------------------------------------------------------------

(defun remote-hostname-and-port (connection &optional do-not-resolve)
  #-(or allegro
        clisp
        clozure
        cmu
        lispworks
        sbcl
        scl)
  (declare (ignore connection do-not-resolve))
  #+allegro
  (let* ((ipaddr (socket:remote-host connection))
         (dotted (socket:ipaddr-to-dotted ipaddr)))
    (values (if do-not-resolve
                dotted
                (let ((resolved (socket:ipaddr-to-hostname ipaddr)))
                  (if resolved
                      (format nil "~a (~a)" dotted resolved)
                      dotted)))
            (socket:remote-port connection)))
  #+clisp
  (socket:socket-stream-peer connection (not do-not-resolve))
  #+clozure
  (let* ((ipaddr (ccl:remote-host connection))
         (dotted (ccl:ipaddr-to-dotted ipaddr)))
    (values (if do-not-resolve
                dotted
                (let ((resolved
                       ;; CCL can error here:
                       (ignore-errors (ccl:ipaddr-to-hostname ipaddr))))
                  (if resolved
                      (format nil "~a (~a)" dotted resolved)
                      dotted)))
            (ccl:remote-port connection)))
  #+cmu
  (let ((fd (sys:fd-stream-fd connection)))
    (multiple-value-bind (ipaddr port)
        (ext:get-peer-host-and-port fd)
      (let ((dotted (ipaddr-to-dotted ipaddr)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipaddr-to-hostname ipaddr)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))   
  #+lispworks
  (multiple-value-bind (ipaddr port)
      (comm:socket-stream-peer-address connection)
    (let ((dotted (comm:ip-address-string ipaddr)))
      (values (if do-not-resolve
                  dotted
                  (let ((resolved (comm::get-host-entry ipaddr
                                                        :fields '(:name))))
                    (if resolved
                        (format nil "~a (~a)" dotted resolved)
                        dotted)))
            port)))
  #+sbcl
  (let ((socket (sb-impl::fd-stream-name connection)))
    (multiple-value-bind (ipvector port)
        (sb-bsd-sockets:socket-peername socket)
      (let ((dotted (ipvector-to-dotted ipvector)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipvector-to-hostname ipvector)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))        
  #+scl
  (let ((fd (sys:fd-stream-fd connection)))
    (multiple-value-bind (ipaddr port)
        (ext:get-peer-host-and-port fd)
      (let ((dotted (ipaddr-to-dotted ipaddr)))
        (values (if do-not-resolve
                    dotted
                    (let ((resolved (ipaddr-to-hostname ipaddr)))
                      (if resolved
                          (format nil "~a (~a)" dotted resolved)
                          dotted)))
                port))))   
  #-(or allegro
        clisp
        clozure
        cmu 
        lispworks
        sbcl
        scl)
  (need-to-port remote-hostname-and-port))

;;; ===========================================================================
;;;  Useful for HTTP line termination:

(defun write-crlf (&optional (stream *standard-output*))
  ;; HTTP requires CR/LF line termination: 
  (write-char #\return stream)
  (write-char #\linefeed stream))

;;; ===========================================================================
;;;  Portable sockets interface is fully loaded:

(pushnew ':portable-sockets *features*)
(pushnew *portable-sockets-version-keyword* *features*)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
