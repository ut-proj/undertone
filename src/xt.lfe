;;;; This module provides LFE with a DSL for sending Extempore Scheme and xtlang
;;;; messages to an Extempore TCP compiler server. It does so using forms
;;;; inspired by Extempore's, but using idioms from LFE and Lisp-2s, given that
;;;; this is for use in a Lisp-2.
(defmodule xt
  ;; API
  (export
   (connect 0)
   (set-tempo! 1)
   (sys-load 1)))

;; XXX These will eventually go into the module that's responsible for
;;     Extempore session management. There's a pretty good chance that
;;     this project will be run as an OTP release, possibly with different
;;     relx profiles for different backends. If that's the case, then
;;     configuration will be done via `./config/sys.config` and/or ENV
;;     VARs.
(defun connect ()
  (application:start 'tcp-client))

;;;;;::=-------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-tempo! (bpm)
  (xt.msg:async
   (xt.lang:sexp "*metro*" (++ "'set-tempo " (xt.lang:->type bpm)))))

(defun sys-load (xtm-file)
  (xt.msg:sync
   (xt.lang:sexp "sys:load" (xt.lang:->type xtm-file))))
