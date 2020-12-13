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
;;     Extempore session management.
(defun connect ()
  (application:start 'tcp-client))

;;;;;::=-------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-tempo! (bpm)
  (xt.msg:async
   (xt.lang:sexp "*metro*" (++ "'set-tempo " (xt.lang:->xt bpm)))))

(defun sys-load (xtm-file)
  (case (xt.msg:sync
         (xt.lang:sexp "sys:load" (xt.lang:->xt xtm-file)))
    ('true #(true loaded))
    ('false #(false already-loaded))))