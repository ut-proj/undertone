;;;; This module provides LFE with a DSL for sending Extempore Scheme and xtlang
;;;; messages to an Extempore TCP compiler server. It does so using forms
;;;; inspired by Extempore's, but using idioms from LFE and Lisp-2s, given that
;;;; this is for use in a Lisp-2.
(defmodule xtl
  (export
   (/> 5)
   (// 1) (// 5)
   (call-msg 1)
   (cast-msg 1)
   (connect 0)
   (play 4)
   (scale 2)
   (set-tempo 1)
   (sys-load 1)))

;; XXX These will eventually go into the module that's responsible for
;;     Extempore session management. There's a pretty good chance that
;;     this project will be run as an OTP release, possibly with different
;;     relx profiles for different backends. If that's the case, then
;;     configuration will be done via `./config/sys.config` and/or ENV
;;     VARs.
(defun connect ()
  (application:start 'tcp-client))

;;;;;::=--------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun /> (name length offset patt-expr patt-lists)
  "Define and play a pattern."
  'ok)

(defun // (name)
  "Stop a pattern."
  (// name 0 0 (funcall (lambda () 'ok)) '()))

(defun // (name length offset patt-expr patt-lists)
  "Stop a pattern."
  'ok)

(defun play (inst pitch-var velocity duration)
  ""
  'ok)

(defun scale (octave-num note-count)
  ""
  'ok)

(defun set-tempo (bpm)
  'ok)

(defun sys-load (xtm-file)
  (arity-1 "sys:load" xtm-file))

;;;;;::=----------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=----------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arity-1 (fn-name arg)
  (binary "(" fn-name " \"" arg "\")"))

(defun wrap-str (arg)
  (++ "\"" arg "\""))

(defun ->type (arg)
  (cond ((io_lib:printable_latin1_list arg) (wrap-str arg))
        ((is_atom arg) (wrap-str (erlang:atom_to_list arg)))
        ((is_integer arg) (erlang:integer_to_list arg))
        ((is_float arg) (erlang:float_to_list arg))
        ((is_binary arg) (binary:bin_to_list arg))
        ;; symbol
        ('true arg)))

(defun payload
  ((bin) (when (is_binary bin))
   (binary (bin binary) "\r\n"))
  ((str) (when (is_list str))
   (payload (binary:list_to_bin str))))

(defun call-msg (bin)
  (tcp-client:call-msg (payload bin)))

(defun cast-msg (bin)
  (tcp-client:cast-msg (payload bin)))

