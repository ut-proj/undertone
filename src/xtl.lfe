;;;; This module provides LFE with a DSL for sending Extempore Scheme and xtlang
;;;; messages to an Extempore TCP compiler server. It does so using forms
;;;; inspired by Extempore's, but using idioms from LFE and Lisp-2s, given that
;;;; this is for use in a Lisp-2.
(defmodule xtl
  (export
   (play 
   (set-temp0 1)
   (sys-load 1))
  (export-macro /> //))

;;;;;::=--------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun /> (name length offset patt-expr patt-lists)
  )

(defun // (name)
  (// name 0 0 )

(defun // (name length offset patt-expr patt-lists)
  )

(defun play (inst pitch-var velocity duration)
  "")

(defun scale (octave-num note-count)
  "")

(defun set-tempo (bpm)
  'tbd)

(defun sys-load (xtm-file)
  (arity-1 "sys:load" xtm-file))

;;;;;::=----------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=----------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arity-1 (fn-name arg)
  (binary "(" fn-name "\"" arg "\""))

(defun wrap-str (arg)
  (++ "\"" arg "\""))

(defun ->type (arg)
  (cond ((io_lib:printable_latin1_list arg) (wrap-str arg))
        ((is_atom arg) (wrap-str (erlang:atom_to_list arg)))
        ((is_integer arg) (erlang:integer_to_list arg))
        ((is_float arg) (erlang:float_to_list arg))
        ((is_binary arg) (binary:bin_to_list arg))
        ;; symbol
        (true arg)))

(defun payload (bin)
  (binary (bin binary) "\r\n"))

(defun call-msg (bin)
  (tcp-client:call-msg (payload bin)))

(defun cast-msg (bin)
  (tcp-client:cast-msg (payload bin)))

(defun identity (x)
  x)'

(defmacro identity
  ((`(,args . ,body))
   `(defun identity args body)))