;;;; This module provides LFE with a DSL for sending Extempore Scheme and xtlang
;;;; messages to an Extempore TCP compiler server. It does so using forms
;;;; inspired by Extempore's, but using idioms from LFE and Lisp-2s, given that
;;;; this is for use in a Lisp-2.
(defmodule xtl
  ;; API
  (export
   (/> 5)
   (// 1) (// 5)
   (play 4)
   (scale 2)
   (set-tempo 1)
   (sys-load 1))
  ;; Utility
  (export
   (->type 1)
   (->types 1)
   (connect 0)
   (payload 1)
   (call-msg 1)
   (cast-msg 1)
   (wrap-str 1)))

(include-lib "lfe/include/clj.lfe")

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

(defun /> (name length offset patt-expr patt-lists)
  "Define and play a pattern."
  (cast-msg
   (sexp ":>" (string:join
                  (list (->types `(,name ,length ,offset))
                        patt-expr
                        patt-lists)
                  " "))))

(defun // (name)
  "Stop a pattern."
  (// name 1 0 (play 'nil 0 0 0) (scale 0 0)))

(defun // (name length offset patt-expr patt-lists)
  "Stop a pattern."
  (cast-msg
   (sexp ":|" (string:join
                  (list (->types `(,name ,length ,offset))
                        patt-expr
                        patt-lists)
                  " "))))

(defun play (inst pitch-var velocity duration)
  ""
  (sexp "play" (->types `(,inst ,pitch-var ,velocity ,duration))))

(defun scale (octave-num note-count)
  ""
  (sexp "scale" (->types `(,octave-num ,note-count))))

(defun set-tempo (bpm)
  (call-msg
   (sexp "*metro*" (++ "'set-tempo " (->type bpm)))))

(defun sys-load (xtm-file)
  (call-msg
   (sexp "sys:load" (->type xtm-file))))

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-str (arg)
  (++ "\"" arg "\""))

(defun sexp (fn-name rest)
  (++ "(" fn-name " " rest ")"))

(defun ->type (arg)
  (cond ((io_lib:printable_latin1_list arg) (wrap-str arg))
        ((is_atom arg) (erlang:atom_to_list arg))
        ((is_integer arg) (erlang:integer_to_list arg))
        ((is_float arg) (io_lib:format "~.5f" `(,arg)))
        ((is_binary arg) (binary:bin_to_list arg))
        ;; symbol
        ('true arg)))

(defun ->types (args)
  (string:join
   (lists:map #'->type/1 args)
   " "))

(defun payload (str)
  (binary:list_to_bin (++ str "\r\n")))

(defun call-msg (bin)
  (lfe_io:format "Calling ~p ... ~n" `(,bin))
  (tcp-client:call-msg (payload bin)))

(defun cast-msg (bin)
  (lfe_io:format "Casting ~p ... ~n" `(,bin))
  (tcp-client:cast-msg (payload bin)))
