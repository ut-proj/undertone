;;;; Basic functions for creating Extempore forms.
;;;;
;;;; Note that the 'lang' does not reference `xtlang` but rather the general
;;;; scheme language used by Extempore.
(defmodule xt.lang
  (export
   (->type 1)
   (->types 1)
   (sexp 1) (sexp 2)
   (wrap-str 1)))

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

(defun sexp (fn-name)
  (++ "(" fn-name ")"))

(defun sexp (fn-name rest)
  (++ "(" fn-name " " rest ")"))

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-str (arg)
  (++ "\"" arg "\""))
