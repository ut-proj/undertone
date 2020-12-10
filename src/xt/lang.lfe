;;;; Basic functions for creating Extempore forms.
;;;;
;;;; Note that the 'lang' does not reference `xtlang` but rather the general
;;;; scheme language used by Extempore.
(defmodule xt.lang
  (export
   (->lfe 1)
   (->xt 1)
   (->xts 1)
   (sexp 1) (sexp 2)
   (wrap-str 1)))

(defun ->xt (arg)
  (cond ((== arg 'true) "#t")
        ((== arg 'false) "#f")
        ((io_lib:printable_latin1_list arg) (wrap-str arg))
        ((is_atom arg) (erlang:atom_to_list arg))
        ((is_integer arg) (erlang:integer_to_list arg))
        ((is_float arg) (io_lib:format "~.5f" `(,arg)))
        ((is_binary arg) (binary:bin_to_list arg))
        ;; symbol
        ('true arg)))

(defun ->xts (args)
  (string:join
   (lists:map #'->xt/1 args)
   " "))

(defun ->lfe (arg)
  (case arg
   (#"#t" 'true)
   (#"#f" 'false)
   (#"NIL" 'nil)
   (_ (cond ((?= `#(result true ,val) (coers:to_int arg))
             val)
            ((?= `#(result true ,val) (coers:to_float arg))
             val)
            ((?= `#(result true ,val) (coers:to_bool arg))
             val)
            ((?= `#(result true ,val) (coers:to_string arg))
             val)
            ('true arg)))))

(defun sexp (fn-name)
  (++ "(" fn-name ")"))

(defun sexp (fn-name rest)
  (++ "(" fn-name " " rest ")"))

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-str (arg)
  (++ "\"" arg "\""))

(defun check-coersion
  ((`#(result true ,val))
   val)
  ((`#(result false ,_))
   'error))

(defun ->float (arg)
  (check-coersion (coers:to_float arg)))

(defun ->int (arg)
  (check-coersion (coers:to_int arg)))

(defun ->string (arg)
  (check-coersion (coers:to_string arg)))
