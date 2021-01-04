(defmodule undertone.repl.extempore.evaler
  (export
   (dispatch 1)
   (eval 1)
   (start 1)
   (xt 1)
   (xt-blocking 1)))

;;;;;::=------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   functional API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dispatch
  (((= `#m(tokens ,tokens) sexp))
   (case tokens
     ;; Supported REPL commands
     ('("check-xt") 'check-xt)
     ('("eom") 'term)
     ('("exit") 'quit)
     ('("h") 'help)
     ('("help") 'help)
     (`("load" ,file) `#(load ,file))
     ('("quit") 'quit)
     (`("rerun" ,idx) `#(rerun ,idx))
     (`("rerun" ,start ,end) `#(rerun ,start ,end))
     ('("sess") 'sess)
     (`("sess" ,idx) `#(sess ,idx))
     (`("sess-line" ,idx) `#(sess-line ,idx))
     (`("sess-load" ,file) `#(sess-load ,file))
     (`("sess-save" ,file) `#(sess-save ,file))
     ('("term") 'term)
     ('("v") 'version)
     ('("version") 'version)
     ;; Pass-throughs to Extempore (sync + async)
     (`("call" . ,_) (xt-blocking sexp))
     (_ (xt sexp)))))

(defun eval (sexp)
  (eval-repl sexp))

(defun eval-repl (sexp)
  "The REPL eval function

  Can't name it 'eval' due to LFE shadowing overrides for core functions/macros.
  The 'eval' alias is provided for conveience when calling from outside this
  module."
  (let ((tokens (mref sexp 'tokens)))
    (case tokens
      ('() 'empty)
      (progn
        (undertone.repl.extempore.util:maybe-save-command sexp)
        (dispatch sexp)))))

(defun xt-blocking (sexp)
  "This is going to be ugly ... (for now/until we have real parsing)"
  ;; 1. with the value associated with the 'source' key, replace initial
  ;;    "(call" with ""
  ;; 2. from the same, do a regex to replace the final ")" with ""
  ;; 3. without any /real/ parsing, this should give us the body of the
  ;;    Extempore code
  ;; 4. send that to Extempore as a synchronous message
  (let* ((source (string:trim (mref sexp 'source)))
         (remove-call (re:replace source "^\\(call" "" '(#(return list))))
         (extempore-form (re:replace remove-call "\\)$" "" '(#(return list)))))
    (xt.msg:sync extempore-form)))

(defun xt (sexp)
  (xt.msg:async (mref sexp 'source)))

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   server-based API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (st)
  (let ((self (self)))
    (spawn_link (lambda () (init st self)))))

(defun init (st pid)
  (eval-loop st pid))

(defun eval-loop (st pid)
  (receive
    (`#(eval-expr ,pid ,sexp)
     (let ((st (eval-sexp st pid sexp)))
       (eval-loop st pid)))))

(defun eval-sexp (st pid sexp)
  (try
    (let ((value (eval-repl sexp)))
      (undertone.repl.extempore.printer:print value)
      (! pid `#(eval-value ,(self) ,value st))
      st)
    (catch
      ;;(exit:normal (exit 'normal))
      (`#(,class ,reason ,stack)
       (! pid `#(eval-error ,(self) ,class))
       (exit (undertone.repl.extempore.util:nocatch
              class `#(,reason ,stack)))))))