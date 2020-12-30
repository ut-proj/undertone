(defmodule undertone.repl.extempore.evaler
  (export
   (dispatch 1)
   (eval 1)
   (xt 1)
   (xt-blocking 1)))

(defun dispatch
  (((= `#m(tokens ,tokens) sexp))
   (case tokens
     (`("call" . ,_) (xt-blocking sexp))
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
     (_ (xt sexp)))))

(defun eval (sexp)
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
