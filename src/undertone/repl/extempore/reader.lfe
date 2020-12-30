(defmodule undertone.repl.extempore.reader
  (export
   (read 0)))

(include-lib "logjam/include/logjam.hrl")

(defun read ()
  (let ((sexp (undertone.sexp:readlines (undertone.server:prompt))))
    (log-debug "Got user input (sexp): ~p" `(,sexp))
    sexp))