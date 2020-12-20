(defmodule undertone.repl.extempore
  (export
   (loop 1)
   (print 1)
   (read 0)
   (start 0)
   (xt-eval 1)))

(include-lib "logjam/include/logjam.hrl")

(defun prompt () "extempore> ")

;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   core repl functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read ()
  (let ((sexp (undertone.sexp:readlines (prompt))))
    (log-debug "Got user input (sexp): ~p" `(,sexp))
    sexp))

(defun repl-eval (sexp)
  (case (mref sexp 'tokens)
    ('() 'empty)
    (`("call" . ,_) (xt-blocking-eval sexp))
    ('("eom") 'term)
    ('("exit") 'quit)
    ('("h") 'help)
    ('("help") 'help)
    ('("quit") 'quit)
    (`("run" ,file) (run file))
    ('("term") 'term)
    ('("v") 'version)
    ('("version") 'version)
    (_ (xt-eval sexp))))

(defun xt-blocking-eval (sexp)
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

(defun xt-eval (sexp)
  (xt.msg:async (mref sexp 'source)))

(defun print (result)
  (case result
    ('() 'ok)
    ('empty 'ok)
    ('help (help))
    ('quit 'ok)
    ('term (xt.msg:async ""))
    ('version (version))
    (_ (lfe_io:format "~p~n" `(,result))))
  result)

(defun loop
  (('quit)
   'good-bye)
  ((_)
   (try
     (loop (print (repl-eval (read))))
     (catch
       (`#(,err ,clause ,stack)
        (io:format "~p: ~p~n" `(,err ,clause))
        (io:format "Stacktrace: ~p~n" `(,stack))
        (loop 'restart))))))

(defun start ()
  (loop 'start))

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   shell functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun help ()
  (lfe_io:format "~s" `(,(binary_to_list
                           (undertone.sysconfig:read-priv
                            "help/repl-extempore.txt")))))
(defun run (file-name)
  (let ((`#(ok ,data) (file:read_file file-name)))
    (xt.msg:sync (binary_to_list data))))

(defun version ()
  (lfe_io:format "~p~n" `(,(undertone.sysconfig:versions))))
