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
    ('() (read))
    (`("exit") 'quit)
    ('("h") 'help)
    ('("help") 'help)
    ('("quit") 'quit)
    (`("run" ,file) (run file))
    ('("v") 'version)
    ('("version") 'version)
    (_ (xt-eval sexp))))

(defun xt-eval (sexp)
  (xt.msg:async (mref sexp 'source)))

(defun print (result)
  (case result
    ('quit 'ok)
    ('help (help))
    ('version (version))
    ('() 'ok)
    (_ (lfe_io:format "~p~n" `(,result))))
  result)

(defun loop
  (('quit)
   'good-bye)
  ((code)
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
