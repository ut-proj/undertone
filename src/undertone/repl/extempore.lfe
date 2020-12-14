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
  (let ((`#(ok ,sexpr) (lfe_io:read_line (prompt))))
    (log-debug "Got user input (sexpr): ~p" `(,sexpr))
    sexpr))

(defun repl-eval (sexpr)
  (case sexpr
    ('() (read))
    (`(exit . ,_) 'quit)
    (`(h . ,_) 'help)
    (`(help . ,_) 'help)
    (`(quit . ,_) 'quit)
    (`(v . ,_) 'version)
    (`(version . ,_) 'version)
    (_ (xt-eval sexpr))))

(defun xt-eval (sexpr)
  (let ((bitstring (list sexpr)))
    (xt.msg:sync bitstring)))

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
(defun version ()
  (lfe_io:format "~p~n" `(,(undertone.sysconfig:versions))))
