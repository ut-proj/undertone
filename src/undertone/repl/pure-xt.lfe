(defmodule undertone.repl.pure-xt
  (export
   (loop 1)
   (print 1)
   (read 0)
   (start 0)
   (xt-eval 1)))

(include-lib "logjam/include/logjam.hrl")

(defun read ()
  (let ((val (string:trim (io:get_line (undertone.sysconfig:prompt)))))
    (log-debug "Got user input: ~p" `(,val))
    val))

(defun xt-eval (input)
  (case input
    ("" (read))
    ('() (read))
    ("quit" 'quit)
    (_ (xt.msg:sync input))))

(defun print (result)
  (case result
    ("quit" 'do-nothing)
    ("" 'do-nothing)
    ('() 'do-nothing)
    (_ (lfe_io:format "~p~n" `(,result))))
  result)

(defun loop
  (('quit)
   'good-bye)
  ((code)
   (loop (print (xt-eval (read))))))

(defun start ()
  (loop 'start))