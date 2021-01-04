(defmodule undertone.repl.extempore.reader
  (export
   (read 0) (read 1) (read 2)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/repl.lfe")

;;;;;::=------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   functional API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read ()
  (read (undertone.server:prompt)))

(defun read (prompt)
  (let ((`#(ok ,sexp) (undertone.sexp:readlines prompt)))
    (log-debug "Got user input (sexp): ~p" `(,sexp))
    sexp))

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   server-based API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read
  (((= (match-state prompt p) st) evaler)
   (let ((reader (spawn_link (lambda () (exit (read p))))))
     (receive-reads st reader evaler))))

(defun receive-reads (st reader evaler)
  (receive
    (`#(EXIT ,reader ,result)
     `#(,result ,evaler))
    (`#(EXIT ,evaler #(,reason ,stack))
     (undertone.repl.extempore.util:report-exception'error reason stack)
     (receive-reads st reader (undertone.repl.extempore.evaler:start st)))
    (`#(EXIT ,evaler ,reason)
     (undertone.repl.extempore.util:report-exception 'error reason '())
     (receive-reads st reader (undertone.repl.extempore.evaler:start st)))))
