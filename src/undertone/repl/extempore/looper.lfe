(defmodule undertone.repl.extempore.looper
  (export
   (loop 0) (loop 1)))

(include-lib "logjam/include/logjam.hrl")

(defun loop ()
  (log-debug "Looping REPL ...")
  (undertone.server:set-repl 'extempore)
  (undertone.repl.extempore.util:display-banner)
  (loop 'start))

(defun loop
  (('quit)
   'good-bye)
  ((_)
   (try
       (loop
        (undertone.repl.extempore.printer:print
         (undertone.repl.extempore.evaler:eval
          (undertone.repl.extempore.reader:read))))
     (catch
       (`#(,err ,clause ,stack)
        (io:format "~p: ~p~n" `(,err ,clause))
        (io:format "Stacktrace: ~p~n" `(,stack))
        (loop 'restart))))))

