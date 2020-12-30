(defmodule undertone.repl.extempore.printer
  (export
   (print 1)))

(defun print (result)
  (case result
    ('()
     'ok)
    ('check-xt
     (undertone.repl.extempore.commands:check-extempore))
    ('empty
     'ok)
    ('help
     (undertone.repl.extempore.commands:help))
    (`#(load ,file)
     (undertone.repl.extempore.commands:load file))
    ('quit
     'ok)
    (`#(rerun ,idx)
     (undertone.repl.extempore.commands:rerun
      (list_to_integer idx)))
    (`#(rerun ,start ,end)
     (undertone.repl.extempore.commands:rerun
      (list_to_integer start)
      (list_to_integer end)))
    ('sess
     (undertone.repl.extempore.commands:sess))
    (`#(sess ,idx)
     (undertone.repl.extempore.commands:sess
      (list_to_integer idx)))
    (`#(sess-line ,idx)
     (undertone.repl.extempore.commands:sess-line
      (list_to_integer idx)))
    (`#(sess-load ,file)
     (undertone.server:session-load file))
    (`#(sess-save ,file)
     (undertone.server:session-save file))
    ('term
     (xt.msg:async ""))
    ('version
     (undertone.repl.extempore.commands:version))
    (_
     (lfe_io:format "~p~n" `(,result))))
  result)