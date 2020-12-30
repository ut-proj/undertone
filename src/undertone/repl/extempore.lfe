(defmodule undertone.repl.extempore
  (export
   (run 0)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   simple, function-based approach   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run ()
  (undertone.repl.extempore.looper:loop))
