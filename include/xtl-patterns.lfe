(defmacro /> args `(xtl:/> ,@args))
(defmacro // args `(xtl:// ,@args))
(defmacro play args `(xtl:play ,@args))
(defmacro scale args `(xtl:scale ,@args))
(defmacro set-tempo args `(xtl:set-tempo ,@args))

;; This function is for display purpses when used in the REPL
;; and need to be the last function in the include file.
(defun |-- loaded include: xtl-patterns --| ()
  'ok)