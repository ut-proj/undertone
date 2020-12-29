(defmacro /> args `(xt.pat:/> ,@args))
(defmacro // args `(xt.pat:// ,@args))
(defmacro play args `(xt.common:play ,@args))
(defmacro scale args `(xt.common:scale ,@args))
(defmacro set-tempo! args `(xt:set-tempo! ,@args))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: xt/patterns --| ()
  'ok)