(defrecord state
  name
  (args '())
  prompt
  curr
  save
  base
  (slurped? 'false))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: repl --| ()
  'ok)