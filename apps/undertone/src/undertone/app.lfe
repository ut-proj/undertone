(defmodule undertone.app
  (behaviour gen_server)
  (export
    ;; app implementation
    (start 2)
    (stop 0)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   application implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_type _args)
  (logger:set_application_level 'undertone 'all)
  (log-notice "Starting undertone application ...")
  (undertone.sup:start_link))

(defun stop ()
  (undertone.sup:stop)
  'ok)
