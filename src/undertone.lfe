(defmodule undertone
  (export
   (quit 0)
   (start-xt-repl 0)))

(include-lib "logjam/include/logjam.hrl")

(defun quit ()
  (io:format ".")
  (log-info "Disconnecting TCP client ...")
  (application:stop 'tcp-client)
  (io:format ".")
  (timer:sleep 500)
  (io:format ".")
  (log-info "Stopping undertone services ...")
  (application:stop 'undertone)
  (io:format ".")
  (timer:sleep 500)
  (io:format ".")
  (application:stop 'undertone-lib)
  (io:format ".")
  (timer:sleep 500)
  (io:format ".")
  (log-info "Stopping support services ...")
  (application:stop 'loise)
  (io:format ".")
  (timer:sleep 500)
  (io:format ".")
  (application:stop 'inets)
  (io:format ".")
  (timer:sleep 500)
  (io:format ".~n")
  (log-notice "undertone shutdown complete")
  (init:stop)
  (io:format "~n"))

(defun start-xt-repl ()
  (undertone.repl.extempore:start))
