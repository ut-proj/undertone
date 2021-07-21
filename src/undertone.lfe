(defmodule undertone
  (export
   (quit 0)))

(include-lib "logjam/include/logjam.hrl")

(defun quit ()
  (log-info "Disconnecting TCP client ...")
  (application:stop 'tcp-client)
  (timer:sleep 500)
  (log-info "Stopping undertone services ...")
  (application:stop 'undertone)
  (timer:sleep 500)
  (application:stop 'undertone-lib)
  (timer:sleep 500)
  (log-info "Stopping support services ...")
  (application:stop 'loise)
  (timer:sleep 500)
  (application:stop 'inets)
  (timer:sleep 500)
  (log-notice "undertone shutdown complete")
  (init:stop))
