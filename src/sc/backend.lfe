(defmodule sc.backend
  (export
   (start 1)))

(include-lib "logjam/include/logjam.hrl")

(defun start (cfg)
  (log-notice "Starting SuperCollider backend ...")
  'ok)