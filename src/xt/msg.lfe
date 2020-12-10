;;;; Functions for sending messages to the Extempore TCP server.
(defmodule xt.msg
  (export
   (async 1)
   (payload 1)
   (sync 1)))

(include-lib "logjam/include/logjam.hrl")

(defun async (bin)
  (log-debug "Casting ~p ... ~n" bin)
  (tcp-client:cast-msg (payload bin)))

(defun payload (str)
  (binary:list_to_bin (++ str "\r\n")))

(defun sync (bin)
  (log-debug "Calling ~p ... ~n" bin)
  (tcp-client:call-msg (payload bin)))
