;;;; Functions for sending messages to the Extempore TCP server.
(defmodule xt.msg
  (export
   (async 1)
   (payload 1)
   (sync 1)))

(include-lib "logjam/include/logjam.hrl")

(defun async (str)
  (log-debug "Casting ~s ... ~n" `(,str))
  (tcp-client:cast-msg (payload str)))

(defun payload (str)
  (binary:list_to_bin (++ str "\r\n")))

(defun sync (str)
  (log-debug "Calling ~s ... ~n" `(,str))
  (tcp-client:call-msg (payload str)))
