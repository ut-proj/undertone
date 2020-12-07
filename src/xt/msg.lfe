;;;; Functions for sending messages to the Extempore TCP server.
(defmodule xt.msg
  (export
   (async 1)
   (payload 1)
   (sync 1)))

(defun async (bin)
  ;; XXX replace the following with loggging calls:
  ;;     see: https://github.com/lfex/undertone/issues/16
  (lfe_io:format "Casting ~p ... ~n" `(,bin))
  (tcp-client:cast-msg (payload bin)))

(defun payload (str)
  (binary:list_to_bin (++ str "\r\n")))

(defun sync (bin)
  ;; XXX replace the following with loggging calls:
  ;;     see: https://github.com/lfex/undertone/issues/16
  (lfe_io:format "Calling ~p ... ~n" `(,bin))
  (tcp-client:call-msg (payload bin)))
