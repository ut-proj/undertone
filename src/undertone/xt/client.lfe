(defmodule undertone.xt.client
  (export
   (one-shot 1) (one-shot 3) (one-shot 4)))

(defun default-host () "localhost")
(defun default-port () 7099)
(defun default-timeout () 2000)
(defun default-options () '(#(active once)
                            binary
                            #(keepalive true)
                            #(packet 0)))

(defun one-shot (bytes)
  (one-shot (default-host) (default-port) bytes))

(defun one-shot (host port bytes)
  (one-shot host port bytes (default-options)))

(defun one-shot (host port bytes opts)
  (let* ((`#(ok ,sock) (gen_tcp:connect host port opts))
         (result (gen_tcp:send sock bytes)))
    (receive
      (`#(tcp ,_sock ,msg) (lfe_io:format "Got msg: ~p~n" `(,msg)))
      (msg (lfe_io:format "Got umknown response: ~p~n" `(,msg))))
    (gen_tcp:close sock)
    result))

; application:start('tcp-mgr').
; 'tcp-mgr':cast_msg(<<"(sys:load \"examples/sharedsystem/setup.xtm\")\r\n">>).
; 'tcp-mgr':cast_msg(<<"(:> ascending-scale 4 0 (play syn1 @1 80 dur) (scale 4 8))\r\n">>).