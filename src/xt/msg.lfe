;;;; Functions for sending messages to the Extempore TCP server.
(defmodule xt.msg
  ;; API
  (export
   (async 1)
   (parse-response 2)
   (payload 1)
   (report 1)
   (sync 1))
  ;; Utilities
  (export
   (split-xt-packet 1)))

(include-lib "logjam/include/logjam.hrl")

(defun xmit-delim () "\r\n")
(defun rcv-delim () #b(0))

(defun async (str)
  (log-debug "Casting: ~s" `(,str))
  (tcp-client:cast-msg (payload str)))

(defun payload (str)
  (binary:list_to_bin (++ str (xmit-delim))))

(defun sync (str)
  (log-debug "Calling: ~s" `(,str))
  (tcp-client:call-msg (payload str)))

;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Callbacks for tcp-client library   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-packet (parsed)
  (case parsed
    ("Welcome to extempore!" (progn
                               (timer:sleep 500)
                               (render-banner)))
    ("#(health ok)" (log-notice "Extempore TCP server connection healthy"))
    (_ (progn
         (log-debug "No special packet handling for parsed value")
         'ok))))

(defun parse-response
  ((packet `#(,reporter-mod ,reporter-func))
   (log-debug "Got packet: ~p" `(,packet))
   (let* ((raw-msgs (split-xt-packet packet))
          (msgs (maybe-one-msg raw-msgs)))
     (list-comp
       ((<- x raw-msgs))
       (apply reporter-mod reporter-func `(,x)))
     (log-debug "Parsed packet: ~p" `(,msgs))
     (handle-packet msgs)
     msgs)))

(defun report (data)
  (log-debug "Got data from TCP server: ~p" `(,data))
  'ok)

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-one-msg
  ((`(,msg . ()))
    msg)
   ((msgs)
    msgs))

(defun render-banner ()
  (io:put_chars `(,(undertone.server:session-banner))))

(defun split-xt-packet (packet)
  (list-comp
    ((<- x (when (=/= x #b()))
         (binary:split packet (rcv-delim) '(global))))
    (xt.lang:->lfe x)))

