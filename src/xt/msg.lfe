;;;; Functions for sending messages to the Extempore TCP server.
(defmodule xt.msg
  (export
   (async 1)
   (payload 1)
   (sync 1)))

(include-lib "logjam/include/logjam.hrl")

(defun xmit-delim () "\r\n")
(defun rcv-delim () #b(0))

(defun async (str)
  (log-debug "Casting ~s ..." `(,str))
  (tcp-client:cast-msg (payload str)))

(defun payload (str)
  (binary:list_to_bin (++ str (xmit-delim))))

(defun sync (str)
  (log-debug "Calling ~s ..." `(,str))
  (tcp-client:call-msg (payload str)))

;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Callbacks for tcp-client library   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-response
  ((packet `#(,reporter-mod ,reporter-func))
   (let ((msgs (split-xt-packet packet)))
     (list-comp
       ((<- x msgs))
       (apply reporter-mod reporter-func `(,x)))
     msgs)))

(defun reporter (_data)
  ;; XXX once logging is working, add log msg here
  'ok)

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-xt-packet (packet)
  (list-comp
    ((<- x (when (=/= x #b())) (binary:split packet (rcv-delim) '(global))))
    (xt.lang:->lfe x)))
