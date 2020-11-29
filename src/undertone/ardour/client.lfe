;;;; The intent for this module is that it be a low-level, very thin wrapper
;;;; around `osc_client` and that there will be a higher-level API at a
;;;; later point in time at `undertone.ardour`.
(defmodule undertone.ardour.client
  ;; constructors
  (export
   (connect 0) (connect 1) (connect 2) (connect 3))
  ;; common
  (export
   (conns 1)
   (ping 1))
  ;; impl-specific
  )

(include-lib "include/client.lfe")

(defun default-host () "127.0.0.1")
(defun default-port () 3819)

(defun root-node-id () 0)

;;; Constructors

(defun connect ()
  (connect (default-host)))

(defun connect (host)
  (connect host (default-port)))

(defun connect (host port)
  (new-connection host port))

(defun connect (host port udp-opts)
  (new-connection host port udp-opts))

;;; Common Functions

(defun conns (_)
   (client-conns))

(defun ping (client)
  (client-ping client))

;;; Implementation-specific Functions


;;; Private Functions

(defun toggle-int (type)
  (case type
    ('off 0)
    ('on 1)))

(defun format-msg
  ((`#(message "/version.reply" (,_name ,maj ,min ,micro ,branch ,commit)))
   `(#(version ,(lists:flatten (io_lib:format "~p.~p~s" `(,maj ,min ,micro))))
     #(branch ,branch)
     #(commit-id ,commit)))
  ((`#(message "/status.reply" (,_ ,ug ,s ,g ,lsd ,cau ,cpu ,nsr ,asr)))
   `(#(unit-generators ,ug)
     #(synths ,s)
     #(gruops ,g)
     #(loaded-synth-definitions ,lsd)
     #(cpu-average-usage ,cau)
     #(cpu-peak-usage ,cpu)
     #(nominal-sample-rate ,nsr)
     #(actual-sample-rate ,asr)))
  ((`#(message "/synced" ,_))
   'ok)
  ((`#(message "/done" ("/notify" ,id ,ml)))
   `(#(client-id ,id)
     #(max-logins ,ml))))