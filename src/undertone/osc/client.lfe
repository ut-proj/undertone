;;;; This is not a general-purpose OSC client, but rather an LFE client for
;;;; the Erlang OSC server (default implementation).
;;;;
;;;; A general-purpose OSC client is available here:
;;;; * https://github.com/erlsci/osc/tree/release/2.1.x/src
(defmodule undertone.osc.client
  ;; constructors
  (export
   (connect 2) (connect 3))
  ;; common
  (export 
   (conns 1)
   (ping 1))
  ;; impl-specific
  (export
   (echo 1) (echo 2)))

(include-lib "include/client.lfe")

;;; Constructor

(defun connect (host port)
  (application:ensure_started 'osc_lib)
  (connect host port (application:get_env 'osc_lib 'udp_opts '())))

(defun connect (host port udp-opts)
  (let ((`#(ok ,sup) (osc_client:start))
        (`#(ok ,pid) (osc_client:connect host port udp-opts)))
    (make-client sup sup pid pid)))

;;; Common Functions

(defun conns (_)
   (client-conns))

(defun ping (client)
  (client-ping client))

;;; Implementation-specific Functions

(defun echo
  (((match-client pid p))
   (osc_client:cast_msg p "/debug/log_message")))

(defun echo
  (((match-client pid p) args)
   (osc_client:cast_msg p "/debug/log_message" args)))

