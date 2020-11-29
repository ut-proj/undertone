;;;; This is not a general-purpose OSC client, but rather an LFE client for
;;;; the Erlang OSC server (default implementation).
;;;;
;;;; The intent for this module is that it be a low-level, very thin wrapper
;;;; around `osc_client` and that there will be a higher-level API at a
;;;; later point in time at `undertone.osc`.
;;;;
;;;; A general-purpose OSC client is available here:
;;;; * https://github.com/erlsci/osc/tree/release/2.1.x/src
(defmodule undertone.osc.client
  ;; constructors
  (export
   (connect 0) (connect 1) (connect 2) (connect 3))
  ;; common
  (export 
   (conns 1)
   (ping 1))
  ;; impl-specific
  (export
   (echo 1) (echo 2)))

(include-lib "include/client.lfe")

(defun default-host () "127.0.0.1")
(defun default-port () 2357)

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

(defun echo
  (((match-client pid p))
   (osc_client:cast_msg p "/debug/log_message")))

(defun echo
  (((match-client pid p) args)
   (osc_client:cast_msg p "/debug/log_message" args)))

