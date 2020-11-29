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
  (export
   (strip-list 1)
   (strip-plugin-list 2)
   (strip-plugin-desc 3)))

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

(defun strip-list
  "Ask for a list of strips.

  XXX - Due to a current limitation of the UDP client, the full list of strips
        is not obtained, and neither is the final message."
  (((match-client pid p))
   (let ((result (osc_client:call_msg p "/strip/list")))
     (format-msg (erlang:setelement 2 result "/strip/list")))))

(defun strip-plugin-list
  "Ask for a list of a strip's plugins.

  XXX - Due to a current limitation of the UDP client, the full list of plugins
        is not obtained."
  (((match-client pid p) strip-id)
   (format-msg (osc_client:call_msg p "/strip/plugin/list" `(,strip-id)))))

(defun strip-plugin-desc
  "Ask for a list of a plug-in's parameters.

  XXX - Due to a current limitation of the UDP client, the full list of plugin
        parameters is not obtained."
  (((match-client pid p) strip-id plugin-id)
   (format-msg (osc_client:call_msg p "/strip/plugin/descriptor"
                                    `(,strip-id ,plugin-id)))))

;;; Private Functions

(defun toggle-int (type)
  (case type
    ('off 0)
    ('on 1)))

(defun strip-type-name (abbrv)
  (case abbrv
    ("AT" "audio track")
    ("MT" "MIDI track")
    ("B" "audio bus")
    ("MB" "MIDI bus")
    ("FB" "foldback bus")
    ("V" "VCA")))

(defun format-msg
  (((= `#(error ,_) err))
   err)
  ((`#(,_ "/strip/list" (,st ,sn ,si ,so ,m ,s ,sid ,re)))
   `(#(strip-id ,sid)
     #(name ,sn)
     #(type ,(strip-type-name st))
     #(inputs ,si)
     #(outputs ,so)
     #(muted? ,m)
     #(soloed? ,s)
     #(record-enabled? ,re)))
  ((`#(,_ "/strip/plugin/list" (,sid ,pid ,pn ,_unknown)))
   `(#(strip-id ,sid)
     #(plugin-id ,pid)
     #(name ,pn)))
  ((`#(,_ "/strip/plugin/descriptor" (,sid ,pid ,parid ,parn ,bs ,dt ,min ,max ,_ ,_ ,cv)))
   ;; XXX Note that the documentation of the third and second to last args seem
   ;;     to be swapped, but since I'm not sure, I'm just leaving those out.
   `(#(strip-id ,sid)
     #(plugin-id ,pid)
     #(parameter (#(id ,parid)
                  #(name ,parn)
                  #(flags-bitset ,bs)
                  #(data-type ,dt)
                  #(min ,min)
                  #(max ,max)
                  #(current-value ,cv))))))