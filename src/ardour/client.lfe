;;;; The intent for this module is that it be a low-level, very thin wrapper
;;;; around `osc_client` and that there will be a higher-level API at a
;;;; later point in time at `ardour`.
(defmodule ardour.client
  ;; constructors
  (export
   (connect 0) (connect 1) (connect 2) (connect 3))
  ;; common
  (export
   (conns 1)
   (ping 1))
  ;; impl-specific
  (export
   (deselect-strip 2)
   (goto-end 1)
   (goto-start 1)
   (list-strips 1)
   (list-strip-plugins 2)
   (mute-monitor 1)
   (select-strip 2)
   (strip-plugin-desc 3)
   (unmute-monitor 1)))

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

(defun deselect-strip
  "Deselect the given strip."
  (((match-client pid p) strip-id)
   (osc_client:cast_msg p "/strip/select" `(,strip-id 0))))

(defun goto-end
  "Move playhead to end of session."
  (((match-client pid p))
   (osc_client:cast_msg p "/goto_end")))

(defun goto-start
  "Move playhead to start of session."
  (((match-client pid p))
   (osc_client:cast_msg p "/goto_start")))

(defun list-strips
  "Ask for a list of strips.

  XXX - Due to a current limitation of the UDP client, the full list of strips
        is not obtained, and neither is the final message."
  (((match-client pid p))
   (let ((result (osc_client:call_msg p "/strip/list")))
     (format-msg (erlang:setelement 2 result "/strip/list")))))

(defun list-strip-plugins
  "Ask for a list of a strip's plugins.

  XXX - Due to a current limitation of the UDP client, the full list of plugins
        is not obtained."
  (((match-client pid p) strip-id)
   (format-msg (osc_client:call_msg p "/strip/plugin/list" `(,strip-id)))))

(defun mute-monitor
  (((match-client pid p))
   (osc_client:cast_msg p "/monitor/mute" '(1))))

(defun select-strip
  "Select the given strip."
  (((match-client pid p) strip-id)
   (osc_client:cast_msg p "/strip/select" `(,strip-id 1))))

(defun strip-plugin-desc
  "Ask for a list of a plug-in's parameters.

  XXX - Due to a current limitation of the UDP client, the full list of plugin
        parameters is not obtained."
  (((match-client pid p) strip-id plugin-id)
   (format-msg (osc_client:call_msg p "/strip/plugin/descriptor"
                                    `(,strip-id ,plugin-id)))))

(defun unmute-monitor
  (((match-client pid p))
   (osc_client:cast_msg p "/monitor/mute" '(0))))

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