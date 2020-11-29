;;;; The intent for this module is that it be a low-level, very thin wrapper
;;;; around `osc_client` and that there will be a higher-level API at a
;;;; later point in time at `undertone.sc`.
(defmodule undertone.sc.client
  ;; constructors
  (export
   (connect 0) (connect 1) (connect 2) (connect 3))
  ;; common
  (export
   (conns 1)
   (ping 1))
  ;; impl-specific
  (export
   (create-synth 2) (create-synth 3) (create-synth 6)
   (delete-node 2)
   (delete-nodes 2)
   (dump 2)
   (notify 2)
   (query-node 2)
   (set-node 3)
   (start-node 2)
   (status 1)
   (stop-node 2)
   (sync 1) (sync 2)
   (toggle-node 3)
   (version 1)))

(include-lib "include/client.lfe")

(defun default-host () "127.0.0.1")
(defun default-port () 57110)
(defun default-synth () "default")

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

(defun create-synth (client id)
  (create-synth client id '()))

(defun create-synth (client id args)
  (create-synth client
                (default-synth)
                id
                'add-tail
                (root-node-id)
                args))

(defun create-synth
  "Creates a new synth and returns the synth id.

  Allowed actions (atoms): add-head, add-tail, prepend, append, replace."
  (((match-client pid p) name id action target-id args)
   (osc_client:cast_msg p "/s_new" (lists:append `(,name
                                                   ,id
                                                   ,(synth-action-id action)
                                                   ,target-id)
                                                 args))
   id))

(defun delete-node
  "Deletes a node."
  (((match-client pid p) id)
   (osc_client:cast_msg p "/n_free" `(,id))))

(defun delete-nodes (client ids)
  (list-comp ((<- id ids))
             (delete-node client id)))                    

(defun dump
  "Allowed types (atoms): off, parsed, hex, both."
  (((match-client pid p) type)
   (osc_client:cast_msg p "/dumpOSC" `(,(dump-id type)))))

(defun query-node
  "Get info about a node."
  (((match-client pid p) id)
   (osc_client:cast_msg p "/n_query" `(,id))))

(defun notify
  "Register to receive notifications from server.

  Toggle values (atoms): on, off."
  (((match-client pid p) toggle)
   (format-msg
    (osc_client:call_msg p "/notify" `(,(toggle-int toggle))))))

(defun set-node
  "Set a node's control value(s)."
  (((match-client pid p) id args)
   (osc_client:cast_msg p "/n_set" (lists:append `(,id) args))))

(defun start-node (client id)
  (toggle-node client id 'on))

(defun status
  (((match-client pid p))
   (format-msg (osc_client:call_msg p "/status"))))

(defun stop-node (client id)
  (toggle-node client id 'off))

(defun sync (client)
  (sync client (rand-id)))

(defun sync
  (((match-client pid p) id)
   (format-msg (osc_client:call_msg p "/sync" `(,id)))))

(defun toggle-node
  (((match-client pid p) id mode)
   (osc_client:cast_msg p "/n_run" `(,id ,(toggle-int mode)))))
  
(defun version
  (((match-client pid p))
   (format-msg (osc_client:call_msg p "/version"))))

;;; Private Functions

(defun toggle-int (type)
  (case type
    ('off 0)
    ('on 1)))

(defun dump-id (type)
  (case type
    ('off 0)
    ('parsed 1)
    ('hex 2)
    ('both 3)))

(defun synth-action-id (action)
  (case action
    ('add-head 0)
    ('add-tail 1)
    ('prepend 2)
    ('append 3)
    ('replace 4)))

(defun rand-id ()
  (rand:uniform 4294967295))

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