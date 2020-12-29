;;; Client data structure
(defrecord client
  sup
  pid)

;;; Client constructors common to all implementations:
(defun new-connection (host port)
  (application:ensure_started 'osc_lib)
  (connect host port (application:get_env 'osc_lib 'udp_opts '())))

(defun new-connection (host port udp-opts)
  (let ((`#(ok ,sup) (osc_client:start))
        (`#(ok ,pid) (osc_client:connect host port udp-opts)))
    (make-client sup sup pid pid)))

;;; Client functions common to all implementations:
(defun client-ping
  (((match-client pid p))
   (osc_client:ping p)))

(defun client-conns ()
   (osc_client_mgr_sup:list_conns))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: osc/client --| ()
  'ok)