(defmodule undertone.app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (start_phase 3)
   (stop 0))
  ;; start phases
  (export
   (render-banner 2)
   (start-backend-client 2)
   (start-osc-clients 2)
   (start-undertone-inets 2)
   (stop-rebar-inets 2)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   application implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (type args)
  (logger:add_handlers 'undertone)
  (log-notice "Starting undertone application ...")
  (log-debug `#m(msg "App start data" type ,type args ,args))
  (undertone.sup:start_link))

(defun stop ()
  (log-notice "Stopping undertone application ...")
  'ok)

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   start phases   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_phase(phase type args)
  (call (MODULE) phase type args)
  'ok)

(defun stop-rebar-inets (_type _args)
  (log-info "Stopping the inets started by rebar3 ...")
  (inets:stop)
  'ok)

(defun start-undertone-inets (_type _args)
  (log-info "Restarting inets with undertone config ...")
  (inets:start)
  'ok)

(defun start-backend-client (type args)
  (log-info "Starting backend client ...")
  (log-debug `#m(msg "Backend client start data"
                 type ,type
                 args ,args))
  (let* ((cfg (undertone.sysconfig:backend)))
    (log-debug `#m(msg "Backend config data"
                   cfg ,cfg))
    (start-backend-client type args cfg)))

(defun start-backend-client
  ((_type _args `#m(has-client? ,has-client?)) (when (not has-client?))
   (log-info "there is no client for the given backend; skipping ...")
   'ok)
  ((_type _args `#m(start-client? ,start?)) (when (not start?))
   (log-info "undertone is not configured to start a client for the given backend; skipping ...")
   'ok)
  ((_type _args (= `#m(name ,name host ,host port ,port) cfg))
   (log-debug "Config for backend client: ~p" `(,cfg))
   (case name
     ;; XXX Fix this to use configured host and port
     ;('extempore (xt:connect host port))
     ('extempore (xt:connect))
     (_ #(error (io_lib:format "No supported client for backend: ~p" `(,name)))))))

;; XXX Implmenent start-up for OSC clients
(defun start-osc-clients (type args)
  (log-info "Starting Open Sound Control clients ...")
  (log-debug `#m(msg "OSC client start data"
                 type ,type
                 args ,args))
  'ok)

(defun render-banner (type args)
  (render-banner type args (undertone.sysconfig:backend)))

(defun render-banner
  ((type args `#m(banner-render ,render-when))
   (if (== render-when 'at-start)
     (progn
       (log-debug "Rendering banner ...")
       (timer:apply_after 500
                          'undertone.server
                          'render-banner
                          '())))
   'ok))
