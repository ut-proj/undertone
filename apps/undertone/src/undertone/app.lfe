(defmodule undertone.app
  (behaviour gen_server)
  ;; app implementation
  (export  
   (start 2)
   (start_phase 3)
   (stop 0))
  ;; start phases
  (export
   (start-backend 2)
   (start-backend-client 2)
   (start-osc-clients 2)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   application implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (type args)
  (logger:set_application_level 'undertone 'all)
  (log-notice "Starting undertone application ...")
  (log-debug `#m(msg "App start data" type ,type args ,args))
  (undertone.sup:start_link))

(defun stop ()
  (undertone.sup:stop)
  'ok)

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   start phases   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_phase(phase type args)
  (call (MODULE) phase type args)
  'ok)
      
(defun start-backend (type args)
  (log-info "Starting backend ...")
  (log-debug `#m(msg "Backend start data"
                 type ,type
                 args ,args))
  'ok)

(defun start-backend-client (type args)
  (log-info "Starting backend client ...")
  (log-debug `#m(msg "Backend client start data"
                 type ,type
                 args ,args))
  'ok)

(defun start-osc-clients (type args)
  (log-info "Starting Open Sound Control clients ...")
  (log-debug `#m(msg "OSC client start data"
                 type ,type
                 args ,args))
  'ok)
