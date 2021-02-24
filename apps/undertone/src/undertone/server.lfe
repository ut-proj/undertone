(defmodule undertone.server
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; backend API
  (export
   (backend-display-name 0)
   (backend-display-version 0)
   (backend-name 0))
  ;; metadata API
  (export
   (versions 0))
  ;; misc API
  (export
   (render-banner 0))
  ;; debug API
  (export
   (pid 0)
   (echo 1)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun initial-state ()
  (let ((bkend (undertone.sysconfig:backend)))
  `#m(backend ,bkend
      banner ,(undertone.sysconfig:banner bkend)
      current-repl undefined
      version #m(all ,(undertone.sysconfig:versions bkend)
                 backend-display ,(undertone.sysconfig:backend-display-version bkend)
                 system ,(undertone.sysconfig:version-system)
                 undertone ,(undertone.sysconfig:version 'undertone)))))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undertone state management server ...")
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   callback implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (state)
  `#(ok ,state))

(defun handle_cast
  ;; Misc support
  ((`#(banner) (= `#m(backend ,back banner ,bnr version ,ver) state))
     (io:format "~s" `(,(string:replace bnr "VERSION" (-display-version back ver))))
   `#(noreply ,state))
  ;; Session-related
  ((`#(repl set ,repl-id) state)
   `#(noreply ,(mset state 'current-repl repl-id)))
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Backend support
  ((`#(backend display-name) _from (= `#m(backend ,back) state))
   `#(reply ,(mref back 'display-name) ,state))
  ((`#(backend display-version) _from (= `#m(backend ,back version ,ver) state))
     `#(reply ,(-display-version back ver) ,state))
  ((`#(backend name) _from (= `#m(backend ,back) state))
   `#(reply ,(mref back 'name) ,state))
  ;; Metadata support
  ((`#(version all) _from (= `#m(version ,ver) state))
   `#(reply ,(mref ver 'all) ,state))
  ;; Stop
  (('stop _from (= `#m(session ,sess) state))
   (ets:delete (mref sess 'table))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   backend API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun backend-display-name ()
  (gen_server:call (SERVER) `#(backend display-name)))

(defun backend-display-version ()
  (gen_server:call (SERVER) `#(backend display-version)))

(defun backend-name ()
  (gen_server:call (SERVER) `#(backend name)))

;;;;;::=----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   metadata API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun versions ()
  (gen_server:call (SERVER) `#(version all)))

;;;;;::=----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   misc API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-banner ()
  (gen_server:cast (SERVER) `#(banner)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -display-version (back ver)
  (let* ((name (mref back 'name))
         (vsn (cond ((== name 'bevin) (undertone.bevin:display-version))
                    ((== name 'extempore) (mref ver 'backend-display))
                    ('true "NA"))))
    (lists:flatten vsn)))
