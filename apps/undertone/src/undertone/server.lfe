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
  ;; repl-history API
  (export
    (session 1)
    (session-banner 0)
    (session-insert 1)
    (session-list 0)
    (session-show-max 0)
    (session-table 0))
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
  `#m(backend ,(undertone.sysconfig:backend)
      prompt ,(undertone.sysconfig:prompt)
      session ,(maps:merge
                (undertone.sysconfig:session)
               `#m(banner #m(file ,(undertone.sysconfig:banner-file)
                             text ,(undertone.sysconfig:banner))))
      version #m(all ,(undertone.sysconfig:versions)
                 backend-display ,(undertone.sysconfig:backend-display-version)
                 system ,(undertone.sysconfig:version-system)
                 undertone ,(undertone.sysconfig:version 'undertone))))

(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

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

(defun init
  (((= `#m(session ,sess) state))
   (let ((table (mref sess 'table)))
     (ets:new table (mref sess 'options))
     (log-debug (maps:merge #m(text "REPL history table info")
                            (maps:from_list (ets:info table)))))
   `#(ok ,state)))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_call
  ;; Backend support
  ((`#(backend display-name) _from (= `#m(backend ,back) state))
   `#(reply ,(mref back 'display-name) state))
  ((`#(backend display-version) _from (= `#m(backend ,back) state))
   `#(reply ,(mref back 'display-name) state))
  ((`#(backend name) _from (= `#m(backend ,back) state))
   `#(reply ,(mref back 'name) state))
  ;; Metadata support
  ((`#(version all) _from (= `#m(version ,ver) state))
   `#(reply ,(mref ver 'all) state))
  ;; Session support
  ((`#(session banner) _from (= `#m(session ,sess) state))
   `#(reply ,(clj:get-in sess '(session banner text)) state))
  ((`#(session show-max) _from (= `#m(session ,sess) state))
   `#(reply ,(mref sess 'show-max) state))
  ((`#(session table) _from (= `#m(session ,sess) state))
   `#(reply ,(mref sess 'table) state))
  ;; Stop
  (('stop _from (= `#m(session ,sess) state))
   (ets:delete (mref sess 'table))
   `#(stop normal ok state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command) ,state)))

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

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   session API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun session (idx)
  (let ((latest (ets:last (session-table))))
    (session idx 1 latest)))

(defun session (dest-idx current-idx current-key)
  (cond
   ((< dest-idx 0) "error: history index must be positive")
   ((== dest-idx 0) (session 1 current-idx current-key))
   ((== current-key 'prev-not-found) "error: no history for given index")
   ((== dest-idx current-idx) (ets:lookup (session-table) current-key))
   ('true (session dest-idx (+ current-idx 1) (session-prev current-key)))))

(defun session-banner ()
  (gen_server:call (SERVER) #(session banner)))

(defun session-insert (data)
  (ets:insert (session-table) `#(,(erlang:monotonic_time) ,data)))

(defun session-list ()
  (ets:tab2list (session-table)))

(defun session-prev (key)
  (try
    (ets:prev (session-table) key)
    (catch (`#(error badarg ,trace)
      (parse-ets-errors trace)))))

(defun session-show-max ()
  (gen_server:call (SERVER) #(session show-max)))

(defun session-table ()
  (gen_server:call (SERVER) #(session table)))

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   history API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TBD

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-ets-errors (trace)
  (case trace
    (`(#(ets prev (,_ $end_of_table) ,_) . ,_) 'prev-not-found)
    (_ (progn
         (log-error (maps:from_list trace))
         'error))))
