(defmodule undertone.xtrepl
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
  ;; repl config API
  (export
   (autostart-repl? 0)
   (default-repl 0)
   (extempore-banner 0)
   (extempore-prompt 0)
   (prompt 0)
   (set-repl 1))
  ;; repl-session API
  (export
   (session 1)
   ;; XXX remove banner support here
   (session-banner 0)
   (session-insert 1)
   (session-list 0)
   (session-load 1)
   (session-save 1)
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
  (let ((bkend (undertone.sysconfig:backend)))
    `#m(backend ,bkend
        current-repl undefined
        history ,(undertone.sysconfig:history)
        repl ,(undertone.sysconfig:repl)
        session ,(maps:merge
                  (undertone.sysconfig:session)
                  ;; XXX remove the banner and required merge
                 `#m(banner #m(file ,(undertone.sysconfig:banner-file)
                               text ,(undertone.sysconfig:banner bkend)))))))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undertone Extempore REPL server ...")
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

(defun handle_cast
  ((`#(repl set ,repl-id) state)
   `#(noreply ,(mset state 'current-repl repl-id)))
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; REPL support
  ((`#(repl autostart?) _from (= `#m(repl ,repl) state))
   `#(reply ,(mref repl 'autostart) ,state))
  ((`#(repl default) _from (= `#m(repl ,repl) state))
   `#(reply ,(mref repl 'default) ,state))
  ((`#(repl extempore-banner) _from (= `#m(repl ,repl) state))
   `#(reply ,(clj:get-in repl '(extempore banner text)) ,state))
  ((`#(repl extempore-prompt) _from (= `#m(repl ,repl) state))
   `#(reply ,(clj:get-in repl '(extempore prompt)) ,state))
  ((`#(repl get-prompt) _from (= `#m(current-repl ,cur-repl repl ,repl) state))
   `#(reply ,(clj:get-in repl `(,cur-repl prompt)) ,state))
  ;; Session support
  ;; XXX remove banner support here
  ((`#(session banner) _from (= `#m(session ,sess) state))
   `#(reply ,(clj:get-in sess '(banner text)) ,state))
  ((`#(session show-max) _from (= `#m(session ,sess) state))
   `#(reply ,(mref sess 'show-max) ,state))
  ((`#(session table) _from (= `#m(session ,sess) state))
   `#(reply ,(mref sess 'table) ,state))
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

;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   REPL API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autostart-repl? ()
  (gen_server:call (SERVER) `#(repl autostart?)))

(defun default-repl ()
  (gen_server:call (SERVER) `#(repl default)))

(defun extempore-banner ()
  (gen_server:call (SERVER) `#(repl extempore-banner)))

(defun extempore-prompt ()
  (gen_server:call (SERVER) `#(repl extempore-prompt)))

(defun prompt()
  (gen_server:call (SERVER) `#(repl get-prompt)))

(defun set-repl (repl-id)
  (gen_server:cast (SERVER) `#(repl set ,repl-id)))

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   session API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun session (idx)
  (let ((latest (ets:last (session-table))))
    (session idx 1 latest)))

(defun session (dest-idx current-idx current-key)
  (cond
   ((< dest-idx 0) "error: session command index must be positive")
   ((== dest-idx 0) (session 1 current-idx current-key))
   ((== current-key 'prev-not-found) "error: no session command for given index")
   ((== dest-idx current-idx) (ets:lookup (session-table) current-key))
   ('true (session dest-idx (+ current-idx 1) (session-prev current-key)))))

;; XXX remove this in favor of a backend-agnotist banner renderer
(defun session-banner ()
  (gen_server:call (SERVER) #(session banner)))

(defun session-insert (data)
  (ets:insert (session-table) `#(,(erlang:monotonic_time) ,data)))

(defun session-list ()
  (ets:tab2list (session-table)))

(defun session-load (file)
  (ets:delete (session-table))
  (ets:file2tab (session-table) file))

(defun session-prev (key)
  (try
    (ets:prev (session-table) key)
    (catch (`#(error badarg ,trace)
      (parse-ets-errors trace)))))

(defun session-save (file)
  (ets:tab2file (session-table) file))

(defun session-show-max ()
  (gen_server:call (SERVER) #(session show-max)))

(defun session-table ()
  ;; XXX check for current / active session
  ;; XXX use that to get the session table name
  (gen_server:call (SERVER) #(session table)))

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   history API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun history-table ()
  "The history record and presented is specific for the given session, in other
  words, the history type is dicated by the current session."
  ;; XXX check for current / active session
  ;; XXX use that to get the history table name
  (gen_server:call (SERVER) #(history table)))

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
