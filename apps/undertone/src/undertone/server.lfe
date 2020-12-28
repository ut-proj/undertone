(defmodule undertone.server
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start_link 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; repl-history API
    (history 1)
    (history-insert 1)
    (history-list 0)
    ;; debug API
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun initial-state () '#())
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;; XXX put this in configuration
(defun hist-table () 'replhistory)

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
  (ets:new (hist-table) '(ordered_set named_table public))
  (log-debug (maps:merge #m(text "REPL history table info")
                         (maps:from_list (ets:info (hist-table)))))
  `#(ok ,state))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_call
  (('stop _from state)
   (ets:delete (hist-table))
   `#(stop normal ok state))
  ;; For sanity-checking / testing
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
;;;::=-   session API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun history (idx)
  (let ((latest (ets:last (hist-table))))
    (history idx 1 latest)))

(defun parse-ets-errors (trace)
  (case trace
    (`(#(ets prev (,_ $end_of_table) ,_) . ,_) 'prev-not-found)
    (_ (progn
         (log-error (maps:from_list trace))
         'error))))

(defun get-prev-hist (key)
  (try
    (ets:prev (hist-table) key)
    (catch (`#(error badarg ,trace)
      (parse-ets-errors trace)))))

(defun history (dest-idx current-idx current-key)
  (cond
   ((< dest-idx 0) "error: history index must be positive")
   ((== dest-idx 0) (history 1 current-idx current-key))
   ((== current-key 'prev-not-found) "error: no history for given index")
   ((== dest-idx current-idx) (ets:lookup (hist-table) current-key))
   ('true (history dest-idx (+ current-idx 1) (get-prev-hist current-key)))))

(defun history-insert (data)
  (ets:insert (hist-table) `#(,(erlang:monotonic_time) ,data)))

(defun history-list ()
  (ets:tab2list (hist-table)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))
