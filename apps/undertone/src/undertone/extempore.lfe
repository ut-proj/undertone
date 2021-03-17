(defmodule undertone.extempore
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
  ;; health API
  (export
   (extempore-responsive? 0)
   (healthy? 0)
   (os-process-alive? 0)
   (status 0))
  ;; debug API
  (export
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/notes/data.lfe")
(include-lib "include/xt/options.lfe")

(defun SERVER () (MODULE))

(defun initial-state ()
  (let* ((opts (server-options))
         (backend (undertone.sysconfig:backend))
         (root-dir (mref backend 'root-dir)))
    `#m(opts ,opts
        backend ,backend
        tcp-port (mref backend 'port)
        binary ,(++ root-dir (mref backend 'binary))
        args ,(lists:append
               (server-options->strings opts)
               (list (++ "--sharedir=" root-dir)))
        pid undefined
        os-pid undefined)))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))
(defun unknown-info (data)
  `#(error ,(lists:flatten (++ "Unknown info: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undertone Extempore server ...")
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
  (erlang:process_flag 'trap_exit 'true)
  `#(ok ,(maps:merge state (start-extempore state))))

(defun handle_cast
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Health
  ((`#(status extempore) _from (= `#m(tcp-port ,port) state))
   `#(reply not-implemented ,state))
  ((`#(status os-process) _from (= `#m(os-pid ,os-pid) state))
   `#(reply ,(ut.os:ps-alive? os-pid) ,state))
  ((`#(status all) _from  (= `#m(recv ,recv send ,send) state))
   `#(reply
      #m(recv-alive? ,(ut.os:ps-alive? (mref recv 'os-pid))
         send-alive? ,(ut.os:ps-alive? (mref send 'os-pid)))
      ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping Extempore ...")
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_info
  ;; Extract MIDI data from packed bits
  ((`#(,port #(data #(eol ,(binary (prefix bytes (size 7)) (rest bitstring))))) state)
   (when (andalso (is_port port) (=:= prefix #"#(MIDI ")))
   (let* ((packed (clj:-> rest
                          (binary_to_list)
                          (lists:sublist 1 (- (size rest) 1))
                          (list_to_integer)))
          (unpacked (midi-hash->map packed)))
     (log-debug "packed: ~p" `(,packed))
     (log-debug "note: ~p - channel: ~p - pitch: ~p - velocity: ~p - time: ~p"
                (list (mref unpacked 'note-state)
                      (mref unpacked 'channel)
                      (mref unpacked 'pitch)
                      (mref unpacked 'velocity)
                      (mref unpacked 'time)))
     ;; XXX we'll eventually communicate with a "recording" gen_server which
     ;;     will, in turn, write the data to an ETS table; for more details,
     ;;     see: https://github.com/ut-proj/undertone/issues/64
     (log-notice unpacked)
     `#(noreply ,state)))
  ;; Standard-output messages
  ((`#(stdout ,_pid ,msg) state)
   (let ((msg (string:replace (sanitize-extempore-msg msg) "\"" "")))
     (log-debug `#m(message ,msg))
     (case (match-stdout-map msg)
       (`#(match (() ,map-str)) (case (lfe_io:read_string map-str)
                                  (`#(ok ,map-data) (handle-stdout-data map-data))
                                  (err (log-error `#m(error ,err data ,map-str)))))
       ('nomatch (log-info "Extempore: ~s" `(,msg)))))
   `#(noreply ,state))
  ;; Port EOL-based messages
  ((`#(,port #(data #(eol ,msg))) state) (when (is_port port))
   (log-info (sanitize-extempore-msg msg))
   `#(noreply ,state))
  ;; Port line-based messages
  ((`#(,port #(data #(,line-msg ,msg))) state) (when (is_port port))
   (log-info "Unknown line message:~p~s" `(,line-msg ,(sanitize-extempore-msg msg)))
   `#(noreply ,state))
  ;; General port messages
  ((`#(,port #(data ,msg)) state) (when (is_port port))
   (log-info "Message from Extempore port:~n~s" `(,(sanitize-extempore-msg msg)))
   `#(noreply ,state))
  ;; Exit-handling
  ((`#(,port #(exit_status ,exit-status)) state) (when (is_port port))
   (log-warn "~p: exited with status ~p" `(,port ,exit-status))
   `#(noreply ,state))
  ((`#(EXIT ,_from normal) state)
   (logger:info "Extempore server is exiting (normal).")
   `#(noreply ,state))
  ((`#(EXIT ,_from shutdown) state)
   (logger:info "Extempore server is exiting (shutdown).")
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)" `(,pid ,reason))
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug "Unknwon info: ~p" `(,msg))
   `#(noreply ,state)))

(defun terminate
  ((_reason `#m(os-pid ,os-pid))
   (log-notice "Terminating Extempore server ...")
   (catch (exec:stop os-pid))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   management API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-extempore
  (((= `#m(args ,args binary ,bin) state))
   (maps:merge state (ut.os:run bin args))))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   health API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extempore-responsive? ()
  (gen_server:call (SERVER) #(status extempore)))

(defun healthy? ()
  (let ((vals (maps:values (status))))
    (not (lists:member 'false vals))))

(defun os-process-alive? ()
  (gen_server:call (SERVER) #(status os-process)))

(defun status ()
  (gen_server:call (SERVER) #(status all)))

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

(defun sanitize-extempore-msg (msg)
  ;;(log-debug "Binary message: ~p" `(,msg))
  (clj:-> msg
          (binary_to_list)
          (string:replace "\\" "")
          (string:trim)))

(defun midi-hash->map (hash)
  (let* ((note-state (band hash #b1))
         (channel (band (bsr hash 1) #b1111))
         (pitch (band (bsr hash 5) #b111111))
         (velocity (band (bsr hash 12) #b111111))
         (time (band (bsr hash 19) #b11111111111111111111)))
    `#m(note-state ,(if (== note-state 1) 'on 'off)
        channel ,channel
        pitch ,pitch
        note ,(lookup-midi pitch)
        velocity ,velocity
        time ,time)))

(defun stdout-map-regex ()
  "(^\\\[\\\")*(?<MAP>['`]*#[mM][^\\\"\\\]]+)(\"\])*$")

(defun match-stdout-map (string)
  (re:run string (stdout-map-regex) '(#(capture 'MAP list))))

(defun handle-stdout-data
  (((= `#m(type sequence-step
           data #m(beat ,beat
                   note ,note
                   note-duration ,dur)) map-data))
   (log-notice `#m(data ,map-data))
   (log-warn "XXX: Save to ETS table!"))
  ((msg)
   (log-warn "Unhandled data format: ~p" `(,msg))))