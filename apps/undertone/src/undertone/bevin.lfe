;;;; The gen_server responsible for managing the OS processes that support the
;;;; "Bevin" backend for undertone.
(defmodule undertone.bevin
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
   (recv-responsive? 0)
   (recv-os-process-alive? 0)
   (recv-port-alive? 0)
   (send-responsive? 0)
   (send-os-process-alive? 0)
   (send-port-alive? 0)
   (healthy? 0)
   (status 0))
  ;; debug API
  (export
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/notes/data.lfe")

(defun SERVER () (MODULE))

(defun initial-state ()
  (let* ((opts (list "--"))
         (backend (undertone.sysconfig:backend))
         (root-dir (mref backend 'root-dir)))
    `#m(backend ,backend
        args opts
        recv #m(binary ,(++ root-dir (mref backend 'recv-binary))
                os-pid undefined
                os-pid-str undefined)
        send #m(binary ,(++ root-dir (mref backend 'send-binary))
                os-pid undefined
                os-pid-str undefined))))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))
(defun unknown-info (data)
  `#(error ,(lists:flatten (++ "Unknown info: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undertone 'Bevin' server ...")
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
  `#(ok ,(maps:merge state (start-bevin state))))

(defun handle_cast
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Health
  ((`#(status bevin) _from (= `#m(tcp-port ,port) state))
   `#(reply not-implemented ,state))
  ((`#(status os-process) _from (= `#m(os-pid-str ,os-pid) state))
   `#(reply ,(has-str? (ps-pid os-pid) os-pid) ,state))
  ((`#(status port) _from (= `#m(port ,port) state))
   `#(reply ,(erlang:is_port port) ,state))
  ((`#(status all) _from state)
   `#(reply
      `#m(recv-port-alive? ,(recv-port-alive?)
          recv-os-process-alive? ,(recv-os-process-alive?)
          recv ,(recv-responsive?)
          send-port-alive? ,(send-port-alive?)
          send-os-process-alive? ,(send-os-process-alive?)
          send ,(send-responsive?))
      ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping the 'Bevin' backend ...")
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
   ;; XXX Update MIDI receive message-parsing for this backend:
   ;;(let* ((packed (clj:-> rest
   ;;                       (binary_to_list)
   ;;                       (lists:sublist 1 (- (size rest) 1))
   ;;                       (list_to_integer)))
   ;;       (unpacked (midi-hash->map packed)))
   ;;  (log-debug "packed: ~p" `(,packed))
   ;;  (log-debug "note: ~p - channel: ~p - pitch: ~p - velocity: ~p - time: ~p"
   ;;             (list (mref unpacked 'note-state)
   ;;                   (mref unpacked 'channel)
   ;;                   (mref unpacked 'pitch)
   ;;                   (mref unpacked 'velocity)
   ;;                   (mref unpacked 'time)))
     ;; XXX we'll eventually communicate with a "recording" gen_server which
     ;;     will, in turn, write the data to an ETS table; for more details,
     ;;     see: https://github.com/ut-proj/undertone/issues/64
   ;;  (log-notice unpacked)
     `#(noreply ,state))
  ;; Port EOL-based messages
  ((`#(,port #(data #(eol ,msg))) state) (when (is_port port))
   (log-info (sanitize-msg msg))
   `#(noreply ,state))
  ;; Port line-based messages
  ((`#(,port #(data #(,line-msg ,msg))) state) (when (is_port port))
   (log-info "Unknown line message:~p~s" `(,line-msg ,(sanitize-msg msg)))
   `#(noreply ,state))
  ;; General port messages
  ((`#(,port #(data ,msg)) state) (when (is_port port))
   (log-info "Message from 'Bevin' port:~n~s" `(,(sanitize-msg msg)))
   `#(noreply ,state))
  ;; Exit-handling
  ((`#(,port #(exit_status ,exit-status)) state) (when (is_port port))
   (log-warn "~p: exited with status ~p" `(,port ,exit-status))
   `#(noreply ,state))
  ((`#(EXIT ,_from normal) state)
   (logger:info "The 'Bevin' backend server is exiting (normal).")
   `#(noreply ,state))
  ((`#(EXIT ,_from shutdown) state)
   (logger:info "The 'Bevin' backend server is exiting (shutdown).")
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)" `(,pid ,reason))
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug "Unknwon info: ~p" `(,msg))
   `#(noreply ,state)))

(defun terminate
  ((_reason `#m(port ,port os-pid-str ,os-pid))
   (log-notice "Terminating the 'Bevin' backend server ...")
   (catch (erlang:port_close port))
   (catch (stop-os-process os-pid))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   management API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-bevin
  ((`#m(recv ,recv-state send ,send-state args ,args))
   `#m(recv ,(start-port recv-state args)
       send ,(start-port send-state args))))

(defun spawn-executable (bin args)
  (let* ((port (erlang:open_port `#(spawn_executable ,bin)
                                 `(binary
                                   use_stdio
                                   exit_status
                                   #(line 1024)
                                   #(args ,args))))
         (port-info (erlang:port_info port))
         (os-pid (proplists:get_value 'os_pid port-info)))
    `#m(port ,port
        port-info ,port-info
        os-pid ,os-pid
        os-pid-str ,(integer_to_list os-pid))))

(defun start-port
  (((= `#m(binary ,bin) port-state) args)
   (maps:merge port-state (spawn-executable bin args))))

(defun stop-os-process (pid-str)
  (os:cmd (++ "kill " pid-str)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   health API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recv-responsive? ()
  (gen_server:call (SERVER) #(status recv)))

(defun recv-os-process-alive? ()
  (gen_server:call (SERVER) #(status recv-os-process)))

(defun recv-port-alive? ()
  (gen_server:call (SERVER) #(status recv-port)))

(defun send-responsive? ()
  (gen_server:call (SERVER) #(status send)))

(defun send-os-process-alive? ()
  (gen_server:call (SERVER) #(status send-os-process)))

(defun send-port-alive? ()
  (gen_server:call (SERVER) #(status send-port)))

(defun healthy? ()
  (let ((vals (maps:values (status))))
    (not (lists:member 'false vals))))

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

(defun has-str? (string pattern)
  (case (string:find string pattern)
    ('nomatch 'false)
    (_ 'true)))

(defun ps-pid (pid)
  (os:cmd (++ "ps -o pid -p" pid)))

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

(defun sanitize-msg (msg)
  msg)

;;; scratch

