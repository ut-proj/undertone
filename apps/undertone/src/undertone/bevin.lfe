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
    (handle_continue 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3))
  ;; general
  (export
   (display-version 0)
   (version 0))
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
  ;; MIDI API
  (export
   (write-midi 1))
  ;; debug API
  (export
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/notes/data.lfe")

(defun SERVER () (MODULE))

(defun initial-state ()
  (let* ((backend (undertone.sysconfig:backend))
         (recv-name (mref backend 'recv-binary))
         (send-name (mref backend 'send-binary))
         (root-dir (mref backend 'root-dir)))
    `#m(backend ,backend
        args ,(list "--")
        banner ,(undertone.sysconfig:banner backend)
        recv #m(name ,recv-name
                binary ,(++ root-dir recv-name)
                os-pid undefined
                version undefined)
        send #m(name ,send-name
                binary ,(++ root-dir send-name)
                os-pid undefined
                version undefined))))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))
(defun unknown-continue (data)
  `#(error ,(lists:flatten (++ "Unknown continue: " data))))
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
  `#(ok ,(maps:merge state (start-bevin state))
        #(continue #(post-init))))

(defun handle_cast
  ;; MIDI
  ((`#(midi-write ,data) (= `#m(send ,send) state))
   (exec:send (mref send 'os-pid) (list data "\n"))
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug "Unknown cast message: ~p" `(,msg))
   `#(noreply ,state)))

(defun handle_call
  ;; General
  ((`#(version display) _from (= `#m(recv ,recv send ,send) state))
   `#(reply ,(io_lib:format "~s ~s / ~s ~s"
                            (list (mref send 'name)
                                  (mref send 'version)
                                  (mref recv 'name)
                                  (mref recv 'version)))
            ,state))
  ((`#(version) _from (= `#m(recv ,recv send ,send) state))
   `#(reply #m(,(mref recv 'name) ,(mref recv 'version)
               ,(mref send 'name) ,(mref send 'version))
            ,state))
  ;; Health
  ((`#(status bevin) _from (= `#m(tcp-port ,port) state))
   `#(reply not-implemented ,state))
  ((`#(status os-process) _from (= `#m(os-pid ,os-pid) state))
   `#(reply ,(process-alive? os-pid) ,state))
  ((`#(status all) _from  (= `#m(recv ,recv send ,send) state))
   `#(reply
      #m(recv-alive? ,(process-alive? (mref recv 'os-pid))
         send-alive? ,(process-alive? (mref send 'os-pid)))
      ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping the 'Bevin' backend ...")
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((msg _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,msg))) ,state)))

(defun handle_continue
  ;; Testing / debugging
  ((`#(post-init) state)
   ;; Was thinking about rendering the banner here; maybe get rid of if we don't need?
   (log-debug "Post-initialization tasks ...")
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug (unknown-continue (io_lib:format "~p" `(,msg))))
   `#(noreply ,state)))

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
  ((_reason `#m(send ,send recv ,recv))
   (log-notice "Terminating the 'Bevin' backend server ...")
   (catch (exec:stop (mref recv 'os-pid)))
   (catch (exec:stop (mref send 'os-pid)))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   management API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-bevin
  ((`#m(recv ,recv send ,send args ,args))
   `#m(recv ,(start-bin recv args)
       send ,(start-bin send args))))

(defun spawn-executable (bin args)
  (log-debug "Starting port for ~s with args ~p" (list bin args))
  (let* ((executable (string:join (lists:append (list bin) args) " "))
         (_ (log-debug "Executable: ~p" `(,executable)))
         (`#(ok ,pid ,os-pid) (exec:run_link executable '(stdin stdout monitor))))
    `#m(version ,(extract-version bin)
        pid ,pid
        os-pid ,os-pid)))

(defun start-bin
  (((= `#m(binary ,bin) port-state) args)
   (maps:merge port-state (spawn-executable bin args))))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   general API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-version ()
  (gen_server:call (SERVER) #(version display)))

(defun version ()
  (gen_server:call (SERVER) #(version)))

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
;;;::=-   MIDI API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-midi (data)
  (gen_server:cast (SERVER) `#(midi-write ,data)))

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

(defun process-alive? (os-pid)
  (has-str? (ps-pid os-pid) (integer_to_list os-pid)))

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

;; XXX move this into bv module
(defun extract-version (bin)
  "Extract the version number from the output, returned as binary."
  (let (((list _bin version _url) (clj:-> bin
                                          (++ " --version")
                                          (os:cmd)
                                          (string:trim)
                                          (re:split "[\n ]"))))
    version))

;; XXX this is used here and in the extempore module; let's move it somewhere
;;     generally useful
(defun stop-os-process (pid-str)
  (os:cmd (++ "kill -9 " pid-str)))

;;; scratch
