;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   SuperCollider server options   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-options ()
  #m(arch undefined          ; - the target architecture [current host]
     attrs ()                ; - list of additional target attributes
     channels undefined      ; - attempts to force num of output audio channels
     compile undefined       ; - compiles xtm file to native executable
     cpu undefined           ; - the target cpu [current host]
     device-name undefined   ; - a regex to match the name of the audio device
                             ;;    to use (output or duplex) (overrides index)
     device undefined        ; - the index of the audio device to use (output or
                             ;;    duplex)
     frames 64               ; - attempts to force frames [64]
     inchannels undefined    ; - attempts to force num of input audio channels
     indevice-name undefined ; - a regex to match the name of the audio input
                             ;;    device to use (overrides index)
     indevice undefined      ; - the index of the audio input device to use
     latency undefined       ; - attempts to force audio output latency
     noaudio undefined       ; - set to 'true for no audio output: use a
                             ;;    "dummy" device (overrides 'device' option)
     nobase undefined        ; - set to 'true to not load base lib on startup
     port 7099               ; - port for primary process
     run undefined           ; - path to a scheme file to load at startup
     samplerate 48000        ; - audio samplerate [48000]
     sharedir undefined      ; - location of the Extempore share dir
                             ;;    (which contains runtime/, libs/, examples/,
                             ;;    etc.)
     term nocolor            ; - either ansi, cmd (windows), basic (for simpler
                             ;;    ansi terms), or nocolor
     timediv 1               ; - timed sub divisions of FRAMES for scheduling
                             ;;    engine (1 = no division which is the default)
     ))

(defun server-options->strings (opts)
  (maps:fold (lambda (k v acc)
               (if (or (== v 'undefined) (== v '()))
                 acc
                 (lists:append acc (list
                                    (lists:flatten (lfe_io_format:fwrite1 "--~p=~p"
                                                           `(,k ,v)))))))
             '()
             opts))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: xt/options --| ()
  'ok)