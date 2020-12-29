;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   SuperCollider server options   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord server-options
  arch               ; - the target architecture [current host]
  (attrs '())        ; - list of additional target attributes
  channels           ; - attempts to force num of output audio channels
  compile            ; - compiles xtm file to native executable
  cpu                ; - the target cpu [current host]
  device-name        ; - a regex to match the name of the audio device to use
                     ;     (output or duplex) (overrides index)
  device             ; - the index of the audio device to use (output or duplex)
  frames             ; - attempts to force frames [1024]
  inchannels         ; - attempts to force num of input audio channels
  indevice-name      ; - a regex to match the name of the audio input device to
                     ;     use (overrides index)
  indevice           ; - the index of the audio input device to use
  latency            ; - attempts to force audio output latency
  (noaudio 'false)   ; - set to 'true for no audio output: use a "dummy" device
                     ;     (overrides 'device' option)
  (nobase 'false)    ; - set to 'true to not load base lib on startup
  (port 7099)        ; - port for primary process
  run                ; - path to a scheme file to load at startup
  (samplerate 48000) ; - audio samplerate
  sharedir           ; - location of the Extempore share dir
                     ;     (which contains runtime/, libs/, examples/, etc.)
  (term 'ansi)       ; - either ansi, cmd (windows), basic (for simpler ansi
                     ;     terms), or nocolor
  (timediv 1)        ; - timed sub divisions of FRAMES for scheduling engine
                     ;     (1 = no division which is the default)
  )

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: xt/options --| ()
  'ok)