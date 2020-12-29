;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   SuperCollider server options   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord server-options
  (num-control-bus 16384)
  (num-audio-bus 1024)
  (num-input-bus 2)
  (num-output-bus 2)
  (block-size 64)
  (hardware-buffer-size 0)
  (hardware-samplerate 0)
  (num-sample-buffers 1024)
  (max-num-nodes 1024)
  (max-num-synthdefs 1024)
  (realtime-mem-size 8192)
  (num-wire-buffers 64)
  (num-random-seeds 64)
  (load-synthdefs-p 1)
  (publish-to-rendezvous-p 1)
  (max-logins 1)
  (verbosity 0)
  (ugen-plugins-path '())
  (device 'undefined))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: sc/options --| ()
  'ok)