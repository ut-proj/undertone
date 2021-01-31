;;;; An undertone / Extempore demonstration in the spirit of the Berlin School
;;;; of electronic music.
;;;;
;;;; Synthesizer: Arturia modular V (Moog System 55 emulator)
;;;; Bank Selection: Gentle Gee
(include-lib "include/notes/midi-name.lfe")
(include-lib "include/arturia/modular-v/midi-gentle-gee.lfe")

(xt:sys-load "libs/external/portmidi.xtm")
(xt:sys-load "libs/core/pattern-language.xtm")

(xt.midi:init)
(xt.midi:list-devices)

(set device-name "*midiout*")
(set device-id 2)

(xt.midi:set-out-stream! device-name device-id)
 
(set sequence-name "seq-1")
(set midi-channel 0)

(set seq1 '(c3 c4 c3  c4 a#3 g3))
(set seq2 '(c3 c4 c3  c4 a#3 g3 a#3 g3))
(set seq3 '(c3 c3 a#3 g3 c4  c3 c4  g3))
(set seq4 '(c3 c3 a#3 g3 c4  c4 a#3 g3))
(set seq5 '(c3 c3 c4  c4 a#3 g3))
(set seq6 '(c4 c3 a#3 g3))
(set seq7 '(c4 c3))

(set pulse1 '(1 1 1 1 1 1))
(set pulse2 '(1 1 1 1 1 1 1 1))
(set pulse3 '(1 1 1 1))
(set pulse4 '(1 1))

(set beats-per-minute 108)
(set beats-per-measure 6)
(set note-timing "1/2")

(set opts (xt.seq:midi-opts sequence-name
                            device-name
                            device-id
                            midi-channel
                            seq1
                            pulse1
                            beats-per-minute
                            beats-per-measure
                            note-timing))

(xt.midi:cc (mupd opts 'cc-code (cutoff-filter-1) 'cc-value 15))
(xt.midi:cc-off (mupd opts 'cc-code (resonance-filter-1)))

;; Adjust volume for increase due to effects
(xt.midi:cc (mupd opts 'cc-code (general-volume) 'cc-value 70))

;; Initialize delay effect
(xt.midi:cc (mupd opts 'cc-code (wet-level-delay) 'cc-value 0))
(xt.midi:cc (mupd opts 'cc-code (delay-tempo-sync-midi) 'cc-value 127))
(xt.midi:cc (mupd opts 'cc-code (rate-left-delay) 'cc-value 90))
(xt.midi:cc (mupd opts 'cc-code (rate-right-delay) 'cc-value 108))

;; Initialize chorous effect
(xt.midi:cc (mupd opts 'cc-code (wel-level-chorous) 'cc-value 0))
(xt.midi:cc (mupd opts 'cc-code (chorous-vca-1) 'cc-value 0))

;; Start playing the seqence
(xt.seq:start opts)

;; Add in the chorous
(xt.midi:cc (mupd opts 'cc-code (wel-level-chorous) 'cc-value 65))

;; Add in the delay
(xt.midi:cc (mupd opts 'cc-code (wet-level-delay) 'cc-value 60))

;; Ramp the cutoff
(xt.midi:cc-ramp (mupd opts 'cc-code (cutoff-filter-1)) 15 40 5)

;; Switch to other sequences
(xt.seq:set-midi-notes! (mupd opts 'notes seq5))

;; After ramp-up, ramp-down the cutoff a bit
(xt.midi:cc-ramp (mupd opts 'cc-code (cutoff-filter-1)) 40 30 5)

;; Start a long ramp of the pulse witdth
(xt.midi:cc-ramp (mupd opts 'cc-code (width-osc-123)) 127 0 60)

(xt.seq:set-midi-notes! (mupd opts 'notes seq6 'pulses pulse3))

;; Ramp-down delay

;; Change oscillator to be a lower octave

(xt.seq:set-midi-notes! (mupd opts 'notes seq7 'pulses pulse4))

;; Change oscillators to be higher octaves

;; Ramp-up delay

(xt.seq:set-midi-notes! (mupd opts 'notes seq2 'pulses pulse2))

;; Ramp-up cutoff again

(xt.seq:set-midi-notes! (mupd opts 'notes seq3))

;; Ramp-up and then down the resonance

(xt.seq:set-midi-notes! (mupd opts 'notes seq4))

(xt.seq:set-midi-notes! (mupd opts 'notes seq1 'pulses pulse1))

;; Ramp-down cutoff

;; Ramp-up pulse width

;; Ramp-up delay

(xt.seq:stop opts)