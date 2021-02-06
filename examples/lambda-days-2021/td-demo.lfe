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

(set device-id 3) ; Arturia Moog modular V (virtual synth)
;;(set device-id 5) ; Behringer Model D (physical synth/eurorack)
(xt.midi:set-out-stream! device-name device-id)

(set beats-per-minute 108)

;;; Effects Sequence

(set midi-channel 1)
(set sequence-name "seq-sfx")

(set seq-sfx1 '(c2))
(set pulse-sfx1 '(127))
(set beats-per-measure-sfx 1)
(set note-timing-sfx "12")

(set opts-sfx (xt.seq:midi-opts sequence-name
                                device-name
                                device-id
                                midi-channel
                                seq-sfx1
                                pulse-sfx1
                                beats-per-minute
                                beats-per-measure-sfx
                                note-timing-sfx))

(xt.seq:start opts-sfx)
(xt.seq:stop opts-sfx)

;;; Pad Sequence

;;; Main Sequence

(set midi-channel 0)
(set sequence-name "seq-1")

(set seq1 '(c3 c4 c3  c4 a#3 g3))
(set seq2 '(c3 c4 c3  c4 a#3 g3 a#3 g3))
(set seq3 '(c3 c3 a#3 g3 c4  c3 c4  g3))
(set seq4 '(c3 c3 a#3 g3 c4  c4 a#3 g3))
(set seq5 '(c3 c3 c4  c4 a#3 g3))
(set seq6 '(c4 c3 a#3 g3))
(set seq7 '(c4 c3))

(set pulse1 '(127 127 127 127 127 127))
(set pulse2 '(127 127 127 127 127 127 127 127))
(set pulse3 '(127 127 127 127))
(set pulse4 '(127 127))

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

;; Start a long ramp-down of the pulse witdth
(xt.midi:cc-ramp (mupd opts 'cc-code (width-osc-123)) 127 0 60)

(xt.seq:set-midi-notes! (mupd opts 'notes seq6 'pulses pulse3))

;; Ramp-down delay
(xt.midi:cc-ramp (mupd opts 'cc-code (wet-level-delay)) 60 30 10)

;; Change oscillator to be a lower octave
(xt.midi:cc (mupd opts 'cc-code (transpose-osc-1) 'cc-value 60))
(xt.midi:cc (mupd opts 'cc-code (transpose-osc-3) 'cc-value 80))

;; Reset the pulse witdth high
(xt.midi:cc (mupd opts 'cc-code (width-osc-123) 'cc-value 127))

(xt.seq:set-midi-notes! (mupd opts 'notes seq7 'pulses pulse4))

;; Change oscillators to be higher octaves
(xt.midi:cc (mupd opts 'cc-code (transpose-osc-1) 'cc-value 80))
(xt.midi:cc (mupd opts 'cc-code (transpose-osc-3) 'cc-value 127))

;; Ramp-up delay
(xt.midi:cc-ramp (mupd opts 'cc-code (wet-level-delay)) 30 60 10)

(xt.seq:set-midi-notes! (mupd opts 'notes seq2 'pulses pulse2))

;; Ramp-up cutoff again
(xt.midi:cc-ramp (mupd opts 'cc-code (cutoff-filter-1)) 30 40 5)

(xt.seq:set-midi-notes! (mupd opts 'notes seq3 'pulses pulse2))

;; Ramp-up the resonance
(xt.midi:cc-ramp (mupd opts 'cc-code (resonance-filter-1)) 0 80 5)

;; Ramp-down the resonance
(xt.midi:cc-ramp (mupd opts 'cc-code (resonance-filter-1)) 80 10 5)

(xt.seq:set-midi-notes! (mupd opts 'notes seq4 'pulses pulse2))

;; Ramp-down cutoff
(xt.midi:cc-ramp (mupd opts 'cc-code (cutoff-filter-1)) 40 25 10)

(xt.seq:set-midi-notes! (mupd opts 'notes seq1 'pulses pulse1))

;; Ramp-down oscilator 2
(xt.midi:cc-ramp (mupd opts 'cc-code (gain-mixer-2)) 80 45 10)

(xt.midi:cc (mupd opts 'cc-code (transpose-osc-1) 'cc-value 90))

;; Ramp-up other oscilators
(xt.midi:cc-ramp (mupd opts 'cc-code (gain-mixer-1)) 80 100 10)
(xt.midi:cc-ramp (mupd opts 'cc-code (gain-mixer-3)) 80 100 10)

;; Ramp-up delay
(xt.midi:cc-ramp (mupd opts 'cc-code (wet-level-delay)) 60 92 10)

(xt.seq:stop opts)




