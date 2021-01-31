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

(xt.midi:cc (mupd opts 'cc-code (cutoff-filter-1) 'cc-value 20))
(xt.midi:cc-off (mupd opts 'cc-code (resonance-filter-1)))

(xt.seq:start opts)

(set opts (mupd opts 'notes seq2
                     'pulses pulse2
                     'beats-per-measure 4))

(xt.seq:set-midi-notes! opts)

(xt.seq:stop opts)