;;;; MIDI example adapted from Ben Swift's Extempore example

;; if you haven't connected to Extempore:
(xt:connect)

;; need to load this first, but only once per running session of the Extempore
;; binary; note that it will take a while (~1 minute) and this call will block
;; until the load is finished
(xt:sys-load "libs/external/portmidi.xtm")
(include-lib "undertone/include/xt-midi.lfe")

(midi-init)

;; call this function to print a list of the MIDI devices on your
;; machine
(midi-devices)

;; if the midi output you want to use has device id 3, then use that
;; device id here to create an output stream (which we will send our
;; midi output from). If the device id is some number other than 3,
;; change it here:
(set-midi-out! 3)

;; stop the midi
