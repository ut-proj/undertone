;;;; LUNA Minimoog MIDI control change code definitions
;;;;
;;;; These are documented at the following URL:
;;;; * https://help.uaudio.com/hc/en-us/articles/360041479272-Moog-Minimoog

;;; Controllers Section

(defun glide () 5)
(defun toggle-glide () 65)

;;; Oscillator Bank Section

(defun osc-1-range () 33)
(defun osc-1-waveform () 26)
(defun osc-2-range () 34)
(defun osc-2-tune () 39)
(defun osc-2-waveform () 27)
(defun osc-3-range () 35)
(defun osc-3-tune () 40)
(defun osc-3-waveform () 28)

;;; Mixer-Section

(defun osc-1-volume () 85)
(defun osc-2-volume () 86)
(defun osc-3-volume () 87)
(defun external-input-volume () 70)
(defun noise-volume () 89)
(defun noise-pink/white () 29)

;;; Modifiers-Section

(defun filter-cutoff-frequency () 74)
(defun filter-emphasis () 71)
(defun filter-contour () 25)
(defun filter-attack-time () 22)
(defun filter-decay-time () 23)
(defun loudness-contour-attack-time () 73)
(defun loudness-contour-decay-time () 75)
(defun decay-on/off () 72)

;;; Modifications-Panel

(defun lfo-rate () 76)

;; This function is for display purpses when used in the REPL
;; and need to be the last function in the include file.
(defun |-- loaded include: luna/minimoog --| ()
  'ok)