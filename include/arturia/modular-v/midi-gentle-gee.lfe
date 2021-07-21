;;;; MIDI control codes for the Arturia modular V (emulation of the Moog system
;;;; 55).

;;; These are the default settings that come with the VST plugin for the bank
;;; selection "Gentle Gee":
(defun general-volume () 7)

(defun cutoff-filter-1 () 74)
(defun resonance-filter-1 () 71)

(defun cutoff-filter-2 () 18)
(defun resonance-filter-2 () 19)

(defun cutoff-filter-3 () 16)
(defun resonance-filter-3 () 17)

(defun attack-time-env-1 () 73)
(defun decay-time-env-1 () 75)
(defun release-time-env-1 () 72)
(defun sustain-level-env-1 () 79)

(defun rate-lfo-1 () 76)
(defun anount-fm-1-filter-1 () 77)

(defun attack-time-vca-1 () 80)
(defun decay-time-vca-1 () 81)
(defun release-time-vca-1 () 83)
(defun sustain-level-vca-1 () 82)

(defun vca-1-soft-clip () 85)

(defun wet-level-delay () 91)
(defun wet-level-chorous () 93)

;;; These are the custom control codes added to the bank selection "Gentle Gee":
(defun width-osc-123 () 86)
(defun transpose-osc-1 () 87)
(defun transpose-osc-2 () 88)
(defun transpose-osc-3 () 89)

(defun gain-mixer-1 () 102)
(defun gain-mixer-2 () 103)
(defun gain-mixer-3 () 104)

(defun panoramic-vca-1 () 105)

(defun delay-tempo-sync-midi () 106)
(defun rate-left-delay () 107)
(defun rate-right-delay () 108)

(defun chorous-vca-1 () 109)

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: arturia/modular-v/midi-gentle-gee --| ()
  'ok)
