;;;; Moog minimoog iPad app MIDI control change code definitions
;;;;
;;;; These are not provided by the manufacturer or application developer; rather
;;;; they are supplied by the user. The mapping below is based upon the mapping
;;;; created in the ticket here:
;;;; * https://github.com/ut-proj/undertone/issues/66

;;; Controllers Section

(defun ctl-tune () 1)
(defun ctl-glide () 2)
(defun ctl-mod-mix () 3)
(defun ctl-osc-3-filter () 4)
(defun ctl-noise-lf0 () 5)
(defun ctl-sco-mod () 6)

;;; Oscillator Bank Section

(defun osc-1-range () 7)
(defun osc-1-wave () 8)
(defun osc-2-range () 9)
(defun osc-2-tune () 10)
(defun osc-2-wave () 11)
(defun osc-3-ctl () 12)
(defun osc-3-range () 13)
(defun osc-3-tune () 14)
(defun osc-3-wave () 15)

;;; Mixer Section

(defun mix-osc-1-vol () 16)
(defun mix-osc-1-toggle () 17)
(defun mix-ext-input-toggle () 18)
(defun mix-ext-input-vol () 19)
(defun mix-osc-2-vol () 20)
(defun mix-osc-2-toggle () 21)
(defun mix-noise-toggle () 22)
(defun mix-noise-vol () 23)
(defun mix-noise-type () 24)
(defun mix-osc-3-vol () 25)
(defun mix-osc-3-toggle () 26)

;;; Modifiers Section

(defun mod-filter-toggle () 27)
;; These switches vary the eff  ect of the keyboard tracking, where the filter
;; section is aff  ected by the pitch of note played:
;; * 1 & 2 off - no keyboard tracking effect
;; * 1 & 2 on - maximum effect
;; * 1 on only - 1/3 of maximum effect
;; * 2 on only - 2/3 of maximum effect
(defun mod-key-ctl-toggle-1 () 28) , 
(defun mod-key-ctl-toggle-2 () 29)
(defun mod-cutoff-freq () 30)
(defun mod-filter-emphasis () 31)
(defun mod-filter-contour () 32)
(defun mod-filter-attach () 33)
(defun mod-filter-decay () 34)
(defun mod-filter-sustain () 35)
(defun mod-loud-attack () 36)
(defun mod-loud-decay () 37)
(defun mod-loud-sustain () 38)

;;; Wheel Section

(defun lfo-rate () 39)
(defun glide-toggle () 40)
(defun decay-toggle () 41)

;;; Volume Section

(defun volume () 42)

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: moog/minimoog --| ()
  'ok)