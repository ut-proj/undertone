;;;; The LFE API for the "Bevin" backend (send/receive MIDI via the CLI tools
;;;; by Geert Bevin.
;;;;
;;;; Note that this backend is 100% MIDI, so you won't see 'midi' in any of the
;;;; function names ;-)
(defmodule bv
  ;; Deviceless/channelless API
  (export
   (devices 0)
   (version 0))
  ;; MIDI "stream"-based API (note we're borrowing that term from Extempore
  (export
   (cc 3)
   (cc-ramp 5)
   (cc-sine 6)
   (create-stream 2)
   ;;(note 4)
   (send 2)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   deviceless/channelless API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun devices ()
  (undertone.bevin:write-midi (list "list\n")))

(defun version ()
  (undertone.bevin:version))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   MIDI "stream"-based API API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc (stream cc-code value)
  (send stream `(cc ,cc-code value)))

(defun cc-ramp (stream cc-code start-val end-val duration)
  (let* ((sq (seq start-val end-val))
         (steps (length sq))
         (incr (/ (* 1000 duration) steps))
         (time 0))
    (lists:foldl
     (lambda (val acc)
       (let* ((sum (++ acc (io_lib:format " ~s cc ~s ~s" time start-val end-val)))
              (sched (timer:apply_after (round sum)
                                        'bv
                                        'cc
                                        ;; placeholder args
                                        `(,stream cc-code start-val end-val duration))))
         (log-debug "Scheduled: ~p" `(,sched))
         sum))
     ""
     sq))
  'ok)

(defun cc-sine (stream cc-code start-value max-value period duration)
  "Max-points:
   * pi/2, 5pi/2, 9pi/2, etc.
   * (floor (+ (* (/ 127 2) (math:sin (/ (* 5 (math:pi)) 2))) 64))
  Mid-points:
   * 0pi, pi, 2pi, etc.
   * (floor (+ (* (/ 127 2) (math:sin (/ (* 2 (math:pi)) 1))) 64))
  Min-points:
   * 3pi/2, 7pi/2, 11pi/2, etc.
   * (floor (+ (* (/ 127 2) (math:sin (/ (* 3 (math:pi)) 2))) 64))
  "
  'not-implemented)

(defun create-stream (device-name midi-channel)
  `#m(dev ,device-name
      ch ,midi-channel))

;;(defun note (stream note-value veloc duration)
;;  (send stream `(on ,note-value ,veloc))
;;  (timer:apply_after duration
;;                     'bv
;;                     'send
;;                    `( ... )))

(defun note (stream bpm note-value veloc note-timing)
  'tbd)

(defun send
  ((stream args) (when (is_atom args))
   (send stream (list args)))
  ((`#m(dev ,dev ch ,ch) args)
   (undertone.bevin:write-midi
    (join-args (lists:append (list
                              (list "dev" dev "ch" ch)
                              args
                              '("\n")))))))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun join-args (args)
  ;; Semi-jokingly: can the coers Erlang lib we're maintaining essentially be
  ;; rewritten to basically do this?
  (clj:->> args
           (lists:map
            (match-lambda
              ((x) (when (is_list x)) x)
              ((x) (when (is_binary x)) (binary_to_list x))
              ((x) (when (is_atom x)) (atom_to_list x))
              ((x) (when (is_integer x)) (integer_to_list x))))
           (lists:join " ")))

;; XXX move into common/util module
(defun seq (start end)
  (if (> end start)
    (lists:seq start end)
    (lists:reverse (lists:seq end start))))
