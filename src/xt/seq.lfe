;;;; Functions for running sequencers in LFE/Extempore.
(defmodule xt.seq
  (export
   (defmidi 1)
   (midi-opts 9)
   (play 1)
   (set-midi-notes! 1)
   (start 1)
   (stop 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/notes/midi-name.lfe")

(defun midi-tmpl ()
  "
(define ~s-sequencer
  (lambda (beat dur nlst plst)
    ;; Uncomment these to print decrementing lists here in Extempore (debugging)
    ;;(println \"nlst:\")
    ;;(println nlst)
    ;;(println \"plst:\")
    ;;(println plst)
    (mplay ~s
           (car nlst)
           (car plst)
           dur
           ~p)
    (if (null? (cdr nlst))
        (callback (*metro* (+ beat (* 1 dur)))
                  '~s-sequencer
                  (+ beat dur)
                  ~s-note-timing
                  ~s-notes
                  ~s-pulses)
        (callback (*metro* (+ beat (* 1 dur)))
                  '~s-sequencer
                  (+ beat dur)
                  ~s-note-timing
                  (cdr nlst)
                  (cdr plst)))))
  ")

(defun play-tmpl ()
  "
(~s-sequencer (*metro* 'get-beat ~s-beats-per-measure)
    ~s-note-timing
    ~s-notes
    ~s-pulses)
  ")

(defun stop-tmpl ()
  "
(define ~s-sequencer
  (lambda (beat dur nlst plst)
    #t))
  ")

(defun midi-opts (name midi-device-name midi-device-id midi-channel
                  notes pulses bpmn bpms note-timing)
  `#m(name ,name
      midi-device-name ,midi-device-name
      midi-device-id ,midi-device-id
      midi-channel ,midi-channel
      notes ,(seq-atoms->xt notes)
      pulses ,pulses
      beats-per-minute ,bpmn
      beats-per-measure ,bpms
      note-timing ,note-timing
      cc-code undefined
      cc-value undefined))

;;; String/formatting functions

(defun midi (opts)
  (let ((name (mref opts 'name)))
    (format (midi-tmpl)
            (list name
                  (mref opts 'midi-device-name)
                  (mref opts 'midi-channel)
                  name name name name name name))))

(defun midi-bpms (opts)
  (format "(define ~s-beats-per-measure ~p)"
        `(,(mref opts 'name)
          ,(mref opts 'beats-per-measure))))

(defun midi-note-timing (opts)
  (format "(define ~s-note-timing ~s)"
        `(,(mref opts 'name)
          ,(mref opts 'note-timing))))

(defun midi-notes (opts)
  (format "(define ~s-notes ~s)"
          `(,(mref opts 'name)
            ,(mref opts 'notes))))

(defun midi-pulses (opts)
  (format "(define ~s-pulses '~w)"
        `(,(mref opts 'name)
          ,(mref opts 'pulses))))

(defun play-midi (opts)
  (let ((name (mref opts 'name)))
    (format (play-tmpl)
            (list name name name name name))))

(defun stop-midi (opts)
  (format (stop-tmpl) (list (mref opts 'name))))

(defun tempo (opts)
  (format "(*metro* 'set-tempo ~p)"
          `(,(mref opts 'beats-per-minute))))

;;; Message functions

(defun defmidi (opts)
  (xt.msg:async (midi opts)))

(defun play (opts)
  (xt.msg:async (play-midi opts)))

(defun set-midi-notes! (opts)
  (xt.msg:async (midi-bpms opts))
  (xt.msg:async (midi-note-timing opts))
  (xt.msg:async (midi-pulses opts))
  (xt.msg:async (midi-notes opts)))

(defun set-tempo! (opts)
  (xt.msg:async (tempo opts)))

(defun start (opts)
  (set-tempo! opts)
  (defmidi opts)
  (set-midi-notes! opts)
  (play opts))

(defun stop (opts)
  (xt.msg:async (stop-midi opts)))

;;; Utility functions

(defun format (tmpl args)
  (lists:flatten
   (lfe_io_format:fwrite1 tmpl args)))

(defun seq-atoms->xt (atoms)
  (xt.lang:sexp "list"
                (string:join
                 (lists:map #'atom_to_list/1 atoms)
                 " ")))