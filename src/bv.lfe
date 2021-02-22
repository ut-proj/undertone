;;;; The LFE API for the "Bevin" backend (send/receive MIDI via the CLI tools
;;;; by Geert Bevin.
(defmodule bv
  (export
   (create-stream 2)
   (devices 0)
   (send 2)
   (version 0)))

(defun create-stream (device-name midi-channel)
  `#m(dev ,device-name
      ch ,midi-channel))

(defun devices ()
  (undertone.bevin:write-midi (list "list\n")))

(defun send
  ((stream args) (when (is_atom args))
   (send stream (list args)))
  ((`#m(dev ,dev ch ,ch) args)
   (undertone.bevin:write-midi
    (join-args (lists:append (list
                              (list "dev" dev "ch" ch)
                              args
                              '("\n")))))))

(defun version ()
  (undertone.bevin:version))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun join-args (args)
  (clj:->> args
           (lists:map
            (match-lambda
              ((x) (when (is_list x)) x)
              ((x) (when (is_binary x)) (binary_to_list x))
              ((x) (when (is_atom x)) (atom_to_list x))
              ((x) (when (is_integer x)) (integer_to_list x))))
           (lists:join " ")))
