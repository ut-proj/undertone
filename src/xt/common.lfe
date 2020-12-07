;;;; Common functions used by multiple components in Extempore.
(defmodule xt.common
  (export
   (play 4)
   (scale 2)))

(defun play (inst pitch-var velocity duration)
  ""
  (xt.lang:sexp "play" (xt.lang:->types `(,inst ,pitch-var ,velocity ,duration))))

(defun scale (octave-num note-count)
  ""
  (xt.lang:sexp "scale" (xt.lang:->types `(,octave-num ,note-count))))
