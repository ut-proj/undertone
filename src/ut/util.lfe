(defmodule ut.util
  (export
   (has-str? 2)))

(defun has-str? (string pattern)
  (case (string:find string pattern)
    ('nomatch 'false)
    (_ 'true)))
