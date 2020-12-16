;;;; Maybe some day this will actually parse and tokenize S-expressions.
;;;;
;;;; Until then, we'll go old-school with a terminating character on its own
;;;; line.
(defmodule undertone.sexp
  (export
   (readlines 1)))

(defun readlines (prompt)
  (readlines prompt 0 '()))

(defun readlines (prompt last-count lines)
  (let* ((line (io:get_line prompt))
         (lines (++ lines (list line)))
         (this-count (balance last-count line)))
    (if (== 0 this-count)
      (parse lines)
      (readlines prompt this-count lines))))

(defun balance (last-count line)
  (lists:foldl (lambda (x acc)
                 (case x
                   (#\( (+ acc 1))
                   (#\) (- acc 1))
                   (_ acc)))
               last-count
               line))

(defun parse (lines)
  (let ((source (lists:flatten lines)))
    `#m(lines ,lines
        source ,(string:trim source)
        tokens ,(string:tokens source " ()#\n\""))))