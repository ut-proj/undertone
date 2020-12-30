(defmodule undertone.repl.extempore.util
  (export
   (extract-prev-cmd 1)
   (get-cmd 1)
   (get-sess-list 1)
   (maybe-save-command 1)
   (print-prev-line 2)
   (show-prev 1)))

(defun extract-prev-cmd
  ((`#(,_ ,elem))
   elem))

(defun get-cmd (n)
  (let ((last-cmd (undertone.server:session n)))
    (case last-cmd
      ('() '())
      (_ (clj:-> last-cmd
                 (car)
                 (extract-prev-cmd))))))

(defun get-sess-list (n)
  (let* ((sess-list (undertone.server:session-list))
         (pos (- (length sess-list) n))
         (idx (if (=< pos 0) 0 pos)))
    (lists:nthtail idx sess-list)))

(defun maybe-save-command
  "Don't save the command if it's a re-run or if it's the same as the previous
  command."
  (((= `#m(tokens ,tkns source ,src) sexp))
   (cond ((== (car tkns) "rerun") 'skip-history)
          ((== src (get-cmd 1)) 'skip-history)
          ('true (undertone.server:session-insert (mref sexp 'source))))))

(defun print-prev-line (elem idx)
  (lfe_io:format "~p. ~s~n" `(,idx ,(extract-prev-cmd elem))))

(defun show-prev
  (('())
   'ok)
  ((`(,h . ,t))
   (print-prev-line h (+ 1 (length t)))
   (show-prev t)))
