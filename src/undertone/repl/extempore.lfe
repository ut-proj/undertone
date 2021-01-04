(defmodule undertone.repl.extempore
  ;; Server-based REPL approach
  (export
   (start 0))
  ;; Function-based REPL approach
  (export
   (run 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "include/repl.lfe")

;;;;;::=-------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (spawn (lambda () (server))))

(defun server ()
  (server (make-state name "extempore-repl")))

(defun server (st)
  (process_flag 'trap_exit 'true)
  (log-debug "Starting REPL server ...")
  (undertone.server:set-repl 'extempore)
  (undertone.repl.extempore.util:display-banner)
  (let ((evaler (undertone.repl.extempore.evaler:start st)))
    (server-loop st evaler)))

(defun server-loop
  (((= (match-state prompt p) st) evaler)
   (let* ((p (if (=/= p 'undefined) p (set-prompt)))
          (st (update-state-prompt st p))
          (`#(,result ,evaler) (undertone.repl.extempore.reader:read
                                st evaler)))
     (case result
       (`#(ok ,sexp)
        (let ((`#(,evaler ,st) (send-eval sexp evaler st)))
          (server-loop st evaler)))
       (`#(error ,error)
        (undertone.repl.extempore.util:list-errors `(,error))
        (server-loop st evaler))))))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   support / utility functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prompt stuff

(defun node-prompt (prompt)
  (let ((node (atom_to_list (node))))
    (case (re:run prompt "~node")
      ('nomatch (list "(" node (list ")" prompt)))
      (_ (list "" (re:replace prompt  "~node" node '(#(return list))) "")))))

(defun set-prompt ()
  (let ((default-prompt (undertone.server:prompt)))
    (case (is_alive)
      ('true (io_lib:format "~s~s~s" `(,(node-prompt default-prompt))))
      ('false (let ((prompt (re:replace default-prompt "~node" "" '(#(return list)))))
                (io_lib:format "~s" `(,prompt)))))))

;;; reader stuff

(defun send-eval (form evaler st)
  (! evaler `#(eval-expr ,(self) ,form))
  (receive
    (`#(eval-value ,evaler ,_val ,st)
     `#(,evaler ,st))
    (`#(eval-error ,evaler ,class)
     (receive
       (`#(EXIT ,evaler #(,reason ,stack))
        (undertone.repl.extempore.util:report-exception class reason stack)))
     `#(,(undertone.repl.extempore.evaler:start st) ,st))
    (`#(EXIT ,evaler ,error)
     ;; the eval process was exited or killed
     (undertone.repl.extempore.util:report-exception 'error error '())
     `#(,(undertone.repl.extempore.evaler:start st) ,st))))

;;;;;::=-----------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   simple, function-based approach   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run ()
  (undertone.repl.extempore.looper:loop))
