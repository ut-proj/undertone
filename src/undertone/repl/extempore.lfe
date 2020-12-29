(defmodule undertone.repl.extempore
  (export
   (loop 1)
   (print 1)
   (read 0)
   (start 0)
   (xt-eval 1)))

(include-lib "logjam/include/logjam.hrl")

;; XXX put these in configuration
(defun prompt () "extempore> ")
(defun max-sess () 50)

;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   core repl functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read ()
  (let ((sexp (undertone.sexp:readlines (prompt))))
    (log-debug "Got user input (sexp): ~p" `(,sexp))
    sexp))

(defun eval-dispatch
  (((= `#m(tokens ,tokens) sexp))
   (case tokens
     (`("call" . ,_) (xt-blocking-eval sexp))
     ('("check-xt") 'check-xt)
     ('("eom") 'term)
     ('("exit") 'quit)
     ('("h") 'help)
     ('("help") 'help)
     (`("load" ,file) `#(load ,file))
     ('("quit") 'quit)
     (`("rerun" ,idx) `#(rerun ,idx))
     ('("sess") 'sess)
     (`("sess" ,idx) `#(sess ,idx))
     (`("sess-line" ,idx) `#(sess-line ,idx))
     ('("term") 'term)
     ('("v") 'version)
     ('("version") 'version)
     (_ (xt-eval sexp)))))

(defun repl-eval (sexp)
  (let ((tokens (mref sexp 'tokens)))
    (case tokens
      ('() 'empty)
      (progn
        (undertone.server:session-insert (mref sexp 'source))
        (eval-dispatch sexp)))))

(defun xt-blocking-eval (sexp)
  "This is going to be ugly ... (for now/until we have real parsing)"
  ;; 1. with the value associated with the 'source' key, replace initial
  ;;    "(call" with ""
  ;; 2. from the same, do a regex to replace the final ")" with ""
  ;; 3. without any /real/ parsing, this should give us the body of the
  ;;    Extempore code
  ;; 4. send that to Extempore as a synchronous message
  (let* ((source (string:trim (mref sexp 'source)))
         (remove-call (re:replace source "^\\(call" "" '(#(return list))))
         (extempore-form (re:replace remove-call "\\)$" "" '(#(return list)))))
    (xt.msg:sync extempore-form)))

(defun xt-eval (sexp)
  (xt.msg:async (mref sexp 'source)))

(defun print (result)
  (case result
    ('() 'ok)
    ('check-xt (check-extempore))
    ('empty 'ok)
    ('help (help))
    ('sess (sess))
    (`#(sess ,idx) (sess (list_to_integer idx)))
    (`#(sess-line ,idx) (sess-line (list_to_integer idx)))
    (`#(load ,file) (load file))
    ('quit 'ok)
    (`#(rerun ,idx) (rerun (list_to_integer idx)))
    ('term (xt.msg:async ""))
    ('version (version))
    (_ (lfe_io:format "~p~n" `(,result))))
  result)

(defun loop
  (('quit)
   'good-bye)
  ((_)
   (try
     (loop (print (repl-eval (read))))
     (catch
       (`#(,err ,clause ,stack)
        (io:format "~p: ~p~n" `(,err ,clause))
        (io:format "Stacktrace: ~p~n" `(,stack))
        (loop 'restart))))))

(defun start ()
  (log-debug "Starting REPL ...")
  (loop 'start))

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   shell functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-extempore ()
  (xt.msg:async "#(health ok)")
  ;; Then let's give the logger a chance to print the result before displaying
  ;; the extempore prompt:
  (timer:sleep 500))

(defun help ()
  (lfe_io:format "~s" `(,(binary_to_list
                           (undertone.sysconfig:read-priv
                            "help/repl-extempore.txt")))))

(defun sess (n)
  (io:format "~n---- Recent REPL Session Commands ----~n~n")
  (log-debug "Got sess line count: ~p" `(,n))
  (show-prev (get-sess-list n))
  (io:format "~n"))

(defun sess ()
  (sess (undertone.server:session-show-max)))

(defun sess-line (n)
  (log-debug "Got session entries index: ~p" `(,n))
  (show-prev (undertone.server:session n)))

(defun load (file-name)
  (let ((`#(ok ,data) (file:read_file file-name)))
    (xt.msg:sync (binary_to_list data))))

(defun rerun (n)
  ;; The line index needs to be incremented, since when it does the lookup,
  ;; this function will have been added to the storage too, incrementing the
  ;; indices for all the other entries in storage by one.
  ;;
  ;; XXX track which command was last executred in the server state, then when
  ;;     this command it run, it can look there and decide whether to run from
  ;;     the session or the history (when support for cross-session commands is
  ;;     added); this will have to wait until the REPL loop is converted to a
  ;;     gen_server or something similar
  (clj:-> (+ n 1)
          (undertone.server:session)
          (car)
          (extract-prev-cmd)
          (undertone.sexp:parse)
          (eval-dispatch)
          (print)))

(defun version ()
  (lfe_io:format "~p~n" `(,(undertone.server:versions))))

;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-prev-cmd
  ((`#(,_ ,elem))
   elem))

(defun get-sess-list (n)
  (let* ((sess-list (undertone.server:session-list))
         (pos (- (length sess-list) n))
         (idx (if (=< pos 0) 0 pos)))
    (lists:nthtail idx sess-list)))

(defun print-prev-line (elem idx)
  (lfe_io:format "~p. ~s~n" `(,idx ,(extract-prev-cmd elem))))

(defun show-prev
  (('())
   'ok)
  ((`(,h . ,t))
   (print-prev-line h (+ 1 (length t)))
   (show-prev t)))
