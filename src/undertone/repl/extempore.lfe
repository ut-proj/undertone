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
(defun max-hist () 50)

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
     ('("hist") 'hist)
     (`("hist" ,idx) `#(hist ,idx))
     (`("hist-line" ,idx) `#(hist-line ,idx))
     (`("load" ,file) `#(load ,file))
     ('("quit") 'quit)
     (`("rerun" ,idx) `#(rerun ,idx))
     ('("term") 'term)
     ('("v") 'version)
     ('("version") 'version)
     (_ (xt-eval sexp)))))

(defun repl-eval (sexp)
  (let ((tokens (mref sexp 'tokens)))
    (case tokens
      ('() 'empty)
      (progn
        (undertone.server:history-insert (mref sexp 'source))
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
    ('hist (hist))
    (`#(hist ,idx) (hist (list_to_integer idx)))
    (`#(hist-line ,idx) (hist-line (list_to_integer idx)))
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

(defun hist (n)
  (io:format "~n---- Recent REPL History ----~n~n")
  (log-debug "Got hist line count: ~p" `(,n))
  (show-hist (get-hist-list n))
  (io:format "~n"))

(defun hist ()
  (hist (max-hist)))

(defun hist-line (n)
  (log-debug "Got hist index: ~p" `(,n))
  (show-hist (undertone.server:history n)))

(defun load (file-name)
  (let ((`#(ok ,data) (file:read_file file-name)))
    (xt.msg:sync (binary_to_list data))))

(defun rerun (n)
  ;; The history line index needs to be incremented, since when it does the
  ;; lookup, this function will have been added to the history, incrementing
  ;; the indices for all the other entries in the history by one.
  (clj:-> (+ n 1)
          (undertone.server:history)
          (car)
          (extract-hist-cmd)
          (undertone.sexp:parse)
          (eval-dispatch)
          (print)))

(defun version ()
  (lfe_io:format "~p~n" `(,(undertone.sysconfig:versions))))

;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-hist-cmd
  ((`#(,_ ,elem))
   elem))

(defun get-hist-list (n)
  (let* ((hist-list (undertone.server:history-list))
         (pos (- (length hist-list) n))
         (idx (if (=< pos 0) 0 pos)))
    (lists:nthtail idx hist-list)))

(defun print-hist-line (elem idx)
  (lfe_io:format "~p. ~s~n" `(,idx ,(extract-hist-cmd elem))))

(defun show-hist
  (('())
   'ok)
  ((`(,h . ,t))
   (print-hist-line h (+ 1 (length t)))
   (show-hist t)))
