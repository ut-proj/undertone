(defmodule undertone.repl.extempore.commands
  (export
   (check-extempore 0)
   (help 0)
   (load 1)
   (rerun 1) (rerun 2)
   (sess 0) (sess 1)
   (sess-line 1)
   (version 0)))

(include-lib "logjam/include/logjam.hrl")

(defun check-extempore ()
  (xt.msg:async "#(health ok)")
  ;; Then let's give the logger a chance to print the result before displaying
  ;; the extempore prompt:
  (timer:sleep 500))

(defun help ()
  (lfe_io:format "~s" `(,(binary_to_list
                           (undertone.sysconfig:read-priv
                            "help/repl-extempore.txt")))))

(defun load (file-name)
  (let ((`#(ok ,data) (file:read_file file-name)))
    (xt.msg:sync (binary_to_list data))))

(defun rerun (n)
  ;; XXX track which command was last executred in the server state, then when
  ;;     this command it run, it can look there and decide whether to run from
  ;;     the session or the history (when support for cross-session commands is
  ;;     added); this will have to wait until the REPL loop is converted to a
  ;;     gen_server or something similar
  (clj:-> (undertone.repl.extempore.util:get-cmd n)
          (undertone.sexp:parse)
          (undertone.repl.extempore.evaler:dispatch)
          (undertone.repl.extempore.printer:print)))

(defun rerun (n m)
  (lists:map #'rerun/1 (lists:reverse (lists:seq m n))))

(defun sess ()
  (sess (undertone.server:session-show-max)))

(defun sess (n)
  (io:format "~n---- Recent REPL Session Commands ----~n~n")
  (log-debug "Got sess line count: ~p" `(,n))
  (undertone.repl.extempore.util:show-prev
   (undertone.repl.extempore.util:get-sess-list n))
  (io:format "~n"))

(defun sess-line (n)
  (log-debug "Got session entries index: ~p" `(,n))
  (undertone.repl.extempore.util:show-prev
   (undertone.server:session n)))

(defun version ()
  (lfe_io:format "~p~n" `(,(undertone.server:versions))))
