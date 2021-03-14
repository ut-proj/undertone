(defmodule ut.os
  (export
   (ps-alive? 1)
   (ps-pid 1)
   (run 2) (run 3)))

(include-lib "logjam/include/logjam.hrl")

(defun default-run-opts ()
  '(stdin stdout monitor))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   API functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-alive? (os-pid)
  (ut.util:has-str? (ps-pid os-pid) (integer_to_list os-pid))) ;

;; XXX is this still needed?
(defun ps-pid (pid-str)
  (os:cmd (++ "ps -o pid -p" pid-str)))

(defun run (cmd args)
  (run cmd args #m()))

(defun run (cmd args opts)
  (log-debug "Starting OS process for ~s with args ~p and opts ~p"
             (list cmd args opts))
  (let* ((executable (join-cmd-args cmd args))
         (`#(ok ,pid ,os-pid) (exec:run_link executable (run-opts opts)))
         (data `#m(pid ,pid os-pid ,os-pid)))
    (if (extract-version? opts)
      (mset data 'raw-version (extract-version cmd))
      data)))

;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   private functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun join-cmd-args (cmd args)
  (clj:-> (list cmd)
          (lists:append args)
          (string:join " ")))

(defun extract-version? (opts)
  (if (not (maps:is_key 'extract-version? opts))
    'false
    (mref opts 'extract-version?)))

(defun extract-version (cmd)
  "Extract the version number from the output, returned as binary."
  (clj:-> cmd
          (++ " --version")
          (os:cmd)
          (string:trim)))

(defun run-opts (opts)
  (if (maps:is_key 'run-opts opts)
    (mref opts 'run-opts)
    (default-run-opts)))
