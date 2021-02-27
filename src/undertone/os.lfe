(defmodule undertone.os
  (export
   (spawn 3)))

(include-lib "logjam/include/logjam.hrl")

(defun spawn (cmd args opts)
  (log-debug "Starting OS process for ~s with args ~p and opts ~p"
             (list cmd args opts))
  (let* ((executable (join-cmd-args cmd args))
         (`#(ok ,pid ,os-pid) (exec:run_link executable (run-opts opts)))
         (data `#m(pid ,pid os-pid ,os-pid)))
    (if (extract-version? opts)
      (mset data 'raw-version (extract-version cmd))
      data)))

(defun join-cmd-args (cmd args)
  (clj:-> cmd
          (list)
          (lists:append args)
          (string:join " ")))

(defun extract-version? (opts)
  (mref opts 'extract-version?))

(defun run-opts (opts)
  (mref opts 'run-opts))

(defun extract-version (cmd)
  "Extract the version number from the output, returned as binary."
  (clj:-> cmd
          (++ " --version")
          (os:cmd)
          (string:trim)))
