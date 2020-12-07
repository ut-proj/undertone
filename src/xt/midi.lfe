(defmodule xt.midi
  (export
   (init 0)
   (list-devices 0)
   (stop 1)
   (set-out-stream! 1)))

(defun init ()
  (xt.msg:async
   (xt.lang:sexp "pm_initialize")))

(defun list-devices ()
  (xt.msg:sync
   (xt.lang:sexp "pm_print_devices")))

(defun stop (fn-name)
  (xt.msg:async
   (xt.lang:sexp
    "define"
    (++ (xt.lang:->type fn-name)
        (xt.lang:sexp "lambda"
                      )))))

(defun set-out-stream! (device-id)
  (xt.msg:async
   (xt.lang:sexp
    "*mout*"
    (xt.lang:sexp
     "pm_create_output_stream"
     (xt.lang:->type device-id)))))
