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

(defun get-input-id (name)
  (xt.msg:sync
   (xt.lang:sexp
    "pm_input_device_with_name"
    (xt.lang:->xt name))))

(defun get-output-id (name)
  (xt.msg:sync
   (xt.lang:sexp
    "pm_output_device_with_name"
    (xt.lang:->xt name))))

(defun stop (fn-name)
  (xt.msg:async
   (xt.lang:sexp
    "define"
    (++ (xt.lang:->xt fn-name)
        (xt.lang:sexp
         "lambda"
         (xt.lang:sexp
          "beat"
          (xt.lang:->xt 'dur)))
        (xt.lang:->xt 'true)))))

(defun set-out-stream! (device-id)
  (xt.msg:async
   (xt.lang:sexp
    "*mout*"
    (xt.lang:sexp
     "pm_create_output_stream"
     (xt.lang:->xt device-id)))))
