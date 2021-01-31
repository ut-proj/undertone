(defmodule xt.midi
  (export
   (cc 1)
   (cc-off 1)
   (cc-on 1)
   (cc-ramp 1)
   (cc-ramp-down 1)
   (cc-ramp-up 1)
   (cc-sine 1)
   (init 0)
   (list-devices 0)
   (stop 1)
   (set-out-stream! 2)))

(defun init ()
  (xt.msg:async
   (xt.lang:sexp "pm_initialize")))

(defun list-devices ()
  (xt.msg:sync
   (xt.lang:sexp "pm_print_devices"))
  (xt.msg:sync
   (xt.lang:sexp "println" "'ok")))

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

(defun set-out-stream! (device-name device-id)
  (xt.msg:async
   (xt.lang:sexp
    "define"
    (++ device-name
        " "
        (xt.lang:sexp
         "pm_create_output_stream"
         (xt.lang:->xt device-id))))))

(defun cc (midi-opts)
  (xt.msg:async
   (xt.lang:sexp
    "send-midi-cc"
    (++ "(now) "
        (mref midi-opts 'midi-device-name)
        " "
        (xt.lang:->xt (mref midi-opts 'cc-code))
        " "
        (xt.lang:->xt (mref midi-opts 'cc-value))
        " "
        (xt.lang:->xt (mref midi-opts 'midi-channel))))))

(defun cc-off (midi-opts)
  (cc (mupd midi-opts 'cc-value 0)))

(defun cc-on (midi-opts)
  (cc (mupd midi-opts 'cc-value 127)))

(defun cc-ramp (midi-opts)
  'tbd)

(defun cc-ramp-down (midi-opts)
  'tbd)

(defun cc-ramp-up (midi-opts)
  'tbd)

(defun cc-sine (midi-opts)
  'tbd)
