;;;; The functions in this module process the classic Erlang property list
;;;; configuration data as maps. To be clear, all function outputs from this
;;;; module should be Erlang maps.
(defmodule undertone.sysconfig
  (export
   (backend 0)))

(defun APPKEY () 'undertone)

(include-lib "logjam/include/logjam.hrl")

(defun backend ()
  (let* ((`#(ok ,cfg) (application:get_env (APPKEY) 'backend))
         (backend (proplists:get_value 'name cfg)))
    (mset
     (maps:from_list
      (proplists:get_value backend cfg))
     'name backend)))
