(defmodule undertone.web.api
  (export
   (phrase-data 3)))

(include-lib "logjam/include/logjam.hrl")

(defun phrase-data (session-id env input)
  (log-debug "Got ESI request ...")
  (let* ((headers '("Content-Type: application/json"
                    "Cache-Control: no-cache"
                    "Cache-Control: no-store"
                    "\r\n"))
         (data #m(key #"C#m"
                  time-signature #"4/4"
                  tempo 108
                  total-beats 8
                  current-beat 5
                  current-chord #"A#maj9"
                  next-chord #"D#m"))
         (payload `#m(result ,data
                      errors ()))
         (encoded (jsone:encode payload)))
    (log-debug "payload: ~p" `(,payload))
    (log-debug "encoded: ~p" `(,encoded))
    (mod_esi:deliver session-id (string:join headers "\r\n"))
    (mod_esi:deliver session-id encoded)))
