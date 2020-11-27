;;; Client data structure
(defrecord client
  sup
  pid)

;;; Client functions common to all implementations:
(defun client-ping
  (((match-client pid p))
   (osc_client:ping p)))

(defun client-conns ()
   (osc_client_mgr_sup:list_conns))

;; This function is for display purpses when used in the REPL
;; and need to be the last function in the include file.
(defun |-- loaded include: client --| ()
  'ok)