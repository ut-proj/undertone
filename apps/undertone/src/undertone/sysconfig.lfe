;;;; The functions in this module process the classic Erlang property list
;;;; configuration data as maps. To be clear, all function outputs from this
;;;; module should be Erlang maps.
(defmodule undertone.sysconfig
  (export
   (backend 0)
   (backend-name 0)
   (backend-display 0)
   (backend-version 0)
   (banner-file 0)
   (banner 0)
   (prompt 0)
   (render-banner 0)
   (version 1)
   (version+name 1)
   (version-arch 0)
   (version-system 0)
   (versions 0)
   (versions-deps 0)
   (versions-langs 0)
   (versions-rebar 0)))

(defun APPKEY () 'undertone)

(include-lib "lfe/include/clj.lfe")
(include-lib "logjam/include/logjam.hrl")

(defun backend ()
  (let* ((cfg (config 'backend))
         (name (mref cfg 'name)))
    (mset (maps:from_list (mref cfg name))
          'name
          name)))

(defun backend-name ()
  (mref (backend) 'name))

(defun backend-display ()
  (mref (backend) 'display-name))

(defun backend-display-version ()
  (++ (backend-display) " " (backend-version)))

(defun backend-version ()
  (mref (backend) 'version))

(defun banner-file ()
  (config 'banner))

(defun banner ()
  (let ((`#(ok ,data) (file:read_file
                       (filename:join (code:priv_dir (APPKEY))
                                      (banner-file)))))
    (clj:-> data
            (binary:replace #"{{VERSION}}"
                            (list_to_binary (version 'undertone)))
            (binary:replace #"{{BACKEND}}"
                            (list_to_binary (backend-display-version)))
            (binary:replace #"{{PROMPT}}"
                            (list_to_binary (prompt))))))

(defun config (key)
  (let ((`#(ok ,cfg) (application:get_env (APPKEY) key)))
    (try
      (maps:from_list cfg)
      (catch
        (`#(,_ ,_ ,_)
         (progn
           (log-debug "Couldn't convert value for ~p to map; using default value ..."
                     `(,key))
           cfg))))))

(defun prompt ()
  (config 'prompt))

(defun render-banner ()
  (io:format "~s" `(,(banner))))

(defun version (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version+name (app-name)
  `#(,app-name ,(version app-name)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version-backend ()
  `#(backend ,(backend-display-version)))

(defun version-system ()
  (string:replace (erlang:system_info 'system_version)
                  "Erlang/OTP"
                  "LFE/OTP"))

(defun versions ()
  (lists:append `((,(version-undertone)
                   ,(version-backend))
                  ,(versions-deps)
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))

(defun versions-deps ()
  `(,(version+name 'osc_lib)
    ,(version+name 'tcp-client)))
  

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun version-undertone ()
  (version+name (APPKEY)))
