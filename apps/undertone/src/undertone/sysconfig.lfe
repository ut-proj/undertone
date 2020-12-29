;;;; The functions in this module process the classic Erlang property list
;;;; configuration data as maps. To be clear, all function outputs from this
;;;; module should be Erlang maps.
(defmodule undertone.sysconfig
  (export
   (backend 0)
   (backend-name 0)
   (backend-display 0)
   (backend-display-version 0)
   (backend-version 0)
   (banner-file 0)
   (banner 0)
   (priv-file 1)
   (prompt 0)
   (read-priv 1)
   (session 0)
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

;Docs: \e[1;34mhttps://cnbbooks.github.io/lfe-music-programming/current/ \e[0m
;File bug report: \e[1;34mhttps://github.com/lfex/undertone/issues/new \e[0m

(defun banner ()
  "Colour sequence:
   - A series of blues for the mushroom and spores
   - The yellow 'welcome'
   - 3 clumps of grass
   - Top of the 'd'
   - 1 clump of grass
   - Top of the 't'
   - 3 clumps of grass
   - Top row of 'undertone'
  "
  (let ((data (binary_to_list (read-priv (banner-file))))
        (lcyan "\e[1;36m")
        (cyan "\e[36m")
        (lblue "\e[1;34m")
        (blue "\e[34m")
        (lyellow "\e[1;33m")
        (yellow "\e[33m")
        (magenta "\e[35m")
        (lgreen "\e[1;32m")
        (green "\e[32m")
        (white "\e[1;37m")
        (lgrey "\e[37m")
        (grey "\e[1;30m")
        (end "\e[0m"))
    (io_lib:format data `(,lcyan ,end
                          ,blue  ,end
                          ,lcyan ,end

                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end
                          ,lblue ,end

                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end
                          ,lblue ,end

                          ,blue  ,end
                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,blue  ,end
                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,cyan    ,end
                          ,blue    ,end
                          ,magenta ,end

                          ,cyan  ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,cyan   ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end

                          ,white ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end

                          ,white ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end

                          ,white ,end
                          ,lgrey ,end
                          ,grey  ,end
                          ,(++ lyellow (version 'undertone) end)
                          ,(++ yellow (backend-display-version) end)
                          ,(++ "Docs: "
                               lblue
                               "https://cnbbooks.github.io/lfe-music-programming/"
                               end
                               "\n"
                               "Bug report: "
                               lblue
                               "https://github.com/lfex/undertone/issues/new"
                               end)
                          ,(prompt)))))

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

(defun priv-file (priv-rel-path)
  (filename:join (code:priv_dir (APPKEY))
                 priv-rel-path))

(defun prompt ()
  (config 'prompt))

(defun read-priv (priv-rel-path)
  (case (file:read_file (priv-file priv-rel-path))
    (`#(ok ,data) data)
    (other other)))

(defun session ()
  (config 'session))

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
