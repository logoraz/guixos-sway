(define-module (config home services config-files)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu home services dotfiles)
  #:export (home-config-files-service-type))


;; Edit setting the Home User
(define %user-name "logoraz")

(define %source (string-append "/home"
                               "/" %user-name
                               "/guixos"))

(define* (resolve dir #:key file)
  "Resolve local config dir & file"
  (let ((filename (if file (string-append "/" file) "")))
    (local-file (string-append
                 %source "/"
                 dir filename)
                #:recursive? #t)))

(define (home-config-files-service config)
  `( ;; Guile Configuration
    (".guile"
     ,(resolve "files/guile" #:file "guile"))

    ;; SBCL Configuration
    (".sbclrc"
     ,(resolve "files/common-lisp" #:file "dot-sbclrc.lisp"))

    ;; Guix Configuration Channels
    (".config/guix/channels.scm"
     ,(resolve "config/system" #:file "channels.scm"))

    ;;TODO: prep for GNU guile-next release to store these in XDG_CONFIG_HOME...
    (".config/guile/guile"
     ,(resolve "files/guile" #:file "guile"))

    ;; Sway configuration files
    ;; (".config/sway"
    ;;  ,(resolve "files/sway"))

    ;; GTK configuration --> needed for mouse theme on GTK applications
    ;; (".config/gtk-3.0/settings.ini"
    ;;  ,(resolve "files/gtk" #:file "settings.ini"))

    ;; Application Selector
    ;; (".config/mako"
    ;;  ,(resolve "files/mako"))

    ;; Screen Locking Application
    ;; (".config/fuzzel"
    ;;  ,(resolve "files/fuzzel"))

    ;; wlogout configuration
    ;; (".config/wlogout"
    ;;  ,(resolve "files/wlogout"))

    ;; GnuPG Configuration
    ;; (".config/gnupg/gpg-agent.conf"
    ;;  ,(resolve "files/gnupg" #:file "gpg-agent.conf"))

    ;; mbsync configuration (for mu/mu4e)
    ;; (".config/isyncrc"
    ;;  ,(resolve "files/isync" #:file "isyncrc"))

    ;; msmtp configuration (for mu/mu4e)
    ;; (".config/msmtp/config"
    ;;  ,(resolve "files/msmtp" #:file "config"))

    ;; Terminal configurations
    ;; (".config/foot/foot.ini"
    ;;  ,(resolve "files/foot" #:file "foot.ini"))

    ;; (".config/vterm/emacs-vterm-bash.sh"
    ;;  ,(resolve "files/vterm" #:file "emacs-vterm-bash.sh"))

    ;; Zathura Configuration File
    ;; (".config/zathura/zathurarc"
    ;;  ,(resolve "files/zathura" #:file "zathurarc"))

    ;; TODO
    ))


(define home-config-files-service-type
  (service-type (name 'home-config-files)
                (description "Service for setting up config files.")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-config-files-service)))
                (default-value #f)))

