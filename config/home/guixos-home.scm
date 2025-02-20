(define-module (config home guixos-home)
  #:use-module (gnu)                       ; -
  #:use-module (gnu home)                  ;-> home-environment
  #:use-module (gnu home services)         ; -
  #:use-module (gnu home services pm)      ; -
  #:use-module (gnu home services shells)  ; -
  #:use-module (gnu home services sound)   ;-> home-pipewire-service-type
  #:use-module (gnu home services desktop) ; -
  #:use-module (guix gexp)                 ; -
  #:use-module (config home services environment)
  #:use-module (config home services xdg-files)
  #:use-module (config home services sway-desktop)
  #:use-module (config home services emacs-guile)
  #:use-module (config home services raz-emacs)
  #:use-module (config home services streaming)
  #:use-module (config home services udiskie)

  #:export (%guixos-sway-home))


(define %guixos-sway-home
  (home-environment
   (services
    (append (list
             ;; Enable bluetooth connections to be handled properly
             ;; bluetooth service only currently available at system level.
             (service home-dbus-service-type)

             ;; Enable pipewire audio
             (service home-pipewire-service-type)

             ;; XDG files configuration
             (service home-xdg-local-files-service-type)

             ;; Set environment variables for every session
             (service home-env-vars-configuration-service-type)

             ;; Monitor battery levels
             (service home-batsignal-service-type)

             ;; Udiskie for auto-mounting
             (service home-udiskie-service-type)

             ;; Raz Emacs Package profile configuration
             (service home-emacs-config-service-type)

             ;; Raz Emacs Configuration
             (service home-raz-emacs-service-type)

             ;; Sway Desktop profile configuration
             (service home-sway-desktop-service-type)

             ;; Streaming profile service
             (service home-streaming-service-type)

             ;; Bash configuration
             (service home-bash-service-type
                      (home-bash-configuration
                       (guix-defaults? #f)
                       (aliases
                        `(("grep" . "grep --color=auto")
                          ("ls"   . "ls -p --color=auto")
                          ("ll"   . "ls -l")
                          ("la"   . "ls -la")
                          ("gosr" . ,(string-append
                                      "sudo guix system -L ~/.guixos-sway/ "
                                      "reconfigure "
                                      "~/.guixos-sway/config/system/guixos.scm"))))
                       (bashrc
                        (list (local-file "dot-bashrc.sh"
                                          #:recursive? #t)))
                       (bash-profile
                        (list (local-file "dot-bash_profile.sh"
                                          #:recursive? #t))))))
            %base-home-services))))
