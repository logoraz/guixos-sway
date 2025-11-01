(define-module (config home guixos-home)
  #:use-module (gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sway)
  #:use-module (guix gexp)
  #:use-module (config home services environment)
  #:use-module (config home services config-files)
  #:use-module (config home services mutable-files)
  #:use-module (config home services streaming)
  #:use-module (config home services udiskie)
  #:use-module (config home services emacs-profile)
  #:use-module (config home services desktop-profile)
  #:use-module (config home services bash)
  #:use-module (config home services sway)
  #:export (guixos-home))



(define guixos-home
  (home-environment
   (services
    (append (list
             ;; Enable bluetooth connections to be handled properly
             ;; bluetooth service only currently available at system level.
             (service home-dbus-service-type)

             ;; Enable pipewire audio
             (service home-pipewire-service-type)

             ;; Setup SSH for home
             (service home-openssh-service-type
                      (home-openssh-configuration
                       (add-keys-to-agent "yes")))

             (service home-ssh-agent-service-type
                      (home-ssh-agent-configuration
                       (openssh openssh-sans-x)))

             ;; Monitor battery levels
             (service home-batsignal-service-type)

             ;; Udiskie for auto-mounting
             (service home-udiskie-service-type)

             ;; Streaming profile service
             (service home-streaming-service-type)

             ;; config files configuration
             (service home-config-files-service-type)

             ;; Mutable symlinks configuration
             (service home-mutable-symlinks-service-type)

             ;; Emacs Packages configuration
             (service home-emacs-packages-profile-service-type)

             ;; Sway Desktop profile configuration
             (service home-desktop-profile-service-type)

             ;; Set environment variables for every session
             (service home-env-vars-configuration-service-type)

             ;; Sway Configuration + package profile
             (service home-sway-service-type
                      (sway-configuration-extension
                       %empty-sway-configuration))
             (service home-sway-configuration-service-type)

             ;; Bash configuration
             (bash-config->service))

            %base-home-services))))

guixos-home
