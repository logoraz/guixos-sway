(define-module (config system guixos)
  #:use-module (ice-9 optargs)                   ; ?
  #:use-module (ice-9 ftw)                       ; ?
  #:use-module (gnu)                             ; ?
  #:use-module (gnu packages cups)               ;-> cups-filters
  #:use-module (gnu packages ssh)                ; openssh
  #:use-module (gnu packages file-systems)       ; bcacefs-tools
  #:use-module (gnu packages package-management) ;-> guix-for-channels
  #:use-module (gnu packages linux)              ;-> brightnessctl,lm-sensors
  #:use-module (gnu packages audio)              ; bluez-alsa
  #:use-module (gnu packages xorg)               ; egl-wayland
  #:use-module (gnu packages wm)                 ; swaylock
  #:use-module (gnu packages wget)               ; wget
  #:use-module (gnu packages curl)               ; curl
  #:use-module (gnu packages version-control)    ; git
  #:use-module (gnu packages compression)        ; zip,unzip
  #:use-module (gnu services guix)               ;-> guix-home-service-type
  #:use-module (gnu services cups)               ; ?
  #:use-module (gnu services ssh)                ; ?
  #:use-module (gnu services xorg)               ; ?
  #:use-module (gnu services desktop)            ; ?
  #:use-module (gnu system setuid)               ; ?
  #:use-module (gnu system file-systems)         ; ?
  #:use-module (gnu system nss)                  ; ?
  #:use-module (gnu system keyboard)             ; ?
  #:use-module (gnu bootloader)                  ; ?
  #:use-module (nongnu packages linux)           ; ?
  #:use-module (nongnu system linux-initrd)      ; ?
  #:use-module (guix transformations)            ;-> options->transformation
  #:use-module (guix ci)                         ; ?
  #:use-module (guix packages)                   ; ?
  #:use-module (guix download)                   ; ?
  #:use-module (config system guixos-channels)
  #:use-module (config home guixos-home))


(define %user-name "loraz")

(define %guixos-keyboard-layout
  (keyboard-layout "us"))

(define %guixos-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout %guixos-keyboard-layout)))

(define %guixos-swap-devices
  (list (swap-space
         (target
          (uuid
	   "aea0c27b-be9f-4384-96b8-e8dba1848280")))))

(define %guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid
		   "F8E9-9C22"
		   'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid
		   "25602346-c255-4995-89a3-3a704346c911"
		   'ext4))
          (type "ext4"))
	 (file-system
	  (mount-point "/home")
	  (device (uuid
		   "ef7e647d-4d74-44ca-8894-90e28d372a89"
		   'ext4))
	  (type "ext4"))
	 %base-file-systems))

(define %guixos-groups
  ;; Add the 'seat' group
  (cons
   (user-group (system? #t) (name "seat"))
   %base-groups))

(define %guixos-users
  (cons* (user-account
          (name "loraz")
          (comment "Worker Bee")
          (home-directory "/home/loraz")
          (group "users")
          (supplementary-groups '("wheel"  ;; sudo
                                  "netdev" ;; network devices
                                  "tty"
                                  "input"
                                  "lp"       ;; control bluetooth devices
                                  "audio"    ;; control audio devices
                                  "video"))) ;; control video devices
         %base-user-accounts))

;; System Services
;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define* (substitutes->services config #:optional (channels %guixos-channels))
  (guix-configuration
   (inherit config)
   (channels channels)
   ;; ref https://guix.gnu.org/manual/devel/en/html_node/Customizing-the-System_002dWide-Guix.html
   (guix (guix-for-channels channels))
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
           "https://ci.guix.gnu.org"
           %default-substitute-urls))
   (authorized-keys
    (cons* (origin
            (method url-fetch)
            (uri "https://substitutes.nonguix.org/signing-key.pub")
            (file-name "nonguix.pub")
            (hash
             (content-hash
              "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
           %default-authorized-guix-keys))))

(define %guixos-services
  (cons*
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "swaylock")
             (program (file-append swaylock-effects "/bin/swaylock"))
             (using-pam? #t)
             (using-setuid? #f)))

   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #t)))

   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))

   ;; ssh user@host -p 2222
   (service openssh-service-type
            (openssh-configuration
             (openssh openssh)
             (port-number 2222)))

   ;; Set up home configuration
   (service guix-home-service-type
            `((,%user-name ,%guixos-home)))

   (service greetd-service-type
            (greetd-configuration
             (greeter-supplementary-groups '("video" "input" "users"))
             (terminals
              (list
               (greetd-terminal-configuration
                (terminal-vt "1")
                (terminal-switch #t)
                ;; issues.guix.gnu.org/65769
                (default-session-command
                  ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
                  (greetd-wlgreet-sway-session)))
               (greetd-terminal-configuration (terminal-vt "2"))
               (greetd-terminal-configuration (terminal-vt "3"))
               (greetd-terminal-configuration (terminal-vt "4"))
               (greetd-terminal-configuration (terminal-vt "5"))
               (greetd-terminal-configuration (terminal-vt "6"))))))

    ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
    (modify-services %desktop-services
                     ;; remove gdm-service-type
                     (delete gdm-service-type)
                     ;; greetd-service-type provides "greetd" PAM service
                     (delete login-service-type)
                     ;; and can be used in place of mingetty-service-type
                     (delete mingetty-service-type)
                     (guix-service-type
                      config =>
                      (substitutes->services config)))))

;;; Package Transformations & Packages
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants

(define %guixos-base-packages
  ;; Install bare-minimum system packages
  (cons* bcachefs-tools
         egl-wayland
         ;; intel-media-driver/nonfree
         bluez
         bluez-alsa
         brightnessctl
         lm-sensors
         openssh
         git
         (list git "send-email")
         curl
         wget
         zip
         unzip
         %base-packages))


(define %guixos
  (operating-system
   ;; (inherit guixos-base)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout %guixos-keyboard-layout)

   (kernel linux)

   (firmware (list linux-firmware))

   (initrd microcode-initrd)

   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   ;; for Lenovo ThinkPad X1 Carbon 4th Gen (Type 20FB) Laptop.
   (kernel-arguments (cons "i915.enable_psr=0"
                           %default-kernel-arguments))

   (bootloader %guixos-bootloader)

   (swap-devices %guixos-swap-devices)

   (file-systems %guixos-file-systems)

   (groups %guixos-groups)

   (users %guixos-users)

   (packages %guixos-base-packages)

   (services %guixos-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))


;; Instantiate GuixOS Sway: code name GuixOS Phonon
%guixos
