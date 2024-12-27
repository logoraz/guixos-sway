(define-module (config system guixos)
  #:use-module (gnu)                          ;-> grub-efi-bootloader, etc.
  #:use-module (gnu packages cups)            ;-> cups-filters
  #:use-module (gnu packages ssh)             ;-> openssh
  #:use-module (gnu packages file-systems)    ;-> bcacefs-tools
  #:use-module (gnu packages linux)           ;-> brightnessctl,lm-sensors
  #:use-module (gnu packages audio)           ;-> bluez-alsa
  #:use-module (gnu packages xorg)            ;-> egl-wayland
  #:use-module (gnu packages wm)              ;-> swaylock
  #:use-module (gnu packages wget)            ;-> wget
  #:use-module (gnu packages curl)            ;-> curl
  #:use-module (gnu packages version-control) ;-> git
  #:use-module (gnu packages compression)     ;-> zip,unzip
  #:use-module (gnu packages guile)           ;-> guile (ensure)
  #:use-module (gnu packages guile-xyz)       ;-> guile-colorized (ensure)
  #:use-module (gnu packages lisp)            ;-> sbcl
  #:use-module (gnu packages lisp-xyz)        ;-> sbcl-slynk
  #:use-module (gnu services guix)            ;-> guix-home-service-type
  #:use-module (gnu services cups)            ;-> cups-service-type
  #:use-module (gnu services ssh)             ;-> openssh-service-type
  #:use-module (gnu services xorg)            ;-> screen-locker-service-type
  #:use-module (gnu services desktop)         ;-> bluetooth-service-type
  #:use-module (gnu services networking)      ;-> tor-service-type
  #:use-module (gnu system nss)               ;-> %mdns-host-lookup-nss
  #:use-module (gnu system keyboard)          ;-> keyboard-layout
  #:use-module (nongnu packages linux)        ;-> Modified NF Linux kernel
  #:use-module (nongnu system linux-initrd)   ;-> Microcode for NF  kernel
  #:use-module (guix gexp)                    ;-> gexp's local-file
  #:use-module (guix transformations)         ;-> options->transformation
  #:use-module (config services substitutes)
  #:use-module (config system guixos-channels)
  #:use-module (config home guixos-home))


;;; operating-system parameters

(define guixos-user-name "loraz")

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
          (uuid "1c3244f3-16d5-4bca-8f33-c6465a6e2f69")))))

(define %guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid "F8E9-9C22"
			'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "90a13ca3-a38d-4b47-a637-d037bc6ac567"
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

;;; System Services

;; Needed for greetd sway configuration...
;; Sneaky step here, need to manually place photo/contents needed
;; by sway-greetd.conf in places like /etc/
(define %greetd-conf (string-append "/home/loraz/.guixos-sway/"
                                    "files/sway/sway-greetd.conf"))

(define %guixos-base-services
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
             (openssh openssh-sans-x)
             (port-number 2222)))

   ;; TODO: New - need to look into & configure!!
   (service tor-service-type)

   (service greetd-service-type
            (greetd-configuration
             (greeter-supplementary-groups '("video" "input" "users"))
             (terminals
              (list
               (greetd-terminal-configuration
                (terminal-vt "1")
                (terminal-switch #t)
                (default-session-command
                  ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
                  ;; issues.guix.gnu.org/65769
                  (greetd-wlgreet-sway-session
                   (sway-configuration
                    (local-file %greetd-conf
                                #:recursive? #t)))))
               (greetd-terminal-configuration
                (terminal-vt "2"))
               (greetd-terminal-configuration
                (terminal-vt "3"))
               (greetd-terminal-configuration
                (terminal-vt "4"))
               (greetd-terminal-configuration
                (terminal-vt "5"))
               (greetd-terminal-configuration
                (terminal-vt "6"))))))

   ;; Set up home configuration
   (service guix-home-service-type
            `((,guixos-user-name ,%guixos-sway-home)))

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
                     (substitutes->services config
                                            #:channels %guixos-channels)))))

;;; Package Transformations & Packages
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants

(define %guixos-base-packages
  ;; Install bare-minimum system packages
  (cons* (specification->package "guile")
         guile-colorized
         sbcl
         sbcl-slynk
         bcachefs-tools
         egl-wayland
         ;; intel-media-driver/nonfree
         bluez
         bluez-alsa
         brightnessctl
         lm-sensors
         openssh-sans-x
         git
         (list git "send-email")
         curl
         wget
         zip
         unzip
         %base-packages))

;;; Define GuixOS Sway Alkaline Ice

(define %guixos-sway-core
  (operating-system
   ;; (inherit system)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout %guixos-keyboard-layout)

   (kernel linux)

   (firmware (list linux-firmware))

   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   ;; for Lenovo ThinkPad X1 Carbon 4th Gen (Type 20FB) Laptop.
   (initrd microcode-initrd)
   (kernel-arguments (cons "i915.enable_psr=0"
                           %default-kernel-arguments))

   (bootloader %guixos-bootloader)

   (swap-devices %guixos-swap-devices)

   (file-systems %guixos-file-systems)

   (groups %guixos-groups)

   (users %guixos-users)

   (packages %guixos-base-packages)

   (services %guixos-base-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

;;; Instantiate GuixOS
%guixos-sway-core
