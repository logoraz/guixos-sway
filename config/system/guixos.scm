(define-module (config system guixos)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu services guix)
  #:use-module (gnu services cups)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu system nss)
  #:use-module (gnu system keyboard)
  #:use-module (nongnu packages firmware)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (config system core substitutes)
  #:use-module (config system core guixos-channels)
  #:use-module (config services firmware)
  #:use-module (config home guixos-home)
  #:export (%guixos))


;;; operating-system parameters

(define guixos-user-name "logoraz")

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
           (uuid "c168ecec-65cf-458b-9d86-46bd4ebd25d3")))))

(define %guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
           (mount-point  "/boot/efi")
           (device (uuid "7CC3-E9BD"
			 'fat32))
           (type "vfat"))
         (file-system
           (mount-point "/")
           (device (uuid "dddb78a8-bd6d-4c6e-99b2-2ff56605c2ad"
			 'ext4))
           (type "ext4"))
         (file-system
           (mount-point "/home")
           (device (uuid
                    "59eef442-44da-4387-8db7-f9e0b2b776f9"
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
           (name "logoraz")
           (comment "Worker Bee")
           (home-directory (string-append "/home/" guixos-user-name))
           (group "users")
           (supplementary-groups '("wheel"    ;; sudo
                                   "seat"     ;; greetd/wlgreet
                                   "netdev"   ;; network devices
                                   "tty"      ;; -
                                   "input"    ;; -
                                   "lp"       ;; control bluetooth devices
                                   "audio"    ;; control audio devices
                                   "video"))) ;; control video devices
         %base-user-accounts))

;;; System Services

;; Needed for greetd sway configuration...
;; Sneaky step here, need to manually place photo/contents needed
;; by sway-greetd.conf in places like /etc/
(define %greetd-conf (string-append "/home/logoraz/guixos/"
                                    "files/wlgreet/sway-greetd.conf"))

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

   ;; TODO: Create (greetd-wlgreet-configuration-service-type)
   ;; see fwuupd-service-type
   (service greetd-service-type
            (greetd-configuration
              (greeter-supplementary-groups '("video" "input" "seat" "users"))
              (terminals
               (list
                (greetd-terminal-configuration
                  (terminal-vt "1")
                  (terminal-switch #t)
                  (default-session-command
                    ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html
                    ;; issues.guix.gnu.org/65769
                    (greetd-wlgreet-sway-session
                      (sway sway)
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
                  (terminal-vt "6"))
                (greetd-terminal-configuration
                  (terminal-vt "7"))))))

   ;; Firmware Updating Service via fwupd
   (service fwupd-service-type)

   ;; Set up home configuration
   ;; (service guix-home-service-type
   ;;          `((,guixos-user-name ,guixos-home)))

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
      (substitutes->services
       config
       #:channels %guixos-channels)))))

;;; Package Transformations & Packages
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants
(define latest-guile ;; example of guile-next
  (options->transformation
   '((with-latest . "guile"))))


;; Install bare-minimum system packages
(define %guixos-base-packages
  (append
   (list guile-3.0 ;;(specification->package "guile")
         (specification->package "guile-json")
         guile-colorized
         sbcl

         ;; File system & firmware tools
         bcachefs-tools
         polkit
         fwupd-nonfree

         ;; WM & Login Manager
         ;; needed for greetd/wlgreet configuration?
         sway
         swaylock-effects
         font-hack
         qogir-icon-theme
         bibata-cursor-theme

         ;; Desktop Tools/Utilities
         pipewire
         wireplumber
         egl-wayland
         bluez
         brightnessctl
         lm-sensors
         openssh-sans-x
         git
         (list git "send-email")
         curl
         wget
         zip
         unzip

         ;; Dev Toolchain
         gcc-toolchain
         binutils
         (specification->package "make")
         (specification->package "cmake")
         meson)

   %base-packages))

;;; Define GuixOS Sway Alkaline Ice

(define %guixos
  (operating-system
    ;; (inherit system)
    (host-name "framework")
    (timezone "America/Los_Angeles")
    (locale "en_US.utf8")
    (keyboard-layout %guixos-keyboard-layout)

    (kernel linux)

    (firmware (list linux-firmware))

    (initrd microcode-initrd)

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
%guixos
