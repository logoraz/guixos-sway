(define-module (config system system-config)
  ;; #:use-module (config system base-system)
  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages glib)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services guix)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix ci)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))


(define %base-keyboard-layout
  (keyboard-layout "us"))

;; System Services
;; TODO: Move to custom system service (if possible)
;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define (substitutes->services config)
  (guix-configuration
   (inherit config)
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
             (content-hash "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
           %default-authorized-guix-keys))))

(define %guix-system-services
  (cons*
   ;; Configure gdm-service for wayland -> move wayland to home?
   ;; https://guix.gnu.org/manual/en/html_node/X-Window.html
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

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


(define user-name "loraz")

(define guixos-sway
  (operating-system
   ;; TODO: create base system config --> base-guixos
   ;; (inherit guixos-base)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout %base-keyboard-layout)

   (kernel linux)
   (firmware (list linux-firmware))
   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   ;; Leave enabled for Wayland...
   (initrd microcode-initrd)
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
		(keyboard-layout %base-keyboard-layout)))

   (swap-devices (list (swap-space
                        (target
                         (uuid
			  "72d71f71-bf44-4046-8d3d-f1fcfbe1cc42")))))

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems (append
                  (list (file-system
                         (mount-point  "/boot/efi")
                         (device (uuid "F8E9-9C22" 'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device (uuid "5ed31dd1-64d8-4efe-9bf6-9559e9c50493" 'ext4))
                         (type "ext4")))
		  %base-file-systems))

   (users (append
           (list (user-account
                  (name user-name)
                  (comment "Erik P Almaraz")
                  (group "users")
                  (home-directory (string-append "/home/" user-name ))
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "lp"))))
           %base-user-accounts))

   (packages (append
	      (list sway
                    swaylock-effects
                    glibc-locales)
	      %base-packages))
   
   (services %guix-system-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

guixos-sway
