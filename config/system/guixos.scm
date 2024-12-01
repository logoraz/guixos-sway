(define-module (config system guixos)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 ftw)

  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
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
  #:use-module (gnu home) ;; for guix-home-serivice-type
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix ci)
  #:use-module (guix packages)
  #:use-module (guix download)

  ;; #:use-module (config system guixos-base)
  #:use-module (config system guixos-channels)
  #:use-module (config home guixos-home))


(define %user-name "loraz")

;; System Services
;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define* (substitutes->services config #:optional (channels %guixos-channels))
  (guix-configuration
   (inherit config)
   (channels channels)
   ;; ref?
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
  ;; Configure gdm-service for wayland -> move wayland to home?
  ;; https://guix.gnu.org/manual/en/html_node/X-Window.html
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

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))

;; TODO: Define in guixos-base
(define %base-keyboard-layout
  (keyboard-layout "us"))

(define %guixos
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
   ;; for Lenovo ThinkPad X1 Carbon 4th Gen (Type 20FB) Laptop.
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
			  "aea0c27b-be9f-4384-96b8-e8dba1848280")))))

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems (cons* (file-system
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

   (users (cons* (user-account
                  (name %user-name)
                  (comment "Erik P Almaraz")
                  (group "users")
                  (home-directory (string-append "/home/" %user-name))
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video" "lp")))
		 %base-user-accounts))

   (packages (append
	      (list sway
                    swaylock-effects
                    glibc-locales)
	      %base-packages))

   (services %guixos-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))


;; Instantiate GuixOS Sway
%guixos
