(define-module (config home services sway-desktop)
  #:use-module (gnu)
  #:use-module (gnu home services)
  ;; #:use-module (gnu home services sway) ;WIP
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:export (home-sway-desktop-service-type))


(use-package-modules certs ssh fonts wm wget curl bootloaders networking
                     xorg xdisorg freedesktop linux glib gnome gnome-xyz
                     kde-frameworks web-browsers gstreamer compression
                     gnuzilla terminals graphics image music video qt
                     package-management password-utils gnupg gnucash gimp
                     inkscape pdf shellutils)

(use-service-modules configuration)

;;; Package Transformations
(define latest-nyxt
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

(define (home-sway-desktop-profile-service config)
  ;; convert to package list -> %sway-desktop-packages
  ;; and define in its own module -> sway-desktop-packages.scm
  (list sway
        swaylock-effects
        swaybg
        swayidle
        fuzzel
        wlogout
        mako
        grimshot ;; grimshot --notify copy area
        wlsunset
        wl-clipboard
        wev

        ;; Compatibility for older Xorg applications
        xorg-server-xwayland

        ;; XDG Utilities
        flatpak
        xdg-desktop-portal
        xdg-desktop-portal-wlr
        xdg-utils ;; For xdg-open, etc
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        adwaita-icon-theme
        breeze-icons ;; for KDE apps
        gnome-themes-extra
        bibata-cursor-theme

        ;; Fonts
        font-jetbrains-mono
        font-fira-code
        font-hack
        font-liberation
        font-iosevka-aile
        font-awesome
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk

        ;; Browsers
        qutebrowser
        qtwayland ;;(specification->package "qtwayland@5")
        ;; (latest-nyxt nyxt)

        ;; Authentication
        gnupg
        pinentry
        keepassxc
        password-store ;; move to password-store eventually...

        ;; Audio devices & Media playback
        mpv ;;|--> gnu packages video
        mpv-mpris
        youtube-dl
        playerctl
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        pipewire ;;|--> gnu packages linux
        wireplumber

        ;; PDF reader
        zathura
        zathura-pdf-mupdf

        ;; Applications
        foot      ;;|--> gnu packages terminals
        gnucash   ;;|--> gnu packages gnucash
        gimp-next ;;|--> gnu packages gimp
        inkscape  ;;|--> gnu packages inkscape
        blender   ;;|--> gnu packages graphics

        ;; General utilities
        ;; egl-wayland
        network-manager-applet
        lm-sensors
        udiskie
        blueman ;;|--> gnu packages networking
        bluez
        brightnessctl
        curl
        wget
        openssh
        zip
        unzip
        trash-cli))

;; TODO - set sway config here in scheme another file
;; https://guix.gnu.org/manual/devel/en/html_node/Sway-window-manager.html
(define (home-sway-config-service config)
  `()
  ;; (list
  ;;  (variables )
  ;;  (keybindings )
  ;;  (packages %sway-desktop-packages) ;; list of packages to add to the user profile
  ;;  (inputs )
  ;;  (outputs )
  ;;  (bar )
  ;;  (startup-programs )
  ;;  (extra-content ))
  )

(define home-sway-desktop-service-type
  (service-type (name 'home-sway-desktop-config)
                (description "Applies my personal Sway desktop configuration.")
                (extensions
                 (list (service-extension ;; TODO remove <- home-sway-service-type
                        home-profile-service-type
                        home-sway-desktop-profile-service)
                       ;; TODO
                       ;; (service-extension
                       ;;  home-sway-service-type
                       ;;  home-sway-config-gexp)
                       ))
                (default-value #f)))
