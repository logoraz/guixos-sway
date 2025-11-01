(define-module (config home services desktop-profile)
  #:use-module (gnu)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages text-editors) ;;lem
  #:use-module (gnu packages tls)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages image)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages video)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnucash)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages games)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages node)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (config packages zen-browser)
  #:export (home-desktop-profile-service-type))


;;; Package Transformations
(define (home-desktop-profile-service config)
  (list ;; XDG Utilities
        flatpak
        fontconfig
        xdg-desktop-portal
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
        xdg-utils ;; For xdg-open, etc
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")
        ;; Compatibility for older Xorg applications
        xorg-server-xwayland

        ;; Appearance
        papirus-icon-theme
        adwaita-icon-theme
        breeze-icons ;; for KDE apps
        matcha-theme
        gnome-themes-extra

        ;; Fonts
        font-jetbrains-mono
        font-fira-code
        font-liberation
        font-iosevka-aile
        font-awesome
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk

        ;; Browsers
        zen-browser-bin
        icecat
        enchant
        speech-dispatcher
        node
        qtwayland ;;(specification->package "qtwayland@5")

        ;; Authentication
        gnupg
        pinentry
        keepassxc
        password-store ;; move to password-store eventually...

        ;; Audio devices & Media playback
        mpv
        mpv-mpris
        yt-dlp
        playerctl
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        ffmpeg
        opus
        ffmpegthumbnailer
        pavucontrol
        steam-devices-udev-rules

        ;; Mail & Document Utilities
        mu
        isync
        msmtp
        texlive
        texinfo
        texi2html
        zathura
        zathura-pdf-mupdf

        ;; Applications
        lem
        gnucash
        gimp-3
        inkscape
        blender
        libreoffice

        ;; lambda
        guile-ares-rs
        guile-fibers
        guile-hall
        guile-hoot
        guile-g-golf
        guile-goblins
        sbcl-slynk

        ;; Utilities
        openssl
        aws-lc
        wev
        alsa-lib
        alsa-utils
        alsa-plugins
        tlp ;; --> alternate for bluetooth & wifi controls
        blueman
        network-manager-applet
        udiskie
        trash-cli))

(define home-desktop-profile-service-type
  (service-type (name 'home-sway-desktop-config)
                (description "Applies my personal Sway desktop configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)))
                (default-value #f)))
