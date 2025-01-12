(define-module (config home services sway-desktop)
  #:use-module (gnu)                             ; -
  #:use-module (gnu packages fonts)              ;-> font packages
  #:use-module (gnu packages wm)                 ;-> sway etc.
  #:use-module (gnu packages networking)         ;-> blueman
  #:use-module (gnu packages xorg)               ; -
  #:use-module (gnu packages xdisorg)            ; -
  #:use-module (gnu packages freedesktop)        ;-> udiskie,
  #:use-module (gnu packages linux)              ; -
  #:use-module (gnu packages glib)               ; -
  #:use-module (gnu packages gnome)              ; -
  #:use-module (gnu packages gnome-xyz)          ; -
  #:use-module (gnu packages kde-frameworks)     ; -
  #:use-module (gnu packages web-browsers)       ;-> qutebrowsers
  #:use-module (gnu packages gstreamer)          ; -
  #:use-module (gnu packages compression)        ; -
  #:use-module (gnu packages gnuzilla)           ; -
  #:use-module (gnu packages terminals)          ; -
  #:use-module (gnu packages graphics)           ; -
  #:use-module (gnu packages image)              ; -
  #:use-module (gnu packages music)              ; -
  #:use-module (gnu packages video)              ; -
  #:use-module (gnu packages qt)                 ; -
  #:use-module (gnu packages package-management) ; -
  #:use-module (gnu packages password-utils)     ;-> password-store
  #:use-module (gnu packages gnupg)              ;-> gnupg
  #:use-module (gnu packages gnucash)            ;-> gnucash
  #:use-module (gnu packages gimp)               ;-> gimp
  #:use-module (gnu packages inkscape)           ;-> inkscape
  #:use-module (gnu packages pdf)                ;-> zathura
  #:use-module (gnu packages shellutils)         ;-> trash-cli
  #:use-module (gnu services configuration)      ; -
  #:use-module (gnu home services)               ; -
  #:use-module (guix gexp)                       ; -
  #:use-module (guix transformations)            ;-> options-transformations

  #:export (home-sway-desktop-service-type))


;;; Package Transformations
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants
(define curr-trash-cli
  ;; Currently failing build due to tests since update to latest python...
  (options->transformation
   '((without-tests . "trash-cli"))))

(define latest-nyxt
  ;; use rde/packages/web-browsers.scm
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

(define (home-sway-desktop-profile-service config)
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
        librsvg

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
        (latest-nyxt nyxt)

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

        ;; Utilities
        wev
        (curr-trash-cli trash-cli)
        blueman
        network-manager-applet
        udiskie))

(define home-sway-desktop-service-type
  (service-type (name 'home-sway-desktop-config)
                (description "Applies my personal Sway desktop configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-sway-desktop-profile-service)))
                (default-value #f)))
