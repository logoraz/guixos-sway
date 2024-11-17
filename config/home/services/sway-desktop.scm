(define-module (config home services sway-desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  ;;#:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages music)
  #:use-module (gnu packages video)
  #:use-module (gnu packages qt)
  ;;#:use-module (gnu packages syncthing)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnucash)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:export (home-sway-desktop-service-type))

;;; Package Transformations
;; Keep for now as an example
;; deploy in package list as (latest-sbcl sbcl)
;; (define latest-sbcl
;;   (options->transformation
;;    '((with-latest . "sbcl"))))

(define (home-sway-desktop-profile-service config)
  (list  swaybg
         swayidle
         fuzzel
         wlogout
         mako
         grimshot ;; grimshot --notify copy area
         wlsunset
         network-manager-applet
         libinput
         wl-clipboard
         wev

         ;; Compatibility for older Xorg applications
         xorg-server-xwayland

         ;;Flatpak & XDG Utilities
         flatpak
         xdg-desktop-portal
         xdg-desktop-portal-gtk
         xdg-desktop-portal-wlr
         xdg-utils ;; For xdg-open, etc
         xdg-dbus-proxy
         shared-mime-info
         (list glib "bin")

         ;; Appearance
         matcha-theme
         papirus-icon-theme
         adwaita-icon-theme
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

         ;; Authentication
         keepassxc
         gnupg
         pinentry
         password-store

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
         ;;alsa-utils
         ;;pavucontrol

         ;; PDF reader
         zathura
         zathura-pdf-mupdf

         ;; File syncing
         ;;syncthing-gtk

         ;; Applications
         foot       ;;|--> gnu packages terminals
         gnucash    ;;|--> gnu packages gnucash
         gimp-next  ;;|--> gnu packages gimp
         inkscape   ;;|--> gnu packages inkscape
         blender    ;;|--> gnu packages graphics

         ;; General utilities
         egl-wayland
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


(define home-sway-desktop-service-type
  (service-type (name 'home-sway-desktop-config)
                (description "Applies my personal Sway desktop configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-sway-desktop-profile-service)))
                (default-value #f)))
