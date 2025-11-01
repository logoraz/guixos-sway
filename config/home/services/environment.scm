(define-module (config home services environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-env-vars-configuration-service-type))


(define %gtk2-rc (string-append "$HOME/.guix-home/profile/share/"
                                "themes/Adwaita-dark/gtk-2.0/gtkrc"))

(define %xdg-data-dirs (string-append "$HOME/.guix-home/profile/share:"
                                      "/run/current-system/profile/share:"
                                      "$XDG_DATA_HOME/flatpak/exports/share:"))

;; borrowed from https://codeberg.org/daviwil/dotfiles/daviwil/systems/common.scm
(define (home-env-vars-config-gexp config)
  `( ;; Sort hidden (dot) files first in ls listings
    ("LC_COLLATE" . "C")

    ;; Set Emacs as editor
    ("EDITOR" . "emacsclient -c -a emacs")
    ("VISUAL" . "emacsclient -c -a emacs")

    ;; Set quotebrowser as the default
    ("BROWSER" . "Zen Browser")

    ;; Set GnuPG Config Dir env
    ("GNUPGHOME" . "$XDG_CONFIG_HOME/gnupg")

    ;; Set wayland-specific environment variables
    ("XDG_CURRENT_DESKTOP" . "sway")
    ("XDG_SESSION_TYPE"    . "wayland")
    ("RTC_USE_PIPEWIRE"    . "true")
    ("SDL_VIDEODRIVER"     . "wayland")
    ("MOZ_ENABLE_WAYLAND"  . "1")
    ("CLUTTER_BACKEND"     . "wayland")
    ("ELM_ENGINE"          . "wayland-egl")
    ("ECORE_EVAS_ENGINE"   . "wayland-egl")

    ;; GTK/QT/Cursor Theming
    ("GTK_THEME"            . "Adwaita:dark")
    ("GTK_ICON_THEME"       . "Qogir-dark")
    ("GDK_DPI_SCALE"        . "1")
    ;;TODO use gexp local-file to resolve this file...
    ("GTK2_RC_FILES"        . ,%gtk2-rc)


    ("QT_STYLE_OVERRIDE"    . "adwaita")
    ("QT_QPA_PLATFORMTHEME" . "gtk3")
    ("QT_QPA_PLATFORM"      . "wayland-egl")

    ("XCURSOR_THEME" . "Bibata-Modern-Classic")
    ("XCURSOR_SIZE"  . "20")

    ;; Set XDG environment variables
    ("XDG_LOG_HOME"     . "$HOME/.local/log")
    ("XDG_DOWNLOAD_DIR" . "$HOME/downloads")
    ("XDG_PICTURES_DIR" . "$HOME/pictures/screenshots")

    ;; Flatpak integration
    ("XDG_LOCAL_BIN"    . "$HOME/.local/bin")
    ("XDG_DATA_DIRS"    . ,%xdg-data-dirs)))


(define home-env-vars-configuration-service-type
  (service-type (name 'home-profile-env-vars-service)
                (description "Service for setting up profile env vars.")
                (extensions
                 (list (service-extension
                        home-environment-variables-service-type
                        home-env-vars-config-gexp)))
                (default-value #f)))
