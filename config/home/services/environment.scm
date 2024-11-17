(define-module (config home services environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:export (home-env-vars-configuration-service-type))

;; borrowed from https://codeberg.org/daviwil/dotfiles/daviwil/systems/common.scm
(define (home-env-vars-config-list config)
  `( ;; Sort hidden (dot) files first in ls listings
    ("LC_COLLATE" . "C")

    ;; Set Emacs as editor
    ("EDITOR" . "emacs")
    ("VISUAL" . "emacs")

    ;; Set quotebrowser as the default
    ("BROWSER" . "qutebrowser")

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
    ("QT_QPA_PLATFORM"     . "wayland-egl")

    ;; GTK & QT Theme
    ("GTK_THEME" . "Adwaita:dark")
    ("QT_STYLE_OVERRIDE" . "adwaita")
    ("QT_QPA_PLATFORMTHEME" . "gtk3")
    ;;TODO use gexp local-file to resolve this file...
    ("GTK2_RC_FILES" . "/home/loraz/.guix-home/profile/share/themes/Adwaita-dark/gtk-2.0/gtkrc")

    ;; Set XDG environment variables
    ("XDG_DOWNLOAD_DIR" . "/home/loraz/Downloads")
    ("XDG_PICTURES_DIR" . "/home/loraz/Pictures/Screenshots")))


(define home-env-vars-configuration-service-type
  (service-type (name 'home-profile-env-vars-service)
                (description "Service for setting up profile env vars.")
                (extensions
                 (list (service-extension
                        home-environment-variables-service-type
                        home-env-vars-config-list)))
                (default-value #f)))
