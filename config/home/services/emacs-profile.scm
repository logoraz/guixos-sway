(define-module (config home services emacs-profile)
  #:use-module (gnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:export (home-emacs-packages-profile-service-type))


(define (home-emacs-packages-profile-service config)
  (list
   emacs-pgtk
   ;; Core functionality
   emacs-diminish
   emacs-delight
   emacs-nord-theme
   emacs-doom-themes
   emacs-all-the-icons
   emacs-nerd-icons
   emacs-doom-modeline
   emacs-ligature
   emacs-no-littering
   emacs-ws-butler
   emacs-undo-tree
   emacs-ace-window
   emacs-mct
   emacs-orderless
   emacs-corfu
   emacs-marginalia
   emacs-beframe
   emacs-denote
   emacs-consult-denote
   emacs-all-the-icons-dired
   emacs-dired-preview
   emacs-colorful-mode
   emacs-ednc
   emacs-exwm-modeline
   ;; emacs-ready-player ;;--> not yet upstream

   ;; Mail & Org
   ;;https://packages.guix.gnu.org/packages/emacs-pinentry/0.1-1.dcc9ba0/
   emacs-pinentry
   emacs-mbsync
   emacs-org-superstar
   emacs-org-appear

   ;; IRC & Tools
   emacs-erc-hl-nicks
   emacs-erc-image
   emacs-emojify
   emacs-0x0 ;; a pastebin integration tool

   ;; Development Packages
   emacs-markdown-mode
   emacs-flycheck
   emacs-neotree
   emacs-colorful-mode
   emacs-xr
   ;; Core Development Packages
   emacs-magit
   emacs-vterm
   emacs-paredit
   emacs-arei
   emacs-guix ;; comes loaded with geiser and geiser-guile etc...
   emacs-macrostep
   emacs-sly
   emacs-arei)) ;; Experimental alternative to geiser

(define home-emacs-packages-profile-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-packages-profile-service)))
                (default-value #f)))
