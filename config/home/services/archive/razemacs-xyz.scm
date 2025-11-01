(define-module (config home services archive razemacs-xyz)
  #:use-module (gnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:export (home-razemacs-packages-profile-service-type
            home-razemacs-packages-profile-service))


(define (home-razemacs-packages-profile-service config)
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
   emacs-visual-fill-column
   emacs-ace-window
   emacs-mct
   emacs-orderless
   emacs-corfu
   emacs-marginalia
   emacs-beframe
   emacs-denote
   emacs-consult-denote
   emacs-markdown-mode
   emacs-flycheck
   emacs-neotree
   emacs-dired-preview

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
   emacs-magit
   emacs-vterm
   emacs-paredit
   emacs-arei
   emacs-guix ;; comes loaded with geiser and geiser-guile etc...
   emacs-macrostep
   emacs-sly
   emacs-arei)) ;; Experimental alternative to geiser

(define home-razemacs-packages-profile-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-razemacs-packages-profile-service)))
                (default-value #f)))
