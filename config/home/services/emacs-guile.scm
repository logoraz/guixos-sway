(define-module (config home services emacs-guile)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:export (home-emacs-config-service-type))

;;; Package Transformations
;; Keep for now as an example
;; deploy in package list as (latest-sbcl sbcl)
;; (define latest-sbcl
;;   (options->transformation
;;    '((with-latest . "sbcl"))))

(define (home-emacs-config-profile-service config)
  (list emacs-diminish       ;;|--> gnu packages emacs-xyz
        emacs-delight
        emacs-nord-theme
        emacs-doom-themes
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

        ;; Mail & Org
        ;;https://packages.guix.gnu.org/packages/emacs-pinentry/0.1-1.dcc9ba0/
        emacs-pinentry
        emacs-mbsync
        emacs-org-superstar
        emacs-org-appear

        ;; IRC
        emacs-erc-hl-nicks
        emacs-erc-image
        emacs-emojify

        ;; Development Packages
        emacs-magit
        emacs-vterm
        emacs-paredit
        emacs-guix
        emacs-macrostep
        emacs-sly
        ;; emacs-arei ;; Experimental alternative to geiser

        ;; Utilities
        git
        (list git "send-email")

        ;; Guile Scheme Emacs Integration
        ;; guile-3.0
        ;; guile-next
        ;; guile-ares-rs
        guile-colorized
        ;; Common Lisp Integration
        sbcl
        sbcl-slynk))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)))
                (default-value #f)))
