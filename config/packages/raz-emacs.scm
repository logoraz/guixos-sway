(define-module (config packages raz-emacs)
  #:use-module (guix download)                      ; -
  #:use-module (guix git-download)                  ; -
  #:use-module (guix packages)                      ; -
  #:use-module (guix build-system copy)             ; -
  #:use-module ((guix licenses) #:prefix license:)) ; -

;;TODO - doesn't work when exported, only with define-public... why?

;;; Note: generate new sha256/base32 via
;;; guix hash -x --serializer=nar .
;;; Get commit via git log
(define-public raz-emacs
  (package
   (name "raz-emacs")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/loraz/raz-emacs.git")
                  (commit "9d805a1e8fb67f42255c3d141ba455afc2a4d3b6")))
            (hash
             (content-hash
              "0l4vfkg508ksb56s3vmj5ndv5l0rj80xifsd5fbvsqn6zj4ggk9p"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/loraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
