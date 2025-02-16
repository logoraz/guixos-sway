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
   (version "0.0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/logoraz/guixos-stumpwm.git")
                  (commit "2f5b55cafccbd7b01049e3f87ad601600c17f213")))
            (hash
             (content-hash
              "11yqn9rp1k7kywgfh8brygg239qd5i2k8kfd5l00323412cx3ms8"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/loraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
