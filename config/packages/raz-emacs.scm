(define-module (config packages raz-emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

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
                  (commit "da4caed6a9a7349308687a53b87ee289220c372b")))
            (hash
             (content-hash
              "0hjyaawdnqx1v14vcrzldja53xbqz8ni4ha8rjk1dnah3w9z8aqz"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/loraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
