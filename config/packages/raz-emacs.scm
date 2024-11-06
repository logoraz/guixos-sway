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
                  (commit "281d54426e0273286a980223f78c804cc7ee0c5e")))
            (hash
             (content-hash "0dnwfvmh2d7nkwpal8krsz7cg8c42d922chk9mq0ipc3sxqmjl1g"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/loraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
