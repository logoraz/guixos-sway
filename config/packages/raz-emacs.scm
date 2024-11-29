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
                  (commit "f5d8550ae6cfdf6a85d316bf3a06e4b8fc37c59b")))
            (hash
             (content-hash
              "08i6b3jfshb3dvlv3nqgj2649p4qpy9i1x3j7j76hhc1yiral9xk"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/loraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
