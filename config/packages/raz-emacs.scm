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
                  (url "https://github.com/logoraz/raz-emacs.git")
                  (commit "5ef8401ed385edf08181769af7c4d02972786b8e")))
            (hash
             (content-hash
              "1wr5bl8sivk0dwjbk6kfirlzsb3g1dwbsm1cyzb5f0yms8zr3wla"))))
   (build-system copy-build-system)
   (home-page "https://github.com/logoraz/raz-emacs")
   (synopsis "Raz Emacs")
   (description "Raz Emacs")
   (license license:agpl3+)))
