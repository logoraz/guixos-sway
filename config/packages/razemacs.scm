(define-module (config packages razemacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))


;;; Note: generate new sha256/base32 via
;;; guix hash -x --serializer=nar .
;;; Get commit via git log
(define-public razemacs
  (package
   (name "razemacs")
   (version "a9d7ce0c93dbda281348b777a1a234bf4d71994c")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/logoraz/razemacs.git")
                  (commit version)))
            (hash
             (content-hash
              "1m6w7310prcyhg3jajwf1piw5wxm3ckj2qnlyxnbr98xpd1mrz1y"))))
   (build-system copy-build-system)
   (home-page "https://codeberg.org/logoraz/razemacs")
   (synopsis "RazEmacs")
   (description "RazEmacs")
   (license license:gpl3+)))
