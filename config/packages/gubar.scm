(define-module (config packages gubar)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (srfi srfi-1))

;; Generate content hash
;; guix hash --serializer=nar .
(define-public gubar
  (package
    (name "gubar")
    (version "4777186") ;; --> main
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://codeberg.org/trevarj/gubar")
                     (commit version)))
              (file-name (git-file-name name version))
              (hash
               (content-hash
                "19ipikcmw735qmq6wh3ydy1z778z1amsr4pi1fi1b878ii1zmgmc"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules
           `(((guix build guile-build-system)
              #:select
              (target-guile-effective-version))
             ,@%default-gnu-imported-modules)
           #:phases
           (with-imported-modules
               `((guix build guile-build-system)
                 ,@%default-gnu-imported-modules)
             (gexp (modify-phases
                       %standard-phases
                     (add-after
                         'install
                         'hall-wrap-binaries
                       (lambda* (#:key inputs #:allow-other-keys)
                         (let* ((version (target-guile-effective-version))
                                (site-ccache
                                 (string-append
                                  "/lib/guile/"
                                  version
                                  "/site-ccache"))
                                (site (string-append
                                       "/share/guile/site/"
                                       version))
                                (dep-path
                                 (lambda (env path)
                                   (list env
                                         ":"
                                         'prefix
                                         (cons (string-append
                                                (ungexp output)
                                                path)
                                               (map (lambda (input)
                                                      (string-append
                                                       (assoc-ref
                                                        inputs
                                                        input)
                                                       path))
                                                    (list "guile-fibers"
                                                          "guile-json"))))))
                                (bin (string-append (ungexp output) "/bin/")))
                           (for-each
                            (lambda (file)
                              (wrap-program
                                  (string-append bin file)
                                (dep-path "GUILE_LOAD_PATH" site)
                                (dep-path
                                 "GUILE_LOAD_COMPILED_PATH"
                                 site-ccache)
                                (dep-path "GUILE_EXTENSIONS_PATH" "/lib")))
                            (list "gubar"))))))))))
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0))
    (propagated-inputs
     (list guile-fibers guile-json-4))
    (synopsis
     "Swaybar generator written in Guile Scheme.")
    (description
     "A bar generator for sway using the swaybar-protocol, written\nand configured with Guile Scheme. It is similar to a program like i3blocks where\nyou can define your own blocks or use some of the provided blocks, which will\nthen be displayed in the bar.")
    (home-page "https://codeberg.org/trevarj/gubar")
    (license license:gpl3+)))
