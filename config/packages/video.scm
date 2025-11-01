;;; Adapted from:
;;; https://codeberg.org/daviwil/dotfiles/src/branch/master/daviwil/packages/video.scm
(define-module (config packages video)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages video)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages assembly)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp))

;; Generate content hash
;; guix hash --serializer=nar .
(define-public obs-vaapi
  (package
    (name "obs-vaapi")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fzwoch/obs-vaapi")
                     (commit version)))
              (file-name (git-file-name name version))
              (hash
               (content-hash
                "150c8wpyc6xnhg6rg7700xwr59xf6fgcck5wi6s8g1ppfaq8nj6a"))))
    (arguments
     (list
      #:configure-flags #~(list (string-append "--prefix=" #$output "/lib/obs-plugins"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config cmake))
    (inputs (list obs gstreamer gst-plugins-base pciutils simde))
    (home-page "https://github.com/fzwoch/obs-vaapi")
    (synopsis "GStreamer-based VAAPI encoder for OBS.")
    (description "GStreamer-based VAAPI encoder for OBS.")
    (license license:gpl2+)))
