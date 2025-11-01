(define-module (config home services archive razemacs)
  #:use-module (ice-9 format)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services utils)
  #:use-module (gnu packages emacs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (config packages razemacs)
  #:export (home-razemacs-service-type
            home-razemacs-configuration))

;; Adapted from:
;; https://systemcrafters.net/live-streams/july-8-2022/
(define-configuration home-razemacs-configuration
  (package
   (package razemacs)
   "The RazEmacs package to use.")
  (emacs-package
   (package emacs-pgtk)
   "The Emacs package to use.")
  ;; (config-file
  ;;  (text-config '())
  ;;  "The config.el file.")
  ;; (early-config-file
  ;;  (text-config '())
  ;;  "The early-config.el file.")
  )

(define package-user-dir "~/.cache/emacs/elpa")

(define (home-razemacs-files-service config)
  (list
   `(".config/emacs" ,razemacs)
   ;; `(".config/razemacs/config.el"
   ;;   ,(mixed-text-file "config.el"
   ;;                     ";; RazEmacs Config from Guix Home\n"
   ;;                     (home-razemacs-configuration-config-file config)))
   ;; `(".config/razemacs/early-config.el"
   ;;   ,(mixed-text-file "early-config.el"
   ;;                     ";; RazEmacs Early Config from Guix Home\n"
   ;;                     (format #f "(make-directory \"~a\" t)\n" package-user-dir)
   ;;                     (format #f "(setq package-user-dir \"~a\")\n" package-user-dir)
   ;;                     (home-razemacs-configuration-early-config-file config)))
   ;; end
   ))

(define (home-razemacs-profile-service config)
  (list
   (home-razemacs-configuration-package config)
   (home-razemacs-configuration-emacs-package config)))

(define home-razemacs-service-type
  (service-type (name 'home-raz-emacs)
                (description "The home service to confiure Raz Emacs.")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-razemacs-files-service)
                       (service-extension
                        home-profile-service-type
                        home-razemacs-profile-service)))
                (default-value (home-razemacs-configuration))))
