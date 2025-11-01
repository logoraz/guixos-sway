(define-module (config home services bash)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:export (bash-config->service))


;; Edit setting the Home User
(define %user-name "logoraz")

(define %source (string-append "/home"
                               "/" %user-name
                               "/guixos"))

(define* (resolve dir #:key file)
  "Resolve local config dir & file"
  (let ((filename (if file (string-append "/" file) "")))
    (local-file (string-append
                 %source "/"
                 dir filename)
                #:recursive? #t)))

(define %gosr (string-append "sudo guix system -L "
                             "~/guixos/ "
                             "reconfigure "
                             "~/guixos/config/system/guixos.scm"))

(define %gohr (string-append "guix home -L "
                             "~/guixos/ "
                             "reconfigure "
                             "~/guixos/config/home/guixos-home.scm"))

(define %gop (string-append "guix pull -L "
                            "~/guixos/"))

(define* (bash-config->service #:key
                               (test #f))
  (service home-bash-service-type
           (home-bash-configuration
             (guix-defaults? #f)
             (aliases
              `(("grep" . "grep --color=auto")
                ("ls"   . "ls -p --color=auto")
                ("ll"   . "ls -l")
                ("la"   . "ls -la")
                ("gosr" . ,%gosr)
                ("gohr" . ,%gohr)
                ("gop"  . ,%gop)))
             (bashrc
              `(,(resolve "files/bash" #:file "dot-bashrc.sh"))
              )
             (bash-profile
              `(,(resolve "files/bash" #:file "dot-bash_profile.sh"))))))
