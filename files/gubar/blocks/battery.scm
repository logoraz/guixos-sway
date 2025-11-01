(define-module (files gubar blocks battery)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module ((ice-9 format) #:prefix fmt:)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (battery))


(define (find-prefix-match prefix lst)
  "Scan a list of strings LST and return the first element that starts with
PREFIX. Returns #f if no match is found."
  (find (lambda (s) (string-prefix? prefix s)) lst))

(define* (battery #:key (format "~a ~a%") (nerd-icons #f) (interval 3))
  (let* ((power-supply-path "/sys/class/power_supply/")
         (bat (string-append
               power-supply-path
               (find-prefix-match "BAT" (scandir power-supply-path))
               "/capacity"))
         (ac (string-append
              power-supply-path
              (find-prefix-match "AC" (scandir power-supply-path))
              "/online"))
         (icons '(󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹)))
    (gublock
     #:block '(("name" . "battery") ("full_text" . "N/A"))
     #:interval 3
     #:procedure
     (lambda (block)
       (let* ((level (string->number (get-line (open-input-file bat))))
              (ac (string->number (get-line (open-input-file ac))))
              (label
               (cond
                ((= ac 1) (if nerd-icons "󰂄" "CHRG:"))
                (nerd-icons (list-ref icons (truncate-quotient level 10)))
                (else "BAT:")))
              (block-alist (block->scm block)))
         (set-block-full-text! block (fmt:format #f format label level))
         (set-block-urgent! block (<= level 10))
         block)))))
