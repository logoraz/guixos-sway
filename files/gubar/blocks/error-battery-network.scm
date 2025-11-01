;;; Error regarding batter.scm gublock

;; Uncaught exception in task:
;; In fibers.scm:
;;     175:8  5 (_)
;; In gubar/gublock.scm:
;;      71:6  4 (gublock-run #<<gublock> block: #<<block> align: #<uns…> …)
;;     50:34  3 (do-procedure #<<gublock> block: #<<block> align: #<un…> …)
;; In gubar/blocks/battery.scm:
;;     33:44  2 (_ #<<block> align: #<unspecified> background: #<unspec…>)
;; In unknown file:
;;            1 (open-file "/sys/class/power_supply/ACAD/online" "r" # # …)
;; In ice-9/boot-9.scm:
;;   1685:16  0 (raise-exception _ #:continuable? _)
;; ice-9/boot-9.scm:1685:16: In procedure raise-exception:
;; In procedure open-file: Too many open files: "/sys/class/power_supply/ACAD/online"
;; Uncaught exception in task:
;; In fibers.scm:
;;     175:8  8 (_)
;; In gubar/gublock.scm:
;;      71:6  7 (gublock-run #<<gublock> block: #<<block> align: #<uns…> …)
;;     50:34  6 (do-procedure #<<gublock> block: #<<block> align: #<un…> …)
;; In gubar/blocks/network-manager-wifi.scm:
;;      39:6  5 (_ #<<block> align: #<unspecified> background: #<unspec…>)
;;     22:10  4 (get-wifi-status)
;; In ice-9/popen.scm:
;;    133:22  3 (open-pipe* "r" "/gnu/store/k66i68s5l93n1lgxp29vn2292n…" …)
;;    112:20  2 (open-process "r" "/gnu/store/k66i68s5l93n1lgxp29vn229…" …)
;; In unknown file:
;;            1 (pipe #<undefined>)
;; In ice-9/boot-9.scm:
;;   1685:16  0 (raise-exception _ #:continuable? _)
;; ice-9/boot-9.scm:1685:16: In procedure raise-exception:
