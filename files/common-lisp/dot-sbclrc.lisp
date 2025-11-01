;;;; dot-sbclrc.lisp -> .sbclrc - SBCL Initialization File

;;; Enable Allegro CL-style Read-Eval-Print Loop for SBCL
(ignore-errors (require :asdf)
               (require :uiop)
               (require :sb-aclrepl))

(when (find-package 'sb-aclrepl)
  (push :aclrepl cl:*features*))

#+aclrepl
(progn
  "From sb-aclrepl example: see ch 17.1 sbcl manual..."
  (setq sb-aclrepl:*max-history* 1000)
  (setf (sb-aclrepl:alias "asdc")
        #'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
  (sb-aclrepl:alias "l" (sys) (asdf:operate 'asdf:load-op sys))
  (sb-aclrepl:alias "t" (sys) (asdf:operate 'asdf:test-op sys))
  ;; The 1 below means that two characaters ("up") are required
  (sb-aclrepl:alias ("up" 1 "Use package") (package) (use-package package))
  ;; The 0 below means only the first letter ("r") is required,
  ;; such as ":r base64"
  (sb-aclrepl:alias ("require" 0 "Require module") (sys) (require sys))
  (setq cl:*features* (delete :aclrepl cl:*features*))
  ;; Alias to quit sbcl repl
  (sb-aclrepl:alias ("quit" 0 "Quit REPL") () (quit))
  ;;
  )
