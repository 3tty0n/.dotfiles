(setq opam-share
  (let ((reply (opam-shell-command-to-string "opam config var share")))
    (when reply (substring reply 0 -1))))

(load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))
