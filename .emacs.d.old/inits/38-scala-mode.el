(require 'ensime)
(require 'sbt-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'sbt-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)))

(setq ensime-completion-style 'company)

(defun scala/enable-eldoc ()
  "Show error message at point by Eldoc."
  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point)))
                      (and err (not (string= err "")) err)))))
  (eldoc-mode +1))
(add-hook 'ensime-mode-hook #'scala/enable-eldoc)

(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)
