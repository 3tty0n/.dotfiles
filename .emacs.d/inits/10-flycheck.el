(add-hook 'after-init-hook #'global-flycheck-mode)
(setenv "LC_ALL" "ja_JP.UTF-8")
(setenv "LANG" "ja_JP.UTF-8")

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
