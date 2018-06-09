(global-set-key (kbd "C-x C-o") 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; show hidden files
(setq neo-show-hidden-files t)

;; open the file automatically creating new file
(setq neo-create-file-auto-open t)

;; don't remove the neotree window by delete-other-window
(setq neo-persist-show t)

;; make keybinds simple
(setq neo-keymap-style 'concise)

;; show current directry showing neotree window
(setq neo-smart-open t)

;; https://github.com/jaypei/emacs-neotree/issues/105
(setq neo-vc-integration '(face char))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; popwin
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))
