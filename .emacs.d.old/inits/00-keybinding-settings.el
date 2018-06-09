(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8-with-signature)

(if window-system (progn
    (when (equal system-type 'darwin)
      (setq mac-option-modifier 'meta))
    ))

(if window-system (progn
    (when (equal system-type 'darwin)
      (add-to-list 'default-frame-alist '(font . "Ricty-12")))
    ))

(when (eq system-type 'darwin)
    (setq ns-command-modifier (quote meta)))
