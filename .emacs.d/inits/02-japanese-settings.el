(setq default-input-method "MacOSX")

(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
(mac-set-input-method-parameter "com.justsystems.inputmethod.atok29.Japanese" `title "あ")

;; カーソルの色
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "red")
(mac-set-input-method-parameter "com.justsystems.inputmethod.atok29.Japanese" `cursor-color "red")
(mac-set-input-method-parameter "com.apple.keylayout.US" `cursor-color "blue")
;; (mac-set-input-method-parameter "com.google.inputmethod.Japanese.Roman" `cursor-color "blue")

;; emacs 起動時は英数モードから始める
(add-hook 'after-init-hook 'mac-change-language-to-us)

;; backslash を優先
(mac-translate-from-yen-to-backslash)

;; minibuffer 内は英数モードにする
(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)