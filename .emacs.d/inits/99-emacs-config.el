;; カーソルの場所を保存する
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/.cache/places")

;; 矩形選択
(cua-mode t)

;; 同じ内容を履歴に保存しない
(setq history-delete-duplicates t)

;; symlinkは必ず追いかける
(setq vc-follow-symlinks t)

;; M-wやC-kでコピーしたものを、他のアプルケーションで貼り付け可能にする
(cond (window-system (setq x-select-enable-clipboard t)))

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;; window の透明度
;; (add-to-list 'default-frame-alist '(alpha . 90))
;; (set-frame-parameter nil 'alpha 0.90)
