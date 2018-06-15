;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; eshell settings ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eshell-command-aliases-list
      (append
       (list
	(list "l" "ls -1a")
	(list "g" "git")
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; internal settings ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no backup
(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; yes-no → y-n

(electric-pair-mode 1) ; カッコを補完する
(show-paren-mode t) ; 対応するカッコを強調表示
(require 'smartparens-config)

(global-nlinum-mode 1)
;; (global-nlinum-mode 1) ; line numberを設定する
;; (setq nlinum-format "%5d ") ; 横に5文字分確保する

(setq inhibit-startup-message t) ; 起動メッセージを非表示
(tool-bar-mode -1) ; ツールバーを非表示
(menu-bar-mode -1) ; メニューバーを非表示

(scroll-bar-mode -1) ; スクロールバーを非表示
(global-yascroll-bar-mode 1) ; yascrollを表示

;; 行頭への移動と非空白文字への移動をトグル的に切り替える
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (bolp) (back-to-indentation) (beginning-of-line)))
(define-key global-map "\C-a" 'back-to-indentation-or-beginning)

;; window
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(define-key global-map "\C-q" (make-sparse-keymap)) ;; C-q をプリフィックスキー化
(global-set-key "\C-q\C-q" 'quoted-insert) ;; quoted-insert は C-q C-q へ割り当て
(global-set-key "\C-q\C-r" 'window-resizer) ;; window-resizer は C-q C-r (resize)
(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; terminal ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multiterm
(load-file "~/.emacs.d/site-lisp/multi-term.el")
(require 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; color theme ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'cyberpunk t)
(load-theme 'spacemacs-dark t)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ide settings ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete
;; (require 'auto-complete-config)
;; (ac-set-trigger-key "TAB")
;; (global-auto-complete-mode t)
;; (setq ac-auto-start 2)          ; n文字以上の単語の時に補完を開始
;; (setq ac-delay 0.05)            ; n秒後に補完開始
;; (setq ac-use-fuzzy t)           ; 曖昧マッチ有効
;; (setq ac-use-comphist t)        ; 補完推測機能有効
;; (setq ac-auto-show-menu 0.05)   ; n秒後に補完メニューを表示
;; (setq ac-quick-help-delay 0.5)  ; n秒後にクイックヘルプを表示
;; (setq ac-use-menu-map t)

(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(company-quickhelp-mode)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

(with-eval-after-load 'company
  (company-flx-mode +1)) ;; fuzzy match

;; syntax check
(add-hook 'after-init-hook #'global-flycheck-mode)

;; neotree
(global-set-key [C-x o] 'neotree-toggle)

;; rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; color of parens
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; helm
(helm-mode 1)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(global-set-key (kbd "M-x") 'helm-M-x) ; helm-M-x
(global-set-key (kbd "C-s") 'helm-swoop) ; helm-swoop

;; yasnippet
(yas-global-mode 1)

;; elscreen
(elscreen-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; git settings ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git-gutter+
(global-git-gutter+-mode)
(setq git-gutter+-modified-sign "  ") ;; two space
(setq git-gutter+-added-sign "++")    ;; multiple character is OK
(setq git-gutter+-deleted-sign "--")

(set-face-background 'git-gutter+-modified "purple") ;; background color
(set-face-foreground 'git-gutter+-added "green")
(set-face-foreground 'git-gutter+-deleted "red")

;; Magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(global-set-key (kbd "C-c m") 'magit-status)

(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red"))))
)

;;;;; infra

;; docker
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;; language

;; ocaml
(setq opam-share
  (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))

(setq opam-bin
      (substring (shell-command-to-string "opam config var bin 2> /dev/null") 0 -1))

(add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))

;; yatex
(unless (package-installed-p 'yatex)
  (package-refresh-contents) (package-install 'yatex))

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
        ("\\.ltx$" . yatex-mode)
        ("\\.sty$" . yatex-mode)) auto-mode-alist))
;; set YaTeX coding system
(setq YaTeX-kanji-code 4) ; UTF-8 の設定
(add-hook 'yatex-mode-hook
      '(lambda ()
         (setq YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
         (setq YaTeX-use-hilit19 nil
           YateX-use-font-lock t)
         (setq tex-command "em-latexmk.sh") ; typeset command
         (setq dvi2-command "evince") ; preview command
         (setq tex-pdfview-command "xdg-open"))) ; preview command

;; c
(add-to-list 'company-backends 'company-c-headers)

;; racket
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;; markdown
(setq markdown-command "multimarkdown") ; require multimarkdown command `brew install multimarkdown'

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
