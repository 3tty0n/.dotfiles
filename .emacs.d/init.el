(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; internal
(setq initial-scratch-message "")
(setq compilation-scroll-output t)

;; save するごとに white space を消す
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; no backup
(setq make-backup-files nil)
(setq auto-save-default nil)

(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))

(defalias 'yes-or-no-p 'y-or-n-p) ; yes-no → y-n

(electric-pair-mode 1) ; カッコを補完する
(show-paren-mode t) ; 対応するカッコを強調表示
(require 'smartparens-config)

(global-linum-mode)
(hlinum-activate)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:foreground "yellow" :background "black"))))
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))

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

;; set C-h to backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; for window system
(if window-system
    (progn
      ;; (set-frame-parameter nil 'fullscreen 'maximized) ;; スクリーンの最大化
      (set-frame-parameter nil 'alpha 95)
      ))

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

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; ddskk
(global-set-key (kbd "C-x C-j") 'skk-mode)

(eval-after-load 'skk-mode
  '(progn
     (setq skk-server-prog "~/.rbenv/shims/google-ime-skk") ; google-ime-skkの場所
     (setq skk-server-inhibit-startup-server nil) ; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
     (setq skk-server-host "localhost") ; サーバー機能を利用
     (setq skk-server-portnum 55100)     ; ポートはgoogle-ime-skk
     (setq skk-share-private-jisyo t)   ; 複数 skk 辞書を共有

     (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
     (setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に

     (setq skk-dcomp-activate t) ; 動的補完
     (setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
     (setq skk-dcomp-multiple-rows 10) ; 動的補完の候補表示件数

     (setq skk-egg-like-newline t)
     (setq skk-comp-circulate t)
     ))

;; multiterm
;; (load-file "~/.emacs.d/site-lisp/multi-term.el")
;; (require 'multi-term)
;; (setq multi-term-program "/usr/local/bin/zsh")


;; color theme
(load-theme 'spacemacs-dark t)
;; (load-theme 'alect-dark t)
(powerline-default-theme)

;; ide settings

;; auto-complete
;; (global-auto-complete-mode t)
(eval-after-load 'auto-complete
  '(progn
     (require 'fuzzy)
     (ac-config-default)
     (setq ac-auto-start 2)          ; n文字以上の単語の時に補完を開始
     (setq ac-delay 0)               ; n秒後に補完開始
     (setq ac-use-fuzzy t)           ; 曖昧マッチ有効
     (setq ac-use-comphist t)        ; 補完推測機能有効
     (setq ac-auto-show-menu 0.05)   ; n秒後に補完メニューを表示
     (setq ac-quick-help-delay 0.5)  ; n秒後にクイックヘルプを表示
     (setq ac-use-menu-map t)
     (setq ac-ignore-case t)
     (ac-set-trigger-key "TAB")))

(global-company-mode);
(eval-after-load 'company
  '(progn
     (setq company-idle-delay 0) ; デフォルトは0.5
     (setq company-minimum-prefix-length 2) ; デフォルトは4
     (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

     (define-key company-active-map (kbd "M-n") nil)
     (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "C-h") nil)

     (global-set-key (kbd "C-M-i") 'company-complete)
     ))

;; syntax check
(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;  neotree
;; (global-set-key (kbd "C-x C-o") 'neotree-toggle)

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
(eval-after-load 'helm-mode
  '(progn
     (require 'helm)
     (helm-autoresize-mode 1)
     (define-key global-map (kbd "C-x C-f") 'helm-find-files)
     (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

     (global-set-key (kbd "M-x") 'helm-M-x) ; helm-M-x
     (global-set-key (kbd "C-s") 'helm-swoop) ; helm-swoop
     (global-set-key (kbd "C-x C-l") 'helm-ls-git-ls) ; helm-ls-git
     (global-set-key (kbd "C-x C-d") 'helm-browse-project) ; helm-brose-project
     (global-set-key (kbd "C-x C-p") 'helm-git-grep) ; helm-git-grep
     (global-set-key (kbd "C-x C-n") 'helm-ghq)

     (setq helm-mode-fuzzy-match t)
     (setq helm-completion-in-region-fuzzy-match t)

     (require 'helm-smex)
     (global-set-key [remap execute-extended-command] #'helm-smex)
     (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)
     ))

;; yasnippet
(yas-global-mode 1)

;; elscreen
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; git

;; git-gutter
;; (global-git-gutter-mode t)
;; (git-gutter:linum-setup)
;; (custom-set-variables
;;  '(git-gutter:update-interval 5))

;; Magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(global-set-key (kbd "C-c m") 'magit-status)



;;;;; infra

;; docker
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;; language

;; ocaml
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null")
       0 -1))

(setq opam-bin
      (substring
       (shell-command-to-string "opam config var bin 2> /dev/null")
       0 -1))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp/"))
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(add-to-list 'auto-mode-alist '("dune" . tuareg-dune-mode))
(add-hook 'tuareg-mode-hook
	  #'(lambda()
	      (setq mode-name "🐫")
	      (auto-fill-mode 1)
	      ))

(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger" t)

;; merlin
(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
(setq merlin-command (concat opam-bin "/ocamlmerlin"))

(require 'merlin)
(eval-after-load 'merlin
  '(progn
     (setq merlin-error-on-single-line t)
     (set-face-background 'merlin-type-face "skyblue")
     (setq merlin-error-after-save nil)
     (flycheck-ocaml-setup)
     ))

(with-eval-after-load 'auto-complete
  '(progn
     (setq merlin-ac-setup 'easy)
     ))

(with-eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'merlin-company-backend)
     (add-hook 'merlin-mode-hook 'company-mode)
     ))

(add-hook 'tuareg-mode-hook #'merlin-mode)

;; ocp-indent
(load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))

;; utop
(autoload 'utop "utop" "Toplevel for Ocaml" t)
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)


(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
	   (append '(("\\.tex$" . latex-mode)
		     ("\\.ltx$" . latex-mode)
		     ("\\.sty$" . latex-mode)) auto-mode-alist))
(add-hook 'yatex-mode 'turn-on-flyspell)
(add-hook 'latex-mode 'turn-on-flyspell)

;; yatex
(eval-after-load 'yatex-mode
  '(progn
     (unless (package-installed-p 'yatex)
       (package-refresh-contents) (package-install 'yatex))
     ;; set YaTeX coding system
     (setq YaTeX-kanji-code 4) ; UTF-8 の設定
     (add-hook 'yatex-mode-hook
	       '(lambda ()
		  (setq YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
		  (setq YaTeX-use-hilit19 nil
			-           YateX-use-font-lock t)
		  (setq tex-command "em-latexmk.sh") ; typeset command
		  (setq dvi2-command "evince") ; preview command
		  (setq tex-pdfview-command "xdg-open"))) ; preview command
     ))

;; c
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-c-headers))

;; racket
(eval-after-load 'racket-mode
  '(progn
     (define-key racket-mode-map (kbd "C-c r") 'racket-run)
     ))

;; scala
(eval-after-load 'ensime
  '(progn
     (setq ensime-startup-notification nil)
     (setq ensime-search-interface 'helm)

     (defun scala/enable-eldoc ()
       "Show error message or type name at point by Eldoc."
       (setq-local eldoc-documentation-function
		   #'(lambda ()
                       (when (ensime-connected-p)
			 (let ((err (ensime-print-errors-at-point))) err))))
       (eldoc-mode +1))

     (defun scala/completing-dot-company ()
       (cond (company-backend
              (company-complete-selection)
              (scala/completing-dot))
             (t
              (insert ".")
              (company-complete))))

     (defun scala/completing-dot-ac ()
       (insert ".")
       (ac-trigger-key-command t))
     ))

;; gnuplot
(add-to-list 'auto-mode-alist '("\\.plot" . gnuplot-mode))

;; markdown
(setq markdown-command "multimarkdown") ; require multimarkdown command `brew install multimarkdown'

;; org-mode
(eval-after-load 'org-mode
  '(progn
     (setq org-startup-with-inline-images t)
     (setq org-hide-leading-stars t) ;; 見出しの余分な*を消す
     (setq org-clock-into-drawer t) ;; LOGBOOK drawerに時間を格納する
     (setq my-org-agenda-dir "~/org/") ;; org-directory内のファイルすべてからagendaを作成する
     (setq org-agenda-files (list my-org-agenda-dir))
     (setq org-todo-keywords
	   '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)"))) ;; TODO状態
     (setq org-log-done 'time) ;; DONEの時刻を記録
     ;; keybindings
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)
     ))

(defun org-open-scrum-todays-file ()
  "Open .org file for scrum in ~/org/scrum directory."
  (interactive)
  (setq org-todays-scrum-file
	(concat
	 (concat
	  "~/org/scrum/"
	  (format-time-string "%Y-%m-%d"))
	 ".org"))
  (find-file org-todays-scrum-file))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (latex-preview-pane yasnippet-snippets company-auctex auctex helm-fuzzy-find quickrun hlinum helm-ghq open-junk-file rspec-mode alect-themes elscreen-multi-term multi-term git-gutter-fringe ddskk docker-api dockerfile-mode yatex yascroll yaml-mode wgrep undo-tree spacemacs-theme smartparens restart-emacs rainbow-delimiters racket-mode pallet nlinum neotree multiple-cursors molokai-theme markdown-mode kubernetes irony helm-swoop helm-smex helm-ls-git helm-git-grep gnuplot git-gutter+ fzf flycheck-ocaml flycheck-cask exec-path-from-shell ensime elscreen el-get docker cyberpunk-theme counsel company-quickhelp company-flx company-c-headers cask-mode auto-complete all-the-icons))))
