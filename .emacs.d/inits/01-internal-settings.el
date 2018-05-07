;; show line number
(global-linum-mode t)

;; file encoding
(set-locale-environment "utf-8")
(prefer-coding-system 'utf-8)

;; no starting window
(setq inhibit-startup-message t)

;; scroll step
(setq scroll-step 1)

;; settings for ( )
(show-paren-mode 1)
(electric-pair-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#ffff00" :weight 'extra-bold)

;; no backup file
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; hide toolbar
(tool-bar-mode -1)

;; no message in scrach
(setq initial-scratch-message "")

;; make yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; swich from a window to another windown with "C-t"
;; (define-key global-map (kbd "C-t") 'other-window)

(global-set-key (kbd "C-m") 'newline-and-indent)

;; language settings
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; set tab width 2
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; share the macOS clipboard and emacs' kill-ring

(cond
 ((string-equal system-type "darwin")
  (defun copy-from-macos ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-macos (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  ))

(setq interprogram-cut-function 'paste-to-macos)
(setq interprogram-paste-function 'copy-from-macos)

;; hidden scrool bar
(scroll-bar-mode -1)

;; tabstop
(setq-default tab-width 2)
(setq tab-width 2)

;; whitespace
(setq-default indent-tabs-mode nil)       ; インデントはタブではなくスペースを使用
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト
;; (add-hook 'font-lock-mode-hook            ; タブをハイライト
;;          (lambda ()
;;            (font-lock-add-keywords
;;             nil
;;            '(("\t" 0 'trailing-whitespace prepend)))))

;; window size
;; http://d.hatena.ne.jp/mooz/20100119/p1
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

(global-set-key "\C-x\C-r" 'window-resizer)

;; http://syo-t.hateblo.jp/entry/2016/12/09/215543
(define-key global-map (kbd "C-x |") (kbd "C-x 3 C-x o"))
(define-key global-map (kbd "C-x -") (kbd "C-x 2 C-x o"))
(define-key global-map (kbd "C-x l") 'windmove-right)
(define-key global-map (kbd "C-x h") 'windmove-left)
(define-key global-map (kbd "C-x j") 'windmove-down)
(define-key global-map (kbd "C-x k") 'windmove-up)
(define-key global-map (kbd "C-x w") (kbd "C-x 0"))
(define-key global-map (kbd "C-x q") (kbd "C-x 1"))
