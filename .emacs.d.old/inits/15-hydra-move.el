(require 'hydra)

(global-set-key
 (kbd "C-l")
 (defhydra hydra-move ()
   "move"
   ("f" forward-char "right")
   ("b" backward-char "left")
   ("n" next-line "down")
   ("p" previous-line "up")
   ("d" scroll-up-command "down")
   ("u" scroll-down-command "up")
   ("C-u" beginning-of-buffer "top")
   ("C-d" end-of-buffer "bottom")
   ("." hydra-repeat "repeat")))

(defhydra hydra-vi ()
  "vi"
  ("h" backward-char "←")
  ("j" next-line "↓")
  ("k" previous-line "↑")
  ("l" forward-char "→")
  ("." hydra-repeat "repeat"))

(global-set-key (kbd "C-x C-v") 'hydra-vi/body)
