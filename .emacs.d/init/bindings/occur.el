;; -*- lexical-binding: t -*-

;; Start in normal mode.
(evil-set-initial-state 'occur-mode 'motion)

;; Press <return> to goto occurrence.
(evil-define-key 'motion occur-mode-map
  [mouse-1] #'occur-mode-goto-occurrence
  (kbd "<return>") #'occur-mode-display-occurrence
  (kbd "M-<return>") #'occur-mode-goto-occurrence)
