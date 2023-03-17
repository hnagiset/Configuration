;; -*- lexical-binding: t -*-

;; Use vanilla emacs bindings in the insert mode.
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;; Use tab to toggle folding.
(define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)

;; jk to return to normal state.
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
