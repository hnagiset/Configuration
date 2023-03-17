;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq epa-armor t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; Scroll one line at a time.
(setq mouse-wheel-progressive-speed nil)            ; Don't accelerate scrolling.
(setq scroll-conservatively 101)
(setq desktop-restore-frames nil)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs #'yes-or-no-p)

(put 'narrow-to-region 'disabled nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

;(if (fboundp 'global-display-line-numbers-mode)
;    (global-display-line-numbers-mode 1)
;  (global-linum-mode 1))
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(savehist-mode 1)
;(desktop-save-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

(/init/util/font-monospace)
(/init/util/position-frame)

;; After selecting buffer through the buffer list, kill the buffer
;; list so the alternate buffer (C-^) works correctly in evil.
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

;; Set highlight/region color to something more visible.
(set-face-attribute 'region nil :background "#79fcaf")

(/init/util/default-theme)
