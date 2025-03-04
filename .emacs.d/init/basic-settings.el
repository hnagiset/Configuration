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
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction nil)

(put 'narrow-to-region 'disabled nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

;;(when (fboundp 'pixel-scroll-precision-mode)
;;  (pixel-scroll-precision-mode 1)
;;  (setq pixel-scroll-precision-use-momentum t)
;;  (setq pixel-scroll-precision-interpolate-page t))

;;(if (fboundp 'global-display-line-numbers-mode)
;;    (global-display-line-numbers-mode 1)
;;  (global-linum-mode 1))

(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(savehist-mode 1)
(desktop-save-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

(/init/util/font-monospace)
(/init/util/position-frame)

;; After selecting buffer through the buffer list, kill the buffer
;; list so the alternate buffer (C-^) works correctly in evil.
(advice-add 'Buffer-menu-this-window :after
            (lambda () (kill-buffer "*Buffer List*")))

(add-hook 'compilation-finish-functions
          '/init/util/bury-compile-buffer-if-successful)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-width frame 110)
            (set-frame-height frame 38)))

;; Set highlight/region color to something more visible.
(set-face-attribute 'region nil :background "#79fcaf")

(/init/util/personal-theme)
