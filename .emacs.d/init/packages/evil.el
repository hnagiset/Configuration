;; -*- lexical-binding: t -*-

(setq evil-shift-width 4)
(setq evil-undo-system 'undo-fu)
(setq evil-want-C-u-scroll t)
;; Case sensitive search in ex mode.
(setq evil-ex-search-case 'sensitive)
;; Make * search for symbol rather than word.
(setq evil-symbol-word-search t)

(/init/util/require-package 'evil)
(require 'evil)

;; Move forward and backward by symbol instead of word.
(defalias #'forward-evil-word #'forward-evil-symbol)

(evil-select-search-module 'evil-search-module
                           'evil-search)

(evil-mode)
