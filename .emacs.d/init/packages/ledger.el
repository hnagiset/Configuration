;; -*- lexical-binding: t -*-

(setq-default ledger-post-amount-alignment-column 80)
(setq-default ledger-post-account-alignment-column 2)
(setq-default ledger-highlight-xact-under-point nil)

(add-hook 'ledger-mode-hook (lambda () (auto-fill-mode -1)))

(/init/util/require-package 'ledger-mode)
