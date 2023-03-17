;; -*- lexical-binding: t -*-

(setq /init/bindings/python/localleader-map (make-sparse-keymap))

;;(/init/bindings/core/define-keys /init/bindings/python/localleader-map
;;  ("o" (lambda () (interactive) (occur "class\\|def")) "Open Occur"))

(evil-define-key 'normal python-mode-map
  (kbd /init/bindings/core/localleader) /init/bindings/python/localleader-map)
