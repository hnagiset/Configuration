;; -*- lexical-binding: t -*-

(setq /init/bindings/dired/localleader-map (make-sparse-keymap))

(defun /init/bindings/dired/up-directory ()
  "Go to parent directory and set sidebar properties."
  (interactive)
  (let ((is-sidebar (bound-and-true-p /init/bindings/core/is-dired-sidebar)))
    (dired-up-directory)
    (when is-sidebar (/init/bindings/core/set-dired-sidebar-properties))))

(/init/bindings/core/define-keys /init/bindings/dired/localleader-map
  ("u" #'/init/bindings/dired/up-directory "Up Directory"))

;; Add localleader-map to dired-mode-map.
;;(with-eval-after-load 'dired
;;  (/init/bindings/core/define-keys dired-mode-map
;;    (/init/bindings/core/localleader /init/bindings/dired/localleader-map)))

;; Add some evil bindings for navigation.
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "w") 'evil-forward-word-begin)
            (define-key evil-normal-state-local-map
              (kbd "gg") 'evil-goto-first-line)
            (define-key evil-normal-state-local-map
              (kbd "G") 'evil-goto-line)))
