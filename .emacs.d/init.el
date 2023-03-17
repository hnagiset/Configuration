;; -*- lexical-binding: t -*-

;; Measure startup time.
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time
                              (time-subtract
                               (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))


(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (user-init-directory (concat user-emacs-directory "init/"))
      (user-themes-directory (concat user-emacs-directory "themes/"))
      (user-packages-directory (concat user-emacs-directory "init/packages/"))
      (user-bindings-directory (concat user-emacs-directory "init/bindings/"))
      (user-elisp-directory (concat user-emacs-directory "elisp/")))

  ;; Setup package.el.
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (require 'package)
  (package-initialize)

  ;; Measure library load times.
  (load-file (concat user-init-directory "measure-load.el"))

  ;; Load custom.el.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; Add elisp directory to load-path.
  (when (file-directory-p user-elisp-directory)
    (add-to-list 'load-path user-elisp-directory)
    (dolist (dir (directory-files user-elisp-directory t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir))))

  ;; Add themes directory to load path.
  (add-to-list 'custom-theme-load-path user-themes-directory)

  (load-file (concat user-init-directory "util.el"))
  (load-file (concat user-init-directory "basic-settings.el"))
  (load-file (concat user-init-directory "c-language.el"))

  ;; Load and configure packages.
  (dolist (pkg '("undo-fu" "evil" "ido" "key-chord" "company"
                 "ledger" "markdown" "htmlize" "org" "whitespace"
                 "which-key" "verilog" "verilog3"))
    (load-file (concat user-packages-directory pkg ".el")))

  ;; Set keybindings.
  (dolist (binding '("core" "dired" "evil" "org" "occur"))
    (load-file (concat user-bindings-directory binding ".el"))))
