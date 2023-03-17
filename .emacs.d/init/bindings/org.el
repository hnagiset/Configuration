;; -*- lexical-binding: t -*-

(setq /init/bindings/org/localleader-map (make-sparse-keymap))

(defun /init/bindings/org/org-insert-link-prefix ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-insert-link))

(/init/bindings/core/define-keys /init/bindings/org/localleader-map
  ("a" #'org-archive-subtree-default-with-confirmation
   "Archive This Entry")
  ("e" nil "Export")
  ("ea" #'org-ascii-export-to-ascii "Export to ASCII")
  ("eh" #'org-html-export-to-html "Export to HTML")
  ("i" #'org-insert-structure-template "Insert Structure Template")
  ("l" nil "Links")
  ("li" #'org-insert-link "Insert Link")
  ("lf" #'/init/bindings/org/org-insert-link-prefix "Insert File Link")
  ("o" #'org-insert-heading-respect-content "Add Heading")
  ("p" #'org-latex-preview "Org Latex Preview")
  ("t" #'org-insert-todo-heading-respect-content "Add TODO Heading"))

;; Add localleader-map to evil-normal-state-local-map
(add-hook 'org-mode-hook
          (lambda ()
            (/init/bindings/core/define-keys evil-normal-state-local-map
              (/init/bindings/core/localleader /init/bindings/org/localleader-map))))
