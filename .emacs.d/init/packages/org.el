;; -*- lexical-binding: t -*-

(setq org-startup-folded t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
(setq org-log-done 'time)
(setq org-cycle-open-archived-trees nil)
(setq org-archive-location "~/Cloud/org/archive.org::* From %s")
(setq org-adapt-indentation nil)

(setq org-agenda-files '("~/Cloud/org/todo.org"))
(setq org-agenda-span 10)
(setq org-agenda-start-day "-4d")
(setq org-agenda-skip-scheduled-if-done t)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
