;; -*- lexical-binding: t -*-

(setq /init/bindings/core/leader ",")
(setq /init/bindings/core/localleader "SPC")
(setq /init/bindings/core/global-leader-map (make-sparse-keymap))

(defmacro /init/bindings/core/define-keys (keymap &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(car (cddr binding))))
                   (when seq
                     (define-key ,keymap (kbd seq) func))
                   (when desc
                     (which-key-add-keymap-based-replacements
                       ,keymap seq desc))))))

;;; Mapped Functions

(defun /init/bindings/core/delete-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (> (count-windows) 1)
    (delete-window)))

(defun /init/bindings/core/open-marks ()
  (interactive)
  (split-window-vertically)
  (find-file "~/.marks")
  (markdown-mode))

(defun /init/bindings/core/open-notes ()
  (interactive)
  (find-file "~/Cloud/org/notes.org"))

(defun /init/bindings/core/open-consolidated-notes ()
  (interactive)
  (find-file "~/Primordial/Repos/Notes/consolidated.org")
  (read-only-mode))

(defun /init/bindings/core/open-scratch-buffer nil
  "Open/Create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;; Leader Mappings
;; Use only lowercase letters
(/init/bindings/core/define-keys /init/bindings/core/global-leader-map
  ("b" #'ido-switch-buffer "Switch Buffer")
  ("d" #'/init/util/insert-date "Insert Date")
  ("f" #'ido-find-file "Find File")
  ("n" #'/init/util/create-note "Create Note")
  ("o" nil "Open...")
  ("oa" #'org-agenda-list "Open Agenda")
  ("on" #'/init/bindings/core/open-notes "Open Notes")
  ("oc" #'/init/bindings/core/open-consolidated-notes "Open Consolidated Notes")
  ("oo" #'/init/util/open-occur-by-major-mode "Open Occur")
  ("os" #'/init/bindings/core/open-scratch-buffer "Open Scratch Buffer")
  ("oy" #'/init/util/view-agenda "Open Year")
  ("p" nil "Preferences...")
  ("p-" #'text-scale-decrease "Decrease Text Scale")
  ("p+" #'text-scale-increase "Increase Text Scale")
  ("p0" (lambda () (interactive) (text-scale-mode 0)) "Reset Text Scale")
  ("pf" #'toggle-frame-fullscreen "Toggle Frame Fullscreen")
  ("pm" #'menu-bar-mode "Toggle Menu Bar")
  ("ps" #'flyspell-mode "Toggle Spell Check On/Off")
  ("pt" nil "Change Theme")
  ("pth" #'/init/util/personal-theme "Personal Theme")
  ("ptp" #'/init/util/primary-theme "Primary Theme")
  ("pts" #'/init/util/secondary-theme "Secondary Theme")
  ("ptl" #'/init/util/load-theme "Load Theme")
  ("ptn" #'/init/util/disable-themes "No Theme")
  ("pw" 'toggle-truncate-lines "Toggle Line-Wrap")
  ("q" #'/init/bindings/core/delete-buffer "Delete Buffer")
  ("w" nil "Windows...")
  ("wc" #'evil-window-delete "Delete Window")
  ("wh" #'evil-window-left "Move Left")
  ("wj" #'evil-window-down "Move Down")
  ("wk" #'evil-window-up "Move Up")
  ("wl" #'evil-window-right "Move Right")
  ("wo" #'delete-other-windows "Delete Other Windows"))

(setq /init/bindings/core/global-map
      (copy-keymap /init/bindings/core/global-leader-map))

;;; Global Mappings
;; Use only uppercase letters
(/init/bindings/core/define-keys /init/bindings/core/global-map
  ("R" (lambda ()
         (interactive)
         (save-buffer)
         (recompile))
   "Save and Recompile")
  ("C" #'company-complete "Company Complete")
  ("G" (lambda ()
         (interactive)
         (message buffer-file-name))
   "Display File Name")
  ("N" #'evil-complete-next "Complete Next")
  ("P" #'evil-complete-previous "Complete Previous")
  ("S" #'ispell-word "Correct Spelling at Point")
  ("V" (lambda ()
         (interactive)
         (insert (gui-get-selection 'PRIMARY 'UTF8_STRING)))
   "Paste Contents of Primary Clipboard")
  ("O" #'just-one-space "Just One Space"))

(/init/bindings/core/define-keys evil-motion-state-map
  (/init/bindings/core/leader /init/bindings/core/global-leader-map "Leader"))

(/init/bindings/core/define-keys global-map
  ("C-c" /init/bindings/core/global-map))
