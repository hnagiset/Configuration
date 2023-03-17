;; -*- lexical-binding: t -*-

(defun /init/util/font-exists-p (font)
  "Return non-nil if FONT is available."
  (if (null (x-list-fonts font)) nil t))

(defun /init/util/font-monospace ()
  "Set the default font to a monospace font."
  (interactive)
  (when (display-graphic-p)
    (cond
     ((/init/util/font-exists-p "Source Code Pro")
      (set-face-attribute 'default nil :font "Source Code Pro" :height 100))
     ((/init/util/font-exists-p "Monospace")
      (set-face-attribute 'default nil :font "Monospace" :height 120))
     (t (set-face-attribute 'default nil :height 114)))))

(defun /init/util/position-frame ()
  "Resize and position frame to top left."
  (interactive)
  (set-frame-width (selected-frame) 110)
  (set-frame-height (selected-frame) 38)
  (set-frame-position (selected-frame) 0 0))

(defun /init/util/set-comment-char (char)
  "Set the comment character for current buffer."
  (interactive "sComment Character: ")
  (setq comment-start char)
  (font-lock-add-keywords
   nil `((,(concat comment-start ".+") . font-lock-comment-face))))

(defun /init/util/toggle-line-numbers ()
  "Toggle line numbering for the current buffer."
  (interactive)
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 'toggle)
    (linum-mode 'toggle)))

(defun /init/util/write-backup ()
  "Write current buffer to a timestamped backup file."
  (interactive)
  (if buffer-file-name
      (write-region
       (point-min) (point-max)
       (concat buffer-file-name "."
               (substring (shell-command-to-string "date +%s") 0 -1)))
    (message "Current buffer does not have a file name.")))

(defun /init/util/insert-date ()
  "Insert the date and time into the current buffer."
  (interactive)
  (insert (shell-command-to-string "echo -n `date`")))

(defun /init/util/disable-themes ()
  "Disable enabled themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun /init/util/default-theme ()
  "Load Default Theme."
  (interactive)
  (/init/util/disable-themes)
  (load-theme 'personal-light t))

(defun /init/util/load-theme (theme-name)
  "Load a Favorite Theme."
  (interactive
   (list
    (completing-read
     "Theme Name: " '("personal-light"
                      "personal-dark"
                      "spacemacs-light"
                      "spacemacs-dark"
                      "leuven"
                      "solo-jazz"
                      "inkpot"
                      "misterioso"
                      "solarized-light"
                      "solarized-dark"
                      "solarized-wombat-dark"
                      "solarized-zenburn"
                      "anti-zenburn")
     nil t)))
  (when (string-match "^spacemacs" theme-name)
    (require 'spacemacs-common))
  (/init/util/disable-themes)
  (load-theme (intern theme-name) t))

(defun /init/util/require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun /init/util/open-occur-by-major-mode ()
  (interactive)
  (let* ((verilog-pattern (concat "module\\|"
                                  ;; Match heading comments.
                                  "//[[:alnum:][:blank:]]+//"))
         (regexp-alist
          `((python-mode . "class\\|def")
            (emacs-lisp-mode . "(def")
            (verilog-mode . ,verilog-pattern)
            (verilog3-mode . ,verilog-pattern)))
         active)
    (setq active (assoc major-mode regexp-alist))
    (when active
      (occur (cdr active))
      (other-window 1))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun /init/util/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun /init/util/copy-file-name ()
  (interactive)
  (gui-set-selection 'PRIMARY (buffer-file-name)))

(defun /init/util/view-agenda (year)
  "Show agenda in an Org buffer."
  (interactive (list (read-string "Year: " "2023")))
  (switch-to-buffer (get-buffer-create "*agenda*"))
  (erase-buffer)
  (insert "# -*- mode: org -*-\n")
  (insert "#+TITLE: Agenda\n")
  (insert (shell-command-to-string
           (concat "~/Cloud/org/agenda.py " year)))
  (org-mode)
  (org-update-statistics-cookies t)
  (read-only-mode))

(defun /init/util/verilog-connect-ports (begin end)
  "Convert a list of port names to port connections."
  (interactive "r")
  (let ((pgm "awk")
        (opt "{printf \".%-30s (%s),\\n\", $1, $1}"))
    (call-process-region begin end pgm t t t opt)))

(defun /init/util/verilog-isolate-signals ()
  "Isolate signal names from port or variable declarations."
  (interactive)
  (let ((pgm "grep")
        (o1  "-Eo")
        (o2  "\\w+\\s*[,;]?\\s*$")
        (o3  "\\w+"))
    (call-process-region (mark) (point) pgm t t t o1 o2)
    (call-process-region (mark) (point) pgm t t t o1 o3)))

(defun /init/util/create-note (title)
  "Create a new Org note."
  (interactive (list (read-string "Title: ")))
  (let* ((timestamp (substring (shell-command-to-string "date +%s") 0 -1))
         (filename (concat "~/Documents/Notes/" timestamp ".org"))
         (date (shell-command-to-string "date +%Y/%m/%d")))
    (find-file filename)
    (erase-buffer)
    (insert "# -*- mode: org -*-\n")
    (insert "#+FILETAGS:\n")
    ;; date includes a newline
    (insert (concat "#+DATE: " date))
    (insert "#+TITLE: " title "\n"))
  (org-mode))