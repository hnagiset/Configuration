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
      (set-face-attribute 'default nil :font "Source Code Pro" :height 110))
     ((/init/util/font-exists-p "Adwaita Mono")
      (set-face-attribute 'default nil :font "Adwaita Mono" :height 110))
     ((/init/util/font-exists-p "Monospace")
      (set-face-attribute 'default nil :font "Monospace" :height 110))
     (t (set-face-attribute 'default nil :height 114)))))

(defun /init/util/position-frame ()
  "Resize and position frame to top left."
  (interactive)
  (set-frame-width (selected-frame) 135)
  (set-frame-height (selected-frame) 45)
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
  (insert (shell-command-to-string "echo -n `date +'%Y/%m/%d (%a, %r %Z)'`")))

(defun /init/util/disable-themes ()
  "Disable enabled themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun /init/util/personal-theme ()
  "Load Personal Theme."
  (interactive)
  (/init/util/disable-themes)
  (load-theme 'personal-light t))

(defun /init/util/primary-theme ()
  "Load Primary Theme."
  (interactive)
  (/init/util/disable-themes)
  (require 'doom-themes)
  (load-theme 'doom-moonlight t))

(defun /init/util/secondary-theme ()
  "Load Secondary Theme."
  (interactive)
  (/init/util/disable-themes)
  (require 'doom-themes)
  (setq doom-one-brighter-comments t)
  (load-theme 'doom-one t))

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
                      "standard-light"
                      "standard-dark"
                      "doom-one"
                      "doom-one-light"
                      "doom-laserwave"
                      "doom-moonlight"
                      "anti-zenburn")
     nil t)))
  (when (string-match "^spacemacs" theme-name)
    (require 'spacemacs-theme))
  (when (string-match "^standard" theme-name)
    (require 'standard-themes))
  (when (string-match "^doom" theme-name)
    (require 'doom-themes))
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

;; https://stackoverflow.com/a/35375498
;;
;; "All the primitives that change the buffer set deactivate-mark. [...] To
;; write Lisp code that modifies the buffer without causing deactivation of the
;; mark at the end of the command, bind deactivate-mark to nil around the code
;; that does the modification."
(defun /init/util/occur-selection ()
  (interactive)
  (when (region-active-p)
    (let (deactivate-mark)
      (occur (regexp-quote
              (buffer-substring (region-beginning) (region-end)))))))

(defun /init/util/copy-file-name ()
  (interactive)
  (gui-set-selection 'PRIMARY (buffer-file-name)))

(defun /init/util/view-agenda (year)
  "Show agenda in an Org buffer."
  (interactive (list (read-string "Year: "
                                  (format-time-string "%Y" (current-time)))))
  (switch-to-buffer (get-buffer-create "*agenda*"))
  (read-only-mode -1)
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
  (let* ((timestamp (car (process-lines "date" "+%s")))
         (filename (concat "~/Primordial/Repos/Notes/" timestamp ".org"))
         (date (substring (shell-command-to-string "date +%Y/%m/%d") 0 -1)))
    (find-file filename)
    (erase-buffer)
    (insert "# -*- mode: org -*-\n")
    (insert "#+FILETAGS:\n")
    ;; date includes a newline
    (insert (concat "#+DATE: " date "\n"))
    (insert (format "#+TITLE: %s\n" title)))
  (org-mode))

(defun /init/util/text-box (prefix text)
  "Create a new Org note."
  (interactive "P\nsText: ")
  (let ((length (length text)))
    (if prefix
        (progn
          (insert "┏━" (make-string length ?━) "━┓" "\n")
          (insert "┃ "  text                   " ┃" "\n")
          (insert "┗━" (make-string length ?━) "━┛" "\n"))
      (insert "┌─" (make-string length ?─) "─┐" "\n")
      (insert "│ "  text                   " │" "\n")
      (insert "└─" (make-string length ?─) "─┘" "\n"))))

(defun /init/util/org-insert-screenshot ()
  "Insert latest screenshot into Org document."
  (interactive)
  (let ((screenshot
         (car
          (process-lines "hn-screenshot.sh"
                         (concat (getenv "HOME") "/Pictures/Screenshots")
                         "./images"))))
    (insert (format "[[%s]]" screenshot))))

;; Copied from https://stackoverflow.com/a/11059012
(defun /init/util/bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1.0 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (delete-window (get-buffer-window buf)))
                    buffer)))
