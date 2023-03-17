;; -*- lexical-binding: t -*-

(deftheme personal-light "Personal theme inspired by Spacemacs and Bespoke.")

(defgroup personal-light-theme nil
  "Personal Light Theme Options"
  :group 'faces)

(defcustom personal-light-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'personal-light-theme)

(defconst personal-light-theme-org-palette
  (let* ((random1  '("#d81e5b" "#6b2b06" "#25283d" "#004f2d" "#39375b"))
         (blues1 '("#197bbd" "#125e8a" "#204b57"))
         (blues2 '("#0a2239" "#53a2be" "#1d84b5" "#132e32" "#176087"))
         (l2dgreen '("#5bba6f" "#3fa34d" "#2a9134" "#137547" "#054a29"))
         (d2lgreen (reverse l2dgreen))
         (purp-blue '("#673c4f" "#7f557d" "#726e97" "#7698b3"))
         (first '("#3256a8" "#32a852" "#a8326f" "#a86432")) ; Original palette.
         (random2 '("#49757a" "57493d" "#499552" "#9da503" "#7f8310"))
         (black '("#000000"))
         (random3 '("#009ffd" "#531CB3" "#ffa400"))
         (purple '("#0a2463" "#531CB3" "#912f40" "#3a5e53"))
         (random4 '("#531CB3" "#FC2F00" "#034732")))
    purple)
  "Color palette for org mode.")

;; (background-color "#fdf6e3") ; Solarized Base3
;; (region-color     "#d7d7ff")
;; (faded-color      "#ababab")

;;(background-color "#eeeeec") ; Tango Light

(let ((background-color "#ffffff")
      (foreground-color "#000000")
      (region-color     "#e7e7ff")
      (active-color     "#e7e5ec")
      (inactive-color   "#f7f5fb")
      (filename-color   "#282b35")
      (faded-color      "#8b8b8b")
      (trail-ws-color   "#f0823e")
      (palette          personal-light-theme-org-palette)
      (palette-length   (length personal-light-theme-org-palette)))
  (custom-theme-set-faces
   'personal-light

   ;; Basic Faces
   `(default ((t :background ,background-color
                 :foreground ,foreground-color)))
   `(region ((t :background ,region-color)))
   `(fringe ((t :background ,background-color)))
   `(mode-line-buffer-id ((t :weight bold :foreground ,filename-color)))
   `(mode-line
     ((t :foreground ,foreground-color
         :background ,active-color
         :box (:line-width 3 :color ,active-color :style nil))))
   `(mode-line-inactive
     ((t :foreground ,foreground-color
         :background ,inactive-color
         :box (:line-width 3 :color ,inactive-color :style nil))))

   ;; Outline
   ;;`(outline-1 ((t :foreground ,(nth (mod 0 palette-length) palette)
   ;;                ,@(when personal-light-theme-org-height '(:height 1.3)))))
   ;;`(outline-2 ((t :foreground ,(nth (mod 1 palette-length) palette)
   ;;                ,@(when personal-light-theme-org-height '(:height 1.25)))))
   ;;`(outline-3 ((t :foreground ,(nth (mod 2 palette-length) palette)
   ;;                ,@(when personal-light-theme-org-height '(:height 1.2)))))
   ;;`(outline-4 ((t :foreground ,(nth (mod 3 palette-length) palette))))
   ;;`(outline-5 ((t :foreground ,(nth (mod 4 palette-length) palette))))
   ;;`(outline-6 ((t :foreground ,(nth (mod 5 palette-length) palette))))
   ;;`(outline-7 ((t :foreground ,(nth (mod 6 palette-length) palette))))
   ;;`(outline-8 ((t :foreground ,(nth (mod 7 palette-length) palette))))

   ;; Whitespace
   `(whitespace-trailing ((t :background ,trail-ws-color)))

   ;; Org
   ;; Default is "Red1"/"#ff0000"
   ;;`(org-todo           ((t :foreground "#ff0000" :weight bold :inherit default)))
   ;; Default is "ForestGreen"/"#228b22"
   ;`(org-done           ((t :foreground "#228b22" :strike-through t :inherit default)))
   ;; Default is "midnight-blue"/"#191970"
   ;;`(org-document-title ((t :foreground "#191970" :height 2.0 :weight bold)))
   ;;`(org-headline-done  ((t :foreground ,faded-color :strike-through t :inherit default)))
   ;;`(org-headline-done  ((t :foreground ,faded-color :inherit default)))

   ;;`(org-priority  ((t :weight bold :height 1.25 :inherit font-lock-keyword-face)))
   ))

(provide-theme 'personal-light)
