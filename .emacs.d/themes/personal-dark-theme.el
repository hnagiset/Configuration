;; -*- lexical-binding: t -*-

(deftheme personal-dark "Personal theme inspired by Spacemacs and Bespoke.")

(let ((background-color "#171421")
      (foreground-color "#ffffff")
      (region-color     "#d7d7ff")
      (active-color     "#2b2836")
      (inactive-color   "#18171c")
      (filename-color   "#7568a1")
      (faded-color      "#ababab")
      (function-color   "#250361")
      (trail-ws-color   "#f0823e")
      (comment-fg-color "#49005c")
      (comment-bg-color "#fbf8ef")
      (salient-color    "#303db4")
      (purple-color     "#6c3163")
      (blue-color       "#7f9ee3")
      (red-color        "#a8326f")
      (green-color      "#32a852")
      (brown-color      "#a86432"))
  (custom-theme-set-faces
   'personal-dark

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
   `(outline-1 ((t :foreground ,blue-color  :height 1.3)))
   `(outline-2 ((t :foreground ,green-color :height 1.2)))
   `(outline-3 ((t :foreground ,red-color   :height 1.1)))
   `(outline-4 ((t :foreground ,brown-color :height 1.0)))
   `(outline-5 ((t :foreground ,blue-color  :height 1.0)))
   `(outline-6 ((t :foreground ,green-color :height 1.0)))
   `(outline-7 ((t :foreground ,red-color   :height 1.0)))
   `(outline-8 ((t :foreground ,brown-color :height 1.0)))

   ;; Whitespace
   `(whitespace-trailing ((t :background ,trail-ws-color)))

   ;; Org
   `(org-done           ((t :foreground ,faded-color :strike-through t)))
   `(org-document-title ((t :foreground ,foreground-color :height 2.0 :weight bold)))
   `(org-headline-done  ((t :foreground ,faded-color)))
   ))

(provide-theme 'personal-dark)
