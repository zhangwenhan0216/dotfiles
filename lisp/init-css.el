;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :ensure t
  :hook (css-mode html-mode sass-mode))


;;; Embedding in html
(use-package mmm-mode
  :ensure t
  :config
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))


;;; SASS and SCSS
(use-package sass-mode
  :ensure t)

(unless (fboundp 'scss-mode)
  ;; Prefer the scss-mode built into Emacs
  (use-package scss-mode
    :ensure t))

(setq-default scss-compile-at-save nil)


;;; LESS
(unless (fboundp 'less-css-mode)
  ;; Prefer the scss-mode built into Emacs
  (use-package less-css-mode
    :ensure t))


;;; Use eldoc for syntax hints
(use-package css-eldoc
  :ensure t
  :hook (css-mode . turn-on-css-eldoc))

(provide 'init-css)
;;; init-css.el ends here
