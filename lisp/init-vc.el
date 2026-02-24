;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)
              ("M-C-]" . diff-hl-next-hunk)
              ("M-C-[" . diff-hl-previous-hunk)))

(provide 'init-vc)
;;; init-vc.el ends here
