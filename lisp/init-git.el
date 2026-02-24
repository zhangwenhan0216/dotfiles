;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package git-modes
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package git-link
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-visit-prefer-worktree t)
  :bind (("M-<f12>" . magit-status)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  (sanityinc/fullframe-mode 'magit-status-mode)
  :config
  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file))
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

(use-package magit-todos
  :ensure t
  :after magit)

(add-hook 'git-commit-mode-hook 'goto-address-mode)

(use-package vc
  :ensure nil
  :config
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

(use-package compile
  :ensure nil
  :config
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))


(provide 'init-git)
;;; init-git.el ends here
