;;; init-flymake.el --- init-flymake.el

;;; Commentary:
;;; Code:

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start)))

(use-package flymake-flycheck
  :after (flymake flycheck)
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :init
  (with-eval-after-load 'flycheck
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))
  :config
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto))

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t))))


(provide 'init-flymake)

;;; init-flymake.el ends here
