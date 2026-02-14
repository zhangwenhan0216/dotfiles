;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :config
  (setq-default eglot-extend-to-xref t)
  (setq eglot-code-action-indications '(eldoc-hint mode-line)))

(use-package consult-eglot
  :ensure t
  :after eglot)


(provide 'init-eglot)

;;; init-eglot.el ends here
