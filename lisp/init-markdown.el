;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\.html\\'" . markdown-mode)
  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
