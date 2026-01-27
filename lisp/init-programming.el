;;; init-programming.el --- configurations for Programmers
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; Rust Programming ;;
;;;;;;;;;;;;;;;;;;;;;;
;; 现代 Rust 开发配置（推荐）
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :config
  ;; 优化性能
  (setq lsp-eldoc-render-all nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-idle-delay 0.5))

;; 可选：更好的 UI
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(provide 'init-programming)

;;; init-programming.el ends here
