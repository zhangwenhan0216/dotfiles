;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :ensure t
  :defer t
  :bind (:map envrc-mode-map
              ("C-c e" . envrc-command-map))
  :hook (after-init . envrc-global-mode)
  :config
  ;; 自定义选项
  (setq envrc-show-summary-in-minibuffer t)) ; 显示加载摘要


(provide 'init-direnv)

;;; init-direnv.el ends here
