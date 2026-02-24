;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Misc config - yet to be placed in separate files

(add-auto-mode 'tcl-mode "^Portfile\\'")

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'conf-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

;; info-colors 包配置
(use-package info-colors
  :ensure t
  :if (require 'info-colors nil t)   ; 若包存在则加载
  :hook (Info-selection . info-colors-fontify-node))

;; regex-tool 包配置
(use-package regex-tool
  :ensure t
  :if (require 'regex-tool nil t)
  :custom (regex-tool-backend 'perl))

;; re-builder 配置（内置包）
(use-package re-builder
  :ensure nil
  :bind (:map reb-mode-map ("C-c C-k" . reb-quit)))

(provide 'init-misc)
;;; init-misc.el ends here
