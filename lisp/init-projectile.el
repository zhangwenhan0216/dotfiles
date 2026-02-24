;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; 常用快捷键（按 C-c p 后）
;; 按键	功能
;; f	查找项目中的文件
;; g	在当前项目中使用 rg/grep 搜索
;; b	切换项目中的缓冲区
;; p	切换项目
;; c	编译项目
;; t	运行项目测试
;; r	替换项目中的字符串
;; i	清除项目缓存（文件列表）
;;


(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :custom
  (projectile-mode-line-prefix " Proj")
  :config
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden -0"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ibuffer-projectile
  :ensure t
  :after projectile)

(provide 'init-projectile)
;;; init-projectile.el ends here
