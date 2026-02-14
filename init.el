;;; init.el --- init.el

;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 启用拼写检查
(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))

;; 调整垃圾回收（GC）阈值以加快启动速度。
(setq gc-cons-threshold (* 128 1024 1024)) ;; 128 MB

;; 程性能调优配置，
(setq read-process-output-max (* 4 1024 1024))    ;; 4 MB
(setq process-adaptive-read-buffering nil)        ;; 禁用自适应缓冲

;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-package)
(require 'init-themes)
(require 'init-exec-path)

;; mode-line
(use-package diminish
  :ensure t)

;; gc 
(use-package gcmh
  :ensure t
  :diminish  ; 自动隐藏
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024)))

(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)

;; ace-window - 快速窗口跳转
;; 当有多个窗口时，按 M-o 后窗口会显示数字编号，按数字键即可跳转
;; x 删除窗口 v 垂直分割窗口 b 水平分割窗口 c 均匀分割窗口，可以是垂直或水平 m 交换窗口 c 复制窗口
(use-package ace-window
  :ensure t
  :config (setq aw-dispatch-always t)
  :bind ("M-o" . ace-window)) ; 将 M-o 绑定为跳转键，比 C-x o 快得多

;; multiple-cursors - 多光标编辑
;; 类似于 VS Code 的多光标功能，可以同时编辑多处相同的内容
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)
(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-rime)


(provide 'init)

;;; init.el ends here
