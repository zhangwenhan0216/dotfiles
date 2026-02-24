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

(require 'init-utils)
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

(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)
(require 'init-recentf)       ;
(require 'init-minibuffer)    ;
(require 'init-rime)          ; 输入法配置
(require 'init-hippie-expand) ;
(require 'init-corfu)         ; 自动补全
(require 'init-windows)       ; 窗口切换
(require 'init-sessions)      ; 保存当前会话
(require 'init-mmm)           ; 在不同的区域使用代码高亮功能
(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-vc)            ; 版本控制
(require 'init-git)

(require 'init-projectile)
(require 'init-compile)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-javascript)
(require 'init-org)
(require 'init-html)
(require 'init-css)
(require 'init-yaml)

(use-package nginx-mode
  :ensure t)

(require 'init-docker)

;; rust
(require 'init-rust)

(require 'init-paredit)
(require 'init-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)
(require 'init-folding)
(require 'init-terminals)

(use-package sudo-edit
  :ensure t)
(use-package dotenv-mode
  :ensure t)
(use-package shfmt
  :ensure t)

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-locales)


(provide 'init)

;;; init.el ends here
