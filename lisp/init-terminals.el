;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eat
  :ensure t
  :hook (eat-exit . sanityinc/on-eat-exit)   ; 进程退出时自动清理
  :init
  ;; 定义进程退出处理函数
  (defun sanityinc/on-eat-exit (process)
    (when (zerop (process-exit-status process))
      (kill-buffer)
      (unless (eq (selected-window) (next-window))
        (delete-window))))

  ;; 定义智能 TERM 名称生成函数
  (defun sanityinc/eat-term-get-suitable-term-name (&optional display)
    "Return a suitable TERM value based on DISPLAY's color capacity."
    (let ((colors (display-color-cells display)))
      (cond ((> colors 8) "xterm-256color")
            ((> colors 1) "xterm-color")
            (t "xterm"))))

  ;; 定义前缀命令键映射
  (defcustom sanityinc/eat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "t") 'eat-other-window)
      map)
    "Prefix map for commands that create and manipulate eat buffers."
    :group 'eat)

  (fset 'sanityinc/eat-map sanityinc/eat-map)   ; 使键盘映射可被用作命令

  :custom
  (eat-term-scrollback-size (* 2 1024 1024))    ; 2 MiB 滚动缓冲区
  (eat-term-name 'sanityinc/eat-term-get-suitable-term-name) ; 动态 TERM

  :config
  ;; 调整半字符模式下的非绑定键，让 ESC w 被 Emacs 捕获
  (setq eat-semi-char-non-bound-keys
        (cl-remove [?\e ?w] eat-semi-char-non-bound-keys :test 'equal))

  :bind ("C-c t" . sanityinc/eat-map))          ; 全局前缀激活键

(provide 'init-terminals)
;;; init-terminals.el ends here
