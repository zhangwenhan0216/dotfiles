;;; init-gui-frames.el --- init-gui-frames.el

;;; Commentary:
;; 

;;; Code:

(defun wh/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'wh/maybe-suspend-frame)

;; 禁用 GUI 干扰元素
(setq use-file-dialog nil)  ; 不用图形文件对话框，用 minibuffer
(setq use-dialog-box nil)   ; 不用图形确认框，用 minibuffer
(setq inhibit-startup-screen t) ; 禁用启动画面
(setq initial-scratch-message nil) 

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(setq display-line-numbers-type 'relative)   ; （可选）显示相对行号

(ido-mode t)
(electric-pair-mode t)                       ; 自动补全括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer

  ;; 移除窗口边框
  (setq default-frame-alist
        (append '((internal-border-width . 0)
                  (border-width . 0)
                  (vertical-scroll-bars . nil))
                default-frame-alist))
  (setq initial-frame-alist
        (append '((internal-border-width . 0)
                  (border-width . 0)
                  (vertical-scroll-bars . nil))
                initial-frame-alist))

  ;; 对所有现有和新创建的 frame 去除边框
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (modify-frame-parameters
               frame
               '((internal-border-width . 0)
                 (border-width . 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily

(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))


(provide 'init-gui-frames)

;;; init-gui-frames.el ends here
