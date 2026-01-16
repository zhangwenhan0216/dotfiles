;;; init-functions.el --- provides some useful functions.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun efs/display-startup-time ()
  "Statistic for the startup time."

  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defun dark-theme ()
  "Activate dark theme."

  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'modus-vivendi t))

(defun light-theme ()
  "Activate light theme."

  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'modus-operandi t))

;; 复制当前行
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-c ,") 'duplicate-line) ;; 复制整行
(global-set-key (kbd "C-c d") 'kill-whole-line) ;; 删除整行

;;;###autoload
(defun preferences()
  "Nothing, but alias like `crux-find-user-init-file', inspired by VSCode."

  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-,") 'preferences)

;;;###autoload
(defun preference-custom()
  "Nothing, but alias like `crux-find-user-custom-file', inspired by VSCode."

  (interactive)
  (find-file custom-file))

;;;###autoload
(defun input-chinese-methods()
  "Enable the Chinese input methods"

  (interactive)
  (require 'init-input-methods)
  (toggle-input-method))

;;;###autoload
(defun treesit-install-language-grammar-all()
  "Install all treesit language grammar"

  (interactive)
  (dolist (lang '(bash c go gomod html java javascript json markdown python rust typescript yaml))
    (treesit-install-language-grammar lang)))

;; programming language hooks
(defun my/prog-mode-common-setup ()
  "通用编程模式设置，包括显示列号、行号和启用次模式等."
  (setq-local column-number-mode t)
  (display-line-numbers-mode 1)
  (electric-pair-mode 1)
  (hs-minor-mode 1)
  (prettify-symbols-mode 1)
  (which-function-mode 1))

;; 复制整个单词
(defun copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-word 1)
    (backward-word 1)
    (let ((b (point)))
      (forward-word 1)
      (copy-region-as-kill b (point)))
    (message "Word copied!")))
(global-set-key (kbd "C-c w") 'copy-whole-word)

;; 快速打开配置文件目录
(defun open-conf-dir ()
  "直接打开 Emacs 配置目录。"
  (interactive)
  (dired user-emacs-directory))

(provide 'init-functions)

;;; init-functions.el ends here
