;;; init-builtins --- settings for builtins  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; os & coding settings
(with-eval-after-load 'w32-win
  (when cabins-os-win
    (setq w32-get-true-file-attributes nil
          w32-pipe-read-delay 0
          w32-pipe-buffer-size (* 64 1024))))

(when cabins-os-mac
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	ns-use-native-fullscreen t))

;; solve the Chinese paste issue
;; let Emacs auto-guess the selection coding according to the Windows/system settings
(prefer-coding-system 'utf-8)
(unless cabins-os-win
  (set-selection-coding-system 'utf-8))

;; 基础包管理器初始化
(require 'package)
;; core package settings
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
;; 确保 use-package 已安装 (Emacs 29 以下需要此步)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 加载 use-package
(require 'use-package)

;; the basic usage for `use-package'
;; (use-package package-name
;;   :ensure t       ; package will be installed automatically, if the package is not installed
;;   :init (code)    ; execute before package loading
;;   :config (code)  ; execute after package loading
;;   :bind (keybindings)
;;   :hook (hooks)
;;   :defer t        ; defer loading, until needed (speep up)
;; )

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t)

;; Emacs builtin packages
(setq-default auto-window-vscroll nil
	      default-directory "~"
	      default-text-properties '(line-spacing 0.2 line-height 1.2) ;default line height
	      frame-title-format "%b"
	      help-window-select t
	      initial-major-mode 'fundamental-mode
	      inhibit-startup-screen t ; disable the startup screen splash
	      kill-whole-line t
	      mode-line-compact t
	      make-backup-files nil	; disable backup file
	      read-process-output-max (* 4 1024 1024)
	      require-final-newline t
	      scroll-conservatively 1000
	      show-trailing-whitespace t
	      system-time-locale "C"
	      use-short-answers t)
;; ibuffer
(defalias 'list-buffers 'ibuffer)

(provide 'init-builtins)

;;; init-builtins.el ends here
