;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary: (c) Cabins Kong, 2022-Present
;;; Code:

;;; 如果在windows上需要配置环境变量，其实就是配置这个 (expand-file-name "~") 这个表达式的值
;;; 这样magit查找.gitconfig文件就可以找到了
;;; HOME -> C:\Users\zwh

;; variables definition
(defvar cabins-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins-os-mac (eq system-type 'darwin))

;; 安装字体： 安装scoop
;; 1. Get-Command scoop -ErrorAction SilentlyContinue
;; 2. Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
;; 3. irm get.scoop.sh | iex
;; 开始安装字体
;; 1. scoop bucket add nerd-fonts
;; 2. scoop install Maple-Mono-NF-CN
;; font settings
(defvar font-name "Maple Mono NF CN")
(when (find-font (font-spec :family font-name))
  (set-face-attribute 'default nil :family font-name))

;; pre-settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 配置 MSYS2
(when (eq system-type 'windows-nt)
  (let ((msys2-root (getenv "MSYS2_ROOT")))
    (message "配置加载中: %s" msys2-root)
    (when msys2-root
      ;; 设置 Shell
      (setq explicit-shell-file-name (expand-file-name "usr/bin/bash" msys2-root))
      (setq shell-file-name explicit-shell-file-name)
      (setenv "SHELL" shell-file-name)

      ;; 设置 MSYSTEM 环境变量（指定 MinGW64 环境）
      (setenv "MSYSTEM" "MINGW64")

      ;; 动态添加路径到 exec-path
      (add-to-list 'exec-path (expand-file-name "mingw64/bin" msys2-root))
      (add-to-list 'exec-path (expand-file-name "usr/bin" msys2-root))
      ;; 同时更新 PATH 环境变量（供子进程使用）
      (setenv "PATH" (concat (expand-file-name "mingw64/bin" msys2-root) ";"
                             (expand-file-name "usr/bin" msys2-root) ";"
                             (getenv "PATH"))))
    ;; 如果没有设置 MSYS2_ROOT，给出警告
    (unless msys2-root
      (warn "环境变量 MSYS2_ROOT 未设置，MSYS2 集成功能不可用"))))

;; 解决 Windows 换行符问题（Cygwin/MSYS2）
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-file-coding-system 'undecided-unix)
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;; customized functions
(require 'init-functions)

;; builtin settings
(require 'init-builtins)

;; third-part packages
(require 'init-third-packages)

;; org-mode
(require 'init-org-mode)

;; custom file settings
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
