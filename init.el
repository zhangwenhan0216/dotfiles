;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Cabins
;; Github: https://github.com/cabins-emacs.d
;;; Commentary: (c) Cabins Kong, 2022-Present
;;; Code:

;; variables definition
(defvar cabins-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins-os-mac (eq system-type 'darwin))

;; 安装字体： 安装scoop 1. Get-Command scoop -ErrorAction SilentlyContinue 2. Set-ExecutionPolicy RemoteSigned -Scope CurrentUser 3. irm get.scoop.sh | iex
;; 开始安装字体 1. scoop bucket add nerd-fonts   2. scoop install Maple-Mono-NF-CN
;; font settings
(defvar font-name "Maple Mono NF CN")
(when (find-font (font-spec :family font-name))
  (set-face-attribute 'default nil :family font-name))

;; pre-settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; customized functions
(require 'init-functions)

;; builtin settings
(require 'init-builtins)

;; third-part packages
(require 'init-third-packages)

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
