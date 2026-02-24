;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :ensure t)
(use-package js2-mode
  :ensure t)
(use-package typescript-mode
  :ensure t)
(use-package prettier-js
  :ensure t)


;;; Basic js-mode setup

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(use-package js
  :ensure nil
  :custom (js-indent-level 2)
  :init (require 'derived)  ; 在 :config 前加载
  :config
  (sanityinc/major-mode-lighter 'js-mode "JS")
  (sanityinc/major-mode-lighter 'js-jsx-mode "JSX"))


;; js2-mode

(use-package js2-mode
  :ensure t
  :init (require 'derived)  ; 在 :config 前加载
  :custom
  (js2-bounce-indent-p nil)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  :config
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    "Enable js2 checks if flycheck is not active."
    (unless (and (fboundp 'flycheck-get-checker-for-buffer)
                 (flycheck-get-checker-for-buffer))
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))

  (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

  (js2-imenu-extras-setup)
  (sanityinc/major-mode-lighter 'js2-mode "JS2")
  (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))


;; xref-js2 for code navigation

(use-package xref-js2
  :ensure t
  :when (executable-find "rg")
  :custom (xref-js2-search-program 'rg)
  :init (require 'derived)  ; 在 :config 前加载
  :config
  (defun sanityinc/enable-xref-js2 ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

  (let ((base-mode (if (fboundp 'js-base-mode) 'js-base-mode 'js-mode)))
    (with-eval-after-load 'js
      (add-hook (derived-mode-hook-name base-mode) 'sanityinc/enable-xref-js2)
      (define-key js-mode-map (kbd "M-.") nil)
      (when (boundp 'js-ts-mode-map)
        (define-key js-ts-mode-map (kbd "M-.") nil))))
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)))


;; Run and interact with an inferior JS via js-comint.el

(use-package js-comint
  :ensure t
  :custom (js-comint-program-command "node")
  :config
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    :init-value nil :lighter " InfJS" :keymap inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

(provide 'init-javascript)
;;; init-javascript.el ends here
