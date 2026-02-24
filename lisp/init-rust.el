;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :config
  (use-package flycheck-rust
    :ensure t
    :hook (flycheck-mode . flycheck-rust-setup)))

(use-package toml-mode
  :ensure t)


(provide 'init-rust)
;;; init-rust.el ends here
