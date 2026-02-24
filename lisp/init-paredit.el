;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :ensure t
  :hook (paredit-mode . sanityinc/maybe-map-paredit-newline)
  :config
  (diminish 'paredit-mode " Par")
  (dolist (binding '("RET" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp-killing-backward))

(defun sanityinc/maybe-map-paredit-newline ()
  "Map RET to paredit-newline in appropriate contexts."
  (unless (or (derived-mode-p 'inferior-emacs-lisp-mode 'cider-repl-mode)
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

;; Minibuffer support
(defvar paredit-minibuffer-commands '(eval-expression
                                     pp-eval-expression
                                     eval-expression-with-eldoc
                                     ibuffer-do-eval
                                     ibuffer-do-view-and-eval))

(defun sanityinc/conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (when (memq this-command paredit-minibuffer-commands)
    (enable-paredit-mode)))

(add-hook 'minibuffer-setup-hook 'sanityinc/conditionally-enable-paredit-mode)
(add-hook 'sanityinc/lispy-modes-hook 'enable-paredit-mode)

;; Puni as alternative
(use-package puni
  :ensure t
  :hook (sanityinc/lispy-modes . (lambda () (puni-mode -1)))
  :config
  (define-key puni-mode-map (kbd "M-(") 'puni-wrap-round)
  (define-key puni-mode-map (kbd "C-(") 'puni-slurp-backward)
  (define-key puni-mode-map (kbd "C-)") 'puni-slurp-forward)
  (define-key puni-mode-map (kbd "C-}") 'puni-barf-forward)
  (define-key puni-mode-map (kbd "C-{") 'puni-barf-backward)
  (define-key puni-mode-map (kbd "M-<up>") 'puni-splice-killing-backward)
  (define-key puni-mode-map (kbd "C-w") nil))

(provide 'init-paredit)
;;; init-paredit.el ends here
