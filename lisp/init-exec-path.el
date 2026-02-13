;;; init-exec-path.el --- init-exec-path.el

;;; Commentary:
;; 

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :custom
  (exec-path-from-shell-variables
   '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" 
     "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  :config
  (exec-path-from-shell-initialize))


(provide 'init-exec-path)

;;; init-exec-path.el ends here
