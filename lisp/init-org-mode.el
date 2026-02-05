;;; init-org-roam --- org roam configurations
;;; Commentary:
;;; Code:

;;(use-package org-roam
;;  :ensure t
;;  :bind (("C-c n l" . org-roam-buffer-toggle)
;;         ("C-c n f" . org-roam-node-find)
;;         ("C-c n i" . org-roam-node-insert))
;;  :config
;;  (setq org-roam-database-connector 'sqlite-builtin)
;;  (org-roam-db-autosync-mode))


(setq org-log-done 'note)  ;; 关闭时记录时间戳 + 备注提示

(provide 'init-org-mode)

;;; init-org-roam.el ends here
