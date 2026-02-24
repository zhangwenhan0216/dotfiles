;;; init-crontab.el --- Working with crontabs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package crontab-mode
  :ensure t
  :mode ("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(provide 'init-crontab)
;;; init-crontab.el ends here
