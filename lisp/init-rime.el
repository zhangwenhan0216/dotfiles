;;; init-rime.el --- init-rime.el

;;; Commentary:
;;; Code: 

;; 安装librime输入法 sudo pacman -S librime
(use-package rime
  :ensure t
  :custom
  (default-input-method "rime"))


(provide 'init-rime)

;;; init-rime.el ends here
