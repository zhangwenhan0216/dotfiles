;;; init-docker.el --- Work with Docker and its tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t
  :config
  (sanityinc/fullframe-mode 'docker-image-mode)
  (sanityinc/fullframe-mode 'docker-machine-mode)
  (sanityinc/fullframe-mode 'docker-volume-mode)
  (sanityinc/fullframe-mode 'docker-network-mode)
  (sanityinc/fullframe-mode 'docker-container-mode))

(use-package dockerfile-mode
  :ensure t)
(use-package docker-compose-mode
  :ensure t)

(provide 'init-docker)
;;; init-docker.el ends here
