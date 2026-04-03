;;; cr-kubernetes.el --- kubernetes.el configuration  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026  Corentin ROY
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;;; Commentary:


;;; Code:

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
  :init
  (fset 'k8s 'kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after (evil kubernetes))

(provide 'cr-kubernetes)
;;; cr-kubernetes.el ends here
