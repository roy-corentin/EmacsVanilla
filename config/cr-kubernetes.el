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
  :init
  (fset 'k8s 'kubernetes-overview))

(provide 'cr-kubernetes)
;;; cr-kubernetes.el ends here
