;;; cr-kubernetes.el --- kubernetes.el configuration  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Corentin ROY
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :init
  (fset 'k8s 'kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :demand t
  :after (kubernetes evil))

(provide 'cr-kubernetes)
;;; cr-kubernetes.el ends here
