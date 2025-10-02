;;; cr-gnuplot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>

;; Configure gnuplot packages

;;; Commentary:


;;; Code:

(use-package gnuplot
  :ensure t
  :defer t)

(use-package gnuplot-mode
  :ensure t
  :defer t)

(provide 'cr-gnuplot)
;;; cr-gnuplot.el ends here
