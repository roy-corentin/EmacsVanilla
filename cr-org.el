;;; cr-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 10, 2024

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Dropbox/Org/"
        org-agenda-files (list org-directory)
        org-log-done 'time
        org-log-into-drawer t
        org-startup-indented t
        org-image-actual-width nil
        org-startup-with-inline-images t
        org-startup-with-latex-preview t))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/RoamNotes")
  (org-roam-index-file "~/Dropbox/RoamNotes/index.org")
  (org-roam-capture-templates
   `(("d" " Default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" " Problems" plain
      "* [[id:f23824a1-0515-47c6-b386-21d83a9aec21][PROBLEM]]\n%?\n* SOLVING"
      :target (file+head "problems/content/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :PROBLEM:\n")
      :unnarrowed t)
     ("a", "󰙅 DataStructure" plain
      "A =${title}= [[id:92421051-83c3-4117-9c25-7f4f9ecf2c0a][Data Structure]] is %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :DATASTRUCTURE:\n")
      :unnarrowed t))))

(provide 'cr-org)
;;; cr-org.el ends here
