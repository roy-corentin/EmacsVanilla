;;; cr-keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 08, 2024

(use-package general
  :ensure t
  :defer t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer cr/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (cr/leader-keys
   "." '(find-file :which-key "find file")
   "SPC" '(project-find-file :which-key "M-x"))
  (cr/leader-keys
   "b" '(:ignore t :which-key "buffer")
   "b b" '(project-switch-to-buffer :which-key "Switch project buffer")
   "b i" '(ibuffer :which-key "Ibuffer")
   "b B" '(switch-to-buffer :which-key "Switch all buffer"))
  (cr/leader-keys
   "f" '(:ignore t :which-key "file")
   "f f" '(find-file :which-key "Find file")
   "f r" '(recentf-open-files :which-key "Recent files")
   "f s" '(save-buffer :which-key "Save file"))
  (cr/leader-keys
   "w" '(:ignore t :which-key "window")
   "w h" '(evil-window-left :which-key "Move left")
   "w j" '(evil-window-down :which-key "Move down")
   "w k" '(evil-window-up :which-key "Move up")
   "w l" '(evil-window-right :which-key "Move right")
   "w w" '(evil-window-next :which-key "Move to next window")
   "w d" '(evil-window-delete :which-key "Delete window")
   "w s" '(evil-window-split :which-key "Split window")
   "w v" '(evil-window-vsplit :which-key "Vsplit window")
   "w m" '(delete-other-windows :which-key "Maximize window"))
  )


(provide 'cr-keybindings)
;;; cr-keybindings.el ends here
