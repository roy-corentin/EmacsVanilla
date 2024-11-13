;;; cr-methods.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

(defun cr/vterm--configure-in-project-root (arg display-fn)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (let ((default-directory (if (or arg (not (project-current))) default-directory (project-root (project-current)))))
    (setenv "PROOT" default-directory)
    (funcall display-fn)))

(defun cr/toggle-vterm-popup (arg)
  (interactive "P")
  (cr/vterm--configure-in-project-root
   arg
   (lambda ()
     (let ((buffer-name (format "*vterm-popup-%s*" (if (project-current) (project-name (project-current)) "main")))
           confirm-kill-processes)
       (let ((buffer (get-buffer buffer-name)))
         (if (buffer-live-p buffer)
             (kill-buffer buffer)
           (vterm buffer-name)))))))

(defun cr/vterm-buffer (arg)
  (interactive "P")
  (cr/vterm--configure-in-project-root
   arg
   (lambda ()
     (let ((buffer-name (format "*vterm-%s*" (if (project-current) (project-name (project-current)) "main"))))
       (vterm buffer-name)))))

(defun cr/smart-vterm-buffer (arg)
  (interactive "P")
  (when (one-window-p)
    (let ((split-width-threshold 130))
      (let ((new-window (split-window-sensibly)))
        (select-window new-window))))
  (cr/vterm-buffer arg))

(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
         (if arg
             (read-directory-name "Search directory: ")
           default-directory)))
    (consult-ripgrep default-directory)))

(defun cr/find-file-in-dir(dir)
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file)))

(defun cr/find-config-file ()
  (interactive)
  (cr/find-file-in-dir (concat user-emacs-directory "config/")))

(defun cr/find-note ()
  (interactive)
  (unless(bound-and-true-p org-directory)
    (require 'org))
  (cr/find-file-in-dir org-directory))

(defun cr/switch-project-in-new-tab ()
  "Create a new tab, switch to the project and rename the tab with project name"
  (interactive)
  (tab-new)
  (call-interactively #'project-switch-project)
  (tab-rename (project-name (project-current))))


(defun cr/split-window-right-and-follow ()
  "Split current window in the right and focus the new window"
  (interactive)
  (let ((new-window (split-window-right)))
    (select-window new-window)))

(defun cr/split-window-below-and-follow ()
  "Split current window below and focus the new window"
  (interactive)
  (let ((new-window (split-window-below)))
    (select-window new-window)))

(defun cr/move-window-right ()
  "Move the current window to the right"
  (interactive)
  (condition-case nil
      (windmove-swap-states-right)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (cr/split-window-right-and-follow)
             (switch-to-buffer buffer-to-swap)))))

(defun cr/move-window-left ()
  "Move the current window to the left"
  (interactive)
  (condition-case nil
      (windmove-swap-states-left)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (split-window-right)
             (switch-to-buffer buffer-to-swap)))))

(defun cr/move-window-down ()
  "Move the current window to the down"
  (interactive)
  (condition-case nil
      (windmove-swap-states-down)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (cr/split-window-below-and-follow)
             (switch-to-buffer buffer-to-swap)))))

(defun cr/move-window-up ()
  "Move the current window to the up"
  (interactive)
  (condition-case nil
      (windmove-swap-states-up)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (split-window-below)
             (switch-to-buffer buffer-to-swap)))))

(defun cr/try-kill-project-buffers (&rest args)
  (when (project-current)
    (project-kill-buffers)))
(advice-add #'tab-close :before #'cr/try-kill-project-buffers)
(advice-add #'tab-switcher-execute :before #'cr/try-kill-project-buffers)

(defun cr/project-open-file-other-window (&rest args)
  (interactive)
  (other-window-prefix)
  (project-find-file args))

(defun cr/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries of a TODO are done, to TODO otherwise."
  (let ((todo-state (org-get-todo-state)))
    (when (member todo-state org-todo-keywords-1)
      (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

(provide 'cr-methods)
