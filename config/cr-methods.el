;;; cr-methods.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Corentin Roy
;;
;; Author: Corentin Roy <corentin.roy02@laposte.net>
;; Maintainer: Corentin Roy <corentin.roy02@laposte.net>
;; Created: avril 13, 2024

;;; Commentary:

;;; Code:

(defun cr/vterm--configure-in-project-root (arg display-fn)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer called with DISPLAY-FN."
  (let ((default-directory (if (or arg (not (project-current))) default-directory (project-root (project-current)))))
    (setenv "PROOT" default-directory)
    (funcall display-fn)))

;;;###autoload
(defun cr/toggle-vterm-popup (arg)
  "Toggle vterm in a popup.  Use ARG."
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

;;;###autoload
(defun cr/vterm-buffer (arg)
  "Open a vterm buffer in new buffer.  Use ARG."
  (interactive "P")
  (cr/vterm--configure-in-project-root
   arg
   (lambda ()
     (let ((buffer-name (format "*vterm-%s*" (if (project-current) (project-name (project-current)) "main"))))
       (vterm buffer-name)))))

;;;###autoload
(defun cr/smart-vterm-buffer (arg)
  "Open a vterm buffer in another window if there is only one.  Use ARG."
  (interactive "P")
  (when (cr/one-main-window-p)
    (let ((split-width-threshold 115))
      (let ((new-window (split-window-sensibly)))
        (select-window new-window))))
  (cr/vterm-buffer arg))

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
         (if arg
             (read-directory-name "Search directory: ")
           default-directory)))
    (consult-ripgrep default-directory)))

;;;###autoload
(defun cr/find-file-in-dir(dir)
  "Find file in DIR."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file)))

;;;###autoload
(defun cr/find-config-file ()
  "Find config file."
  (interactive)
  (cr/find-file-in-dir (concat user-emacs-directory "config/")))

;;;###autoload
(defun cr/find-note ()
  "Find note."
  (interactive)
  (unless(bound-and-true-p org-directory)
    (require 'org))
  (cr/find-file-in-dir org-directory))

;;;###autoload
(defun cr/switch-project-in-new-tab ()
  "Create a new tab, switch to project and rename the tab with project name."
  (interactive)
  (tab-new)
  (call-interactively #'project-switch-project)
  (tab-rename (project-name (project-current))))

;;;###autoload
(defun cr/split-window-right-and-follow ()
  "Split current window in the right and focus the new window."
  (interactive)
  (let ((new-window (split-window-right)))
    (select-window new-window)))

;;;###autoload
(defun cr/split-window-below-and-follow ()
  "Split current window below and focus the new window."
  (interactive)
  (let ((new-window (split-window-below)))
    (select-window new-window)))

;;;###autoload
(defun cr/move-window-right ()
  "Move the current window to the right."
  (interactive)
  (condition-case nil
      (windmove-swap-states-right)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (cr/split-window-right-and-follow)
             (switch-to-buffer buffer-to-swap)))))

;;;###autoload
(defun cr/move-window-left ()
  "Move the current window to the left."
  (interactive)
  (condition-case nil
      (windmove-swap-states-left)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (split-window-right)
             (switch-to-buffer buffer-to-swap)))))

;;;###autoload
(defun cr/move-window-down ()
  "Move the current window to the down."
  (interactive)
  (condition-case nil
      (windmove-swap-states-down)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (cr/split-window-below-and-follow)
             (switch-to-buffer buffer-to-swap)))))

;;;###autoload
(defun cr/move-window-up ()
  "Move the current window to the up."
  (interactive)
  (condition-case nil
      (windmove-swap-states-up)
    (error (let ((buffer-to-swap (current-buffer)))
             (delete-window)
             (split-window-below)
             (switch-to-buffer buffer-to-swap)))))

;;;###autoload
(defun cr/try-kill-project-buffers (&rest args)
  "Kill all project buffers if in project.  ARGS optional."
  (ignore args)
  (when (project-current)
    (project-kill-buffers)))

(advice-add #'tab-close :before #'cr/try-kill-project-buffers)
(advice-add #'tab-switcher-execute :before #'cr/try-kill-project-buffers)

;;;###autoload
(defun cr/project-open-file-other-window (&rest args)
  "Open project file in other window.  ARGS optional."
  (interactive)
  (other-window-prefix)
  (project-find-file args))

;;;###autoload
(defun cr/org-summary-todo (_n-done n-not-done)
  "Switch entry to done when all subentries (if N-NOT-DONE is zero) of a todo are done, to todo otherwise."
  (let ((todo-state (org-get-todo-state)))
    (when (member todo-state org-todo-keywords-1)
      (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

(with-eval-after-load 'evil-search
  (defun cr/search-symbol-at-point-in-project ()
    "Search in project symbol at point"
    (interactive)
    (consult-ripgrep nil (evil-find-thing t 'symbol))))

;;;###autoload
(defun cr/switch-theme (theme)
  "Switch to new THEME and disable previous."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (when (load-theme theme t)
      (disable-theme current-theme)
      (setq emacs-theme theme))))

;;;###autoload
(defun cr/project-buffer-dwim ()
  "Switch to buffer in project or all buffer."
  (interactive)
  (if (project-current nil)
      (consult-project-buffer)
    (consult-buffer)))

;;;###autoload
(defun cr/find-file-dwim ()
  "Find file in project or all buffer."
  (interactive)
  (if (project-current nil)
      (project-find-file)
    (consult-buffer)))

;;;###autoload
(defun cr/reload-theme ()
  "Reload current theme."
  (interactive)
  (disable-theme emacs-theme)
  (load-theme emacs-theme t)
  (posframe-delete-all)
  (message "Theme reloaded"))

;;;###autoload
(defun cr/comment-line (n)
  "Comment as I want (Do What I Mean).
If the region is active call `comment-or-uncomment-region'
Otherwise use line positions as range to call `comment-or-uncomment-region'.
If a prefix N is given, it is passed on to the respective function."
  (interactive "*P")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) n)
    (let ((range
           (list (line-beginning-position)
                 (line-end-position n))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (back-to-indentation)))

;;;###autoload
(defun disable-rainbow-delimiter-mode ()
  "Disable rainbow delimieter mode."
  (rainbow-delimiters-mode -1))

;;;###autoload
(defun cr/vterm-insert-up ()
  "Insert key up in vterm."
  (interactive)
  (vterm-send-key "<up>"))

;;;###autoload
(defun cr/vterm-insert-down ()
  "Insert key down in vterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun cr/one-main-window-p ()
  "Like `one-window-p`, but taking into account side windows like treemacs."
  (let* ((mainwindow (window-main-window))
         (child-count (window-child-count mainwindow)))
    (= 0 child-count)))

;;;###autoload
(defun cr/tabs-next ()
  "Move to next tab."
  (interactive)
  (if (centaur-tabs-mode-on-p)
      (centaur-tabs-forward-tab)
    (next-buffer)))

;;;###autoload
(defun cr/tabs-previous ()
  "Move to previous tab."
  (interactive)
  (if (centaur-tabs-mode-on-p)
      (centaur-tabs-backward-tab)
    (previous-buffer)))

;;;###autoload
(defun cr/eldoc-doc-buffer ()
  "Call right eldoc method."
  (interactive)
  (if eldoc-box-hover-mode
      (funcall #'eldoc-box-help-at-point)
    (call-interactively #'eldoc-doc-buffer)))

;;;###autoload
(defun cr/gptel-api-key ()
  "Fetch api key for gptel."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search :host "api.openai.com" :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `gptel-api-key' found in the auth source")))

;;;###autoload
(defun cr/org-ai-on-current-project ()
  "Use `org-ai-on-project' on current project."
  (interactive)
  (org-ai-on-project (project-root (project-current))))

;;;###autoload
(defun kb/toggle-window-transparency (arg)
  "Toggle the value of `alpha-background'.
Toggles between 100 and 70 by default.  Can choose which value to change
to if called with ARG, or any prefix argument."
  (interactive "P")
  (let ((transparency (pcase arg
                        ((pred numberp) arg)
                        ((pred car) (read-number "Change the transparency to which value (0-100)? "))
                        (_
                         (pcase (frame-parameter nil 'alpha-background)
                           (85 100)
                           (100 85)
                           (t 100))))))
    (set-frame-parameter nil 'alpha-background transparency)))

(provide 'cr-methods)
;;; cr-methods.el ends here
