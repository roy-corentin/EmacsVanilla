(defun cr/vterm-in-project (arg)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (interactive "P")
  (let ((default-directory (if arg default-directory (project-root (project-current t)))))
    (setenv "PROOT" default-directory)
    (vterm vterm-buffer-name)))

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
    (select-window new-window))
  )

(defun cr/split-window-below-and-follow ()
  "Split current window below and focus the new window"
  (interactive)
  (let ((new-window (split-window-below)))
    (select-window new-window))
  )

(defun cr/olivetti-on-single-prog-window ()
  (when (and (one-window-p) (derived-mode-p 'prog-mode))
    (olivetti-mode)))

(provide 'cr-methods)
