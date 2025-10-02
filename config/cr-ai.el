;;; cr-ai.el --- AI packages configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roy Corentin

;; Author: Roy Corentin <croy@motherbase-xps139340>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package gptel
  :ensure t
  :hook (gptel-post-response-functions . gptel-end-of-response)
  :custom
  (gptel-api-key #'cr/gptel-openai-api-key)
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'cr/gptel-anthropic-api-key)
  (gptel-make-gemini "Gemini" :stream t :key #'cr/gptel-gemini-api-key)
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_buffer"        ; javascript-style snake_case name
                :function (lambda (buffer) ; the function that will run
                            (unless (buffer-live-p (get-buffer buffer))
                              (error "error: BUFFER %s is not live" buffer))
                            (with-current-buffer  buffer
                              (buffer-substring-no-properties (point-min) (point-max))))
                :description "return the contents of an emacs buffer"
                :args (list '(:name "buffer"
                                    :type string ; :type value must be a symbol
                                    :description "the name of the buffer whose contents are to be retrieved"))
                :category "emacs")) ; An arbitrary label for grouping
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "append_to_buffer"
                :function (lambda (buffer text)
                            (with-current-buffer (get-buffer-create buffer)
                              (save-excursion
                                (goto-char (point-max))
                                (insert text)))
                            (format "Appended text to buffer %s" buffer))
                :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
                :args (list '(:name "buffer"
                                    :type string
                                    :description "The name of the buffer to append text to.")
                            '(:name "text"
                                    :type string
                                    :description "The text to append to the buffer."))
                :category "emacs"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "edit_buffer"
                :function #'my-gptel--edit-buffer
                :description "Edits Emacs buffers"
                :args (list '(:name "buffer_name"
                                    :type string
                                    :description "Name of the buffer to modify"
                                    :required t)
                            '(:name "old_string"
                                    :type string
                                    :description "Text to replace (must match exactly)"
                                    :required t)
                            '(:name "new_string"
                                    :type string
                                    :description "Text to replace old_string with"
                                    :required t))
                :category "edit"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "replace_buffer"
                :function #'my-gptel--replace-buffer
                :description "Completely overwrites buffer contents"
                :args (list '(:name "buffer_name"
                                    :type string
                                    :description "Name of the buffer to overwrite"
                                    :required t)
                            '(:name "content"
                                    :type string
                                    :description "Content to write to the buffer"
                                    :required t))
                :category "edit"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_file"
                :function (lambda (filepath)
                            (with-temp-buffer
                              (insert-file-contents (expand-file-name filepath))
                              (buffer-string)))
                :description "Read and display the contents of a file"
                :args (list '(:name "filepath"
                                    :type string
                                    :description "Path to the file to read. Supports relative paths and ~."))
                :category "filesystem"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "create_file"                       ; javascript-style  snake_case name
                :function (lambda (path filename content) ; the function that runs
                            (let ((full-path (expand-file-name filename path)))
                              (with-temp-buffer
                                (insert content)
                                (write-file full-path))
                              (format "Created file %s in %s" filename path)))
                :description "Create a new file with the specified content"
                :args (list '(:name "path" ; a list of argument specifications
	                            :type string
	                            :description "The directory where to create the file")
                            '(:name "filename"
	                            :type string
	                            :description "The name of the file to create")
                            '(:name "content"
	                            :type string
	                            :description "The content to write to the file"))
                :category "filesystem")) ; An arbitrary label for grouping
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "list_directory"
                :function (lambda (directory)
                            (mapconcat #'identity
                                       (directory-files directory)
                                       "\n"))
                :description "List the contents of a given directory"
                :args (list '(:name "directory"
                                    :type string
                                    :description "The path to the directory to list"))
                :category "filesystem"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "edit_file"
                :function #'my-gptel--edit_file
                :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
                :args (list '(:name "file-path"
                                    :type string
                                    :description "The full path of the file to edit")
                            '(:name "file-edits"
                                    :type array
                                    :items (:type object
                                                  :properties
                                                  (:line_number
                                                   (:type integer :description "The line number of the file where edit starts.")
                                                   :old_string
                                                   (:type string :description "The old-string to be replaced.")
                                                   :new_string
                                                   (:type string :description "The new-string to replace old-string.")))
                                    :description "The list of edits to apply on the file"))
                :category "filesystem"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "run_command"
                :function (lambda (command &optional working_dir)
                            (with-temp-message (format "Executing command: `%s`" command)
                              (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                                           (expand-file-name working_dir)
                                                         default-directory)))
                                (shell-command-to-string command))))
                :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
                :args (list
                       '(:name "command"
                               :type string
                               :description "The complete shell command to execute.")
                       '(:name "working_dir"
                               :type string
                               :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
                :category "command"
                :confirm t
                :include t))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "run_async_command"
                :function #'my-gptel--run-async-command
                :description "Run an async command."
                :args (list
                       '(:name "command"
                               :type "string"
                               :description "Command to run."))
                :category "command"
                :async t
                :include t))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "echo_message"
                :function (lambda (text)
                            (message "%s" text)
                            (format "Message sent: %s" text))
                :description "Send a message to the *Messages* buffer"
                :args (list '(:name "text"
                                    :type string
                                    :description "The text to send to the messages buffer"))
                :category "emacs"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_documentation"
                :function #'my-gptel--read-documentation
                :description "Read the documentation for a given function or variable"
                :args (list '(:name "name"
                                    :type string
                                    :description "The name of the function or variable whose documentation is to be retrieved"))
                :category "emacs"))
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_url"
                :function (lambda (url)
                            (with-current-buffer (url-retrieve-synchronously url)
                              (goto-char (point-min))
                              (forward-paragraph)
                              (let ((dom (libxml-parse-html-region (point) (point-max))))
                                (run-at-time 0 nil #'kill-buffer (current-buffer))
                                (with-temp-buffer
                                  (shr-insert-document dom)
                                  (buffer-substring-no-properties (point-min) (point-max))))))
                :description "Fetch and read the contents of a URL"
                :args (list '(:name "url"
                                    :type string
                                    :description "The URL to read"))
                :category "web"))
  )

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick"))

(use-package org-ai
  :ensure t
  :defer t
  :commands (org-ai-mode
             org-ai-global-mode)
  :hook org-mode
  :custom
  (org-ai-default-chat-model "gpt-4")
  :init
  (org-ai-global-mode)
  :config
  (org-ai-install-yasnippets))

(provide 'cr-ai)
;;; cr-ai.el ends here
