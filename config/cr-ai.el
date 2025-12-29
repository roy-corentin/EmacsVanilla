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
  (gptel-model 'claude-sonnet-4-5-20250929)
  :config
  (gptel-make-gemini "Gemini" :stream t :key #'cr/gptel-gemini-api-key)
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key #'cr/gptel-anthropic-api-key)))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick"))

(use-package gptel-agent
  :ensure t
  :after gptel
  :config (gptel-agent-update))

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
