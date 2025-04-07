;;; -*- lexical-binding: t -*-

;; From Doom
;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (if (fboundp 'embark-dwim)
        (save-selected-window
          (embark-dwim))
      (user-error "Embark not installed, aborting..."))))

;;;###autoload
(with-eval-after-load 'evil
  (defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
    (interactive "P")
    (if (button-at (point))
        (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
                type (org-element-type context)))
        (pcase type
          ((or `citation `citation-reference)
           (org-cite-follow context arg))

          (`headline
           (cond ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-cycle-force-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
                         (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
                         (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
                 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
                 (goto-char (org-element-property :contents-begin context))
                 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage)))
             (if (or (equal (org-element-property :type lineage) "img")
                     (and path (image-supported-file-p path)))
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))
               (org-open-at-point arg))))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (org-toggle-checkbox))

          (`paragraph
           (+org--toggle-inline-images-in-subtree))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil  t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context)))))))))

;;;###autoload
(defun +org/shift-return (&optional arg)
  "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
  (interactive "p")
  (if (org-at-table-p)
      (org-table-copy-down arg)
    (org-return nil arg)))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))


;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))

(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-fold-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;; I use these instead of `org-insert-item' or `org-insert-heading' because they
;; impose bizarre whitespace rules depending on cursor location and many
;; settings. These commands have a much simpler responsibility.
;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(provide 'doom-methods)
