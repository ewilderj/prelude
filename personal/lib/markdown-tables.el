;;; markdown-tables.el --- Table rendering for markdown-overlays -*- lexical-binding: t -*-

;; Author: Edd Wilder-James
;; Description: Adds markdown table rendering to markdown-overlays
;; This can be submitted upstream to xenodium/shell-maker once validated

;;; Commentary:
;;
;; Extends markdown-overlays to render markdown tables with proper column
;; alignment. Uses overlay 'display properties to visually pad cells without
;; modifying buffer content.
;;
;; Before: | Name | Role |
;;         |------|------|
;;         | Alice Smith | Engineer |
;;
;; After:  | Name        | Role     |
;;         |-------------|----------|
;;         | Alice Smith | Engineer |
;;
;; Usage: Simply load this file after markdown-overlays is available.
;; It advises `markdown-overlays-put' to also process tables.

;;; Code:

(require 'cl-lib)

(defgroup markdown-tables nil
  "Markdown table rendering for markdown-overlays."
  :group 'markdown-overlays)

(defcustom markdown-tables-header-face 'bold
  "Face to apply to table header row content."
  :type 'face
  :group 'markdown-tables)

(defcustom markdown-tables-border-face 'font-lock-comment-face
  "Face to apply to table borders (pipes and dashes)."
  :type 'face
  :group 'markdown-tables)

(defcustom markdown-tables-use-unicode-borders t
  "When non-nil, use Unicode box-drawing characters for table borders.
Replaces | with │, dashes with ─, and intersections with ┼."
  :type 'boolean
  :group 'markdown-tables)

(defconst markdown-tables--border-pipe "│"
  "Unicode vertical line for table borders.")

(defconst markdown-tables--border-dash "─"
  "Unicode horizontal line for table borders.")

(defconst markdown-tables--border-cross "┼"
  "Unicode cross for table border intersections.")

(defconst markdown-tables--border-tee-left "├"
  "Unicode left-edge tee for table borders.")

(defconst markdown-tables--border-tee-right "┤"
  "Unicode right-edge tee for table borders.")

(defvar markdown-tables--table-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (not (any "\n")))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a single line of a markdown table.")

(defvar markdown-tables--separator-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (or "-" ":" "|" " "))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a table separator line (e.g., |---|---|).")

(defun markdown-tables--find-tables (&optional avoid-ranges)
  "Find all markdown tables in the buffer.
Returns a list of plists with :start, :end, :header-end, and :rows.
AVOID-RANGES is a list of (start . end) cons to skip."
  (let ((tables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-tables--table-regexp nil t)
        (let ((line-start (line-beginning-position))
              (in-avoided nil))
          ;; Check if we're in an avoided range
          (dolist (range avoid-ranges)
            (when (and (>= line-start (car range))
                       (<= line-start (cdr range)))
              (setq in-avoided t)))
          (unless in-avoided
            ;; Found potential table start, scan for full table
            (goto-char line-start)
            (let ((table-start line-start)
                  (table-end nil)
                  (separator-row nil)
                  (rows '())
                  (row-num 0))
              ;; Collect all consecutive table lines
              (while (and (not (eobp))
                          (looking-at markdown-tables--table-regexp))
                (let ((row-start (point))
                      (row-end (line-end-position))
                      (is-sep (looking-at markdown-tables--separator-regexp)))
                  (when (and (not separator-row) is-sep)
                    (setq separator-row row-num))
                  (push (list :start row-start
                              :end row-end
                              :num row-num
                              :separator is-sep)
                        rows)
                  (setq row-num (1+ row-num))
                  (setq table-end row-end))
                (forward-line 1))
              ;; Only count as table if we have at least 2 rows
              (when (>= (length rows) 2)
                (push (list :start table-start
                            :end table-end
                            :separator-row separator-row
                            :rows (nreverse rows))
                      tables)))))))
    (nreverse tables)))

(defun markdown-tables--parse-row (start end)
  "Parse a table row between START and END into cells.
Returns a list of plists with :start, :end, :content for each cell."
  (let ((cells '()))
    (save-excursion
      (goto-char start)
      ;; Skip leading whitespace and pipe
      (when (looking-at "[ \t]*|")
        (goto-char (match-end 0)))
      (let ((cell-start (point)))
        ;; Find each cell delimited by |
        (while (re-search-forward "|" end t)
          (let ((cell-end (1- (point))))
            (push (list :start cell-start
                        :end cell-end
                        :content (string-trim
                                  (buffer-substring-no-properties cell-start cell-end)))
                  cells)
            (setq cell-start (point))))))
    (nreverse cells)))

(defun markdown-tables--compute-column-widths (table)
  "Compute the maximum width of each column in TABLE.
Returns a list of integers, one per column."
  (let ((widths nil))
    (dolist (row (plist-get table :rows))
      (unless (plist-get row :separator)
        (let ((cells (markdown-tables--parse-row
                      (plist-get row :start)
                      (plist-get row :end)))
              (col 0))
          (dolist (cell cells)
            (let ((content-width (string-width (plist-get cell :content))))
              (if (nth col widths)
                  (setf (nth col widths)
                        (max (nth col widths) content-width))
                ;; Extend widths list
                (setq widths (append widths (list content-width))))
              (setq col (1+ col)))))))
    widths))

(defun markdown-tables--pad-string (str width)
  "Pad STR with spaces to reach WIDTH."
  (let ((current-width (string-width str)))
    (if (>= current-width width)
        str
      (concat str (make-string (- width current-width) ?\s)))))

(defun markdown-tables--make-separator-cell (width)
  "Create a separator cell string of dashes for WIDTH."
  (if markdown-tables-use-unicode-borders
      (make-string width (string-to-char markdown-tables--border-dash))
    (make-string width ?-)))

(defun markdown-tables--align-table (table)
  "Apply display overlays to align TABLE columns."
  (let* ((col-widths (markdown-tables--compute-column-widths table))
         (separator-row (plist-get table :separator-row))
         (rows (plist-get table :rows)))
    
    (dolist (row rows)
      (let* ((row-start (plist-get row :start))
             (row-end (plist-get row :end))
             (row-num (plist-get row :num))
             (is-separator (plist-get row :separator))
             (is-header (and separator-row (< row-num separator-row)))
             (cells (markdown-tables--parse-row row-start row-end))
             (col 0))
        
        (dolist (cell cells)
          (let* ((cell-start (plist-get cell :start))
                 (cell-end (plist-get cell :end))
                 (content (plist-get cell :content))
                 (target-width (or (nth col col-widths) (string-width content)))
                 (padded-content
                  (if is-separator
                      ;; For separator rows, fill entire cell with dashes (no spaces)
                      (markdown-tables--make-separator-cell (+ target-width 2))
                    ;; For content rows, pad with spaces
                    (concat " " (markdown-tables--pad-string content target-width) " ")))
                 (display-str padded-content)
                 (ov (make-overlay cell-start cell-end)))
            
            ;; Apply face to the display string
            (when is-header
              (setq display-str (propertize display-str 'face markdown-tables-header-face)))
            (when is-separator
              (setq display-str (propertize display-str 'face markdown-tables-border-face)))
            
            ;; Use 'display property to show aligned content
            (overlay-put ov 'category 'markdown-overlays)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'display display-str)
            
            (setq col (1+ col))))
        
        ;; Style pipe characters - replace with Unicode if enabled
        ;; Track position to use correct tee/cross characters
        (save-excursion
          (goto-char row-start)
          (let ((pipe-num 0)
                (total-pipes (how-many "|" row-start row-end)))
            (while (re-search-forward "|" row-end t)
              (setq pipe-num (1+ pipe-num))
              (let ((ov (make-overlay (1- (point)) (point)))
                    (is-first (= pipe-num 1))
                    (is-last (= pipe-num total-pipes)))
                (overlay-put ov 'category 'markdown-overlays)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'face markdown-tables-border-face)
                (when markdown-tables-use-unicode-borders
                  (overlay-put ov 'display
                               (propertize
                                (if is-separator
                                    (cond (is-first markdown-tables--border-tee-left)
                                          (is-last markdown-tables--border-tee-right)
                                          (t markdown-tables--border-cross))
                                  markdown-tables--border-pipe)
                                'face markdown-tables-border-face)))))))))))

(defun markdown-tables--fontify-all (&optional avoid-ranges)
  "Find and align all markdown tables, avoiding AVOID-RANGES."
  (let ((tables (markdown-tables--find-tables avoid-ranges)))
    (dolist (table tables)
      (markdown-tables--align-table table))))

(defun markdown-tables--advice-put (orig-fun &rest args)
  "Advice for `markdown-overlays-put' to also handle tables.
Calls ORIG-FUN with ARGS, then processes tables."
  (apply orig-fun args)
  ;; Get source block ranges to avoid (they're already processed)
  (let ((avoid-ranges '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "```" nil t)
        (let ((block-start (match-beginning 0)))
          (when (re-search-forward "```" nil t)
            (push (cons block-start (match-end 0)) avoid-ranges)))))
    (markdown-tables--fontify-all avoid-ranges)))

;;;###autoload
(defun markdown-tables-enable ()
  "Enable markdown table rendering in markdown-overlays."
  (interactive)
  (advice-add 'markdown-overlays-put :around #'markdown-tables--advice-put)
  (message "Markdown table rendering enabled"))

;;;###autoload
(defun markdown-tables-disable ()
  "Disable markdown table rendering."
  (interactive)
  (advice-remove 'markdown-overlays-put #'markdown-tables--advice-put)
  (message "Markdown table rendering disabled"))

;; Auto-enable when loaded
(with-eval-after-load 'markdown-overlays
  (markdown-tables-enable))

(provide 'markdown-tables)

;;; markdown-tables.el ends here
