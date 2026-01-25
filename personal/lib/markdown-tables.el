;;; markdown-tables.el --- Table rendering for markdown-overlays -*- lexical-binding: t -*-

;; Author: Edd Wilder-James
;; Description: Adds markdown table rendering to markdown-overlays
;; This can be submitted upstream to xenodium/shell-maker once validated

;;; Commentary:
;;
;; Extends markdown-overlays to render markdown tables with:
;; - Column alignment using overlay display properties
;; - Automatic column wrapping when table exceeds window width
;; - Unicode box-drawing borders (│ ─ ┼ ├ ┤)
;; - Zebra striping for better row distinction
;; - Bold headers, dimmed borders
;;
;; Before: | Name | Role |
;;         |------|------|
;;         | Alice Smith | Engineer |
;;
;; After:  │ Name        │ Role     │
;;         ├─────────────┼──────────┤
;;         │ Alice Smith │ Engineer │
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

(defcustom markdown-tables-wrap-columns t
  "When non-nil, wrap table columns to fit within window width.
Uses proportional shrinking with minimum widths based on longest word."
  :type 'boolean
  :group 'markdown-tables)

(defcustom markdown-tables-max-width-fraction 0.9
  "Fraction of window width to use as max table width.
Only applies when `markdown-tables-wrap-columns' is non-nil."
  :type 'float
  :group 'markdown-tables)

(defcustom markdown-tables-zebra-stripe t
  "When non-nil, alternate row backgrounds for better readability."
  :type 'boolean
  :group 'markdown-tables)

(defcustom markdown-tables-row-underline nil
  "When non-nil, underline the last line of each data row.
Helps distinguish row boundaries in wrapped tables."
  :type 'boolean
  :group 'markdown-tables)

(defface markdown-tables-zebra-face
  '((((background light)) :background "#f0f0f0" :foreground "#333333")
    (((background dark)) :background "#2a2a2a" :foreground "#cccccc"))
  "Face for alternating (even) rows in tables."
  :group 'markdown-tables)

(defface markdown-tables-row-face
  '((((background light)) :foreground "#555555")
    (((background dark)) :foreground "#aaaaaa"))
  "Face for regular (odd) data rows in tables."
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
Returns a list of plists with :start, :end, :separator-row, and :rows.
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

(defun markdown-tables--compute-min-column-widths (table)
  "Compute minimum width for each column in TABLE.
Minimum is the longest single word in the column (to avoid breaking words)."
  (let ((min-widths nil))
    (dolist (row (plist-get table :rows))
      (unless (plist-get row :separator)
        (let ((cells (markdown-tables--parse-row
                      (plist-get row :start)
                      (plist-get row :end)))
              (col 0))
          (dolist (cell cells)
            (let ((longest-word (markdown-tables--longest-word
                                 (plist-get cell :content))))
              (if (nth col min-widths)
                  (setf (nth col min-widths)
                        (max (nth col min-widths) longest-word))
                (setq min-widths (append min-widths (list longest-word))))
              (setq col (1+ col)))))))
    min-widths))

(defun markdown-tables--longest-word (str)
  "Return length of longest word in STR."
  (if (or (null str) (string-empty-p str))
      0
    (apply #'max (mapcar #'length (split-string str "[ \t\n]+" t)))))

(defun markdown-tables--total-width (widths)
  "Calculate total rendered width for WIDTHS including borders and padding."
  ;; Each column: content + 2 spaces padding + 1 pipe. Plus 1 pipe at start.
  (+ 1 (seq-reduce (lambda (acc w) (+ acc w 3)) widths 0)))

(defun markdown-tables--allocate-widths (natural-widths min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS."
  (let* ((total (markdown-tables--total-width natural-widths))
         (excess (- total target)))
    (if (<= excess 0)
        natural-widths  ; Fits already
      ;; Calculate how much each column can shrink
      (let* ((shrinkable (seq-mapn (lambda (w m) (max 0 (- w m)))
                                   natural-widths min-widths))
             (total-shrinkable (seq-reduce #'+ shrinkable 0)))
        (if (<= total-shrinkable 0)
            min-widths  ; Can't shrink further
          (let ((ratio (min 1.0 (/ (float excess) total-shrinkable))))
            (seq-mapn (lambda (w m s)
                        (max m (floor (- w (* s ratio)))))
                      natural-widths min-widths shrinkable)))))))

(defun markdown-tables--wrap-text (text width)
  "Wrap TEXT to fit within WIDTH, returning list of lines."
  (if (or (null text) (<= (length text) width))
      (list (or text ""))
    (with-temp-buffer
      (insert text)
      (let ((fill-column width))
        (fill-region (point-min) (point-max)))
      (split-string (buffer-string) "\n"))))

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
  "Apply display overlays to align TABLE columns.
If `markdown-tables-wrap-columns' is non-nil and table is too wide,
columns are wrapped to fit within window width."
  (let* ((natural-widths (markdown-tables--compute-column-widths table))
         (min-widths (markdown-tables--compute-min-column-widths table))
         (target-width (when markdown-tables-wrap-columns
                         (floor (* (window-body-width)
                                   markdown-tables-max-width-fraction))))
         (total-natural (markdown-tables--total-width natural-widths))
         (col-widths (if (and markdown-tables-wrap-columns
                              target-width
                              (> total-natural target-width))
                         (markdown-tables--allocate-widths
                          natural-widths min-widths target-width)
                       natural-widths))
         (needs-wrapping (not (equal col-widths natural-widths)))
         (separator-row (plist-get table :separator-row))
         (rows (plist-get table :rows))
         (data-row-num 0))  ; Track data rows for zebra striping

    (dolist (row rows)
      (let* ((row-start (plist-get row :start))
             (row-end (plist-get row :end))
             (row-num (plist-get row :num))
             (is-separator (plist-get row :separator))
             (is-header (and separator-row (< row-num separator-row)))
             (is-zebra (and markdown-tables-zebra-stripe
                            (not is-header)
                            (not is-separator)
                            (= (mod data-row-num 2) 1)))
             (cells (markdown-tables--parse-row row-start row-end)))

        ;; Increment data row counter for non-header, non-separator rows
        (unless (or is-header is-separator)
          (setq data-row-num (1+ data-row-num)))

        (if (and (not is-separator) (not needs-wrapping))
            ;; Simple case: no wrapping needed and not separator
            (let ((col 0))
              (dolist (cell cells)
                (let* ((cell-start (plist-get cell :start))
                       (cell-end (plist-get cell :end))
                       (content (plist-get cell :content))
                       (cell-width (or (nth col col-widths) (string-width content)))
                       (padded-content
                        (concat " " (markdown-tables--pad-string content cell-width) " "))
                       (display-str (cond
                                     (is-header
                                      (propertize padded-content 'face markdown-tables-header-face))
                                     (is-zebra
                                      (propertize padded-content 'face 'markdown-tables-zebra-face))
                                     (t
                                      (propertize padded-content 'face 'markdown-tables-row-face))))
                       (ov (make-overlay cell-start cell-end)))
                  (overlay-put ov 'category 'markdown-overlays)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'display display-str)
                  (setq col (1+ col)))))

          ;; Wrapping case OR separator row - render as complete row overlay
          (if is-separator
              ;; Separator row
              (let* ((pipe (if markdown-tables-use-unicode-borders
                               markdown-tables--border-cross "|"))
                     (pipe-left (if markdown-tables-use-unicode-borders
                                    markdown-tables--border-tee-left "|"))
                     (pipe-right (if markdown-tables-use-unicode-borders
                                     markdown-tables--border-tee-right "|"))
                     ;; Preserve leading whitespace
                     (leading-ws (save-excursion
                                   (goto-char row-start)
                                   (if (looking-at "^[ \t]*")
                                       (match-string 0) "")))
                     (row-display
                      (concat
                       leading-ws
                       (propertize pipe-left 'face markdown-tables-border-face)
                       (mapconcat
                        (lambda (idx)
                          (let ((width (nth idx col-widths)))
                            (propertize (markdown-tables--make-separator-cell (+ width 2))
                                        'face markdown-tables-border-face)))
                        (number-sequence 0 (1- (length col-widths)))
                        (propertize pipe 'face markdown-tables-border-face))
                       (propertize pipe-right 'face markdown-tables-border-face)))
                     (ov (make-overlay row-start row-end)))
                (overlay-put ov 'category 'markdown-overlays)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'display row-display))

          ;; Complex case: wrapping needed
          (let* ((wrapped-cells
                  (seq-map-indexed
                   (lambda (cell idx)
                     (let ((width (or (nth idx col-widths) 10)))
                       (markdown-tables--wrap-text
                        (plist-get cell :content) width)))
                   cells))
                 (max-lines (apply #'max 1 (mapcar #'length wrapped-cells)))
                 (pipe (if markdown-tables-use-unicode-borders
                           markdown-tables--border-pipe "|"))
                 (styled-pipe (propertize pipe 'face markdown-tables-border-face))
                 ;; Preserve leading whitespace
                 (leading-ws (save-excursion
                               (goto-char row-start)
                               (if (looking-at "^[ \t]*")
                                   (match-string 0) "")))
                 ;; Build multi-line display string for entire row
                 (row-display
                  (mapconcat
                   (lambda (line-idx)
                     (let ((is-last-line (= line-idx (1- max-lines))))
                       (concat
                        leading-ws
                        styled-pipe
                        (mapconcat
                         (lambda (col-idx)
                           (let* ((cell-lines (nth col-idx wrapped-cells))
                                  (width (nth col-idx col-widths))
                                  (line (or (nth line-idx cell-lines) ""))
                                  (padded (concat " "
                                                  (markdown-tables--pad-string line width)
                                                  " "))
                                  (base-face (cond
                                              (is-header 'markdown-tables-header-face)
                                              (is-zebra 'markdown-tables-zebra-face)
                                              (t 'markdown-tables-row-face)))
                                  (final-face (if (and markdown-tables-row-underline
                                                       is-last-line
                                                       (not is-header))
                                                  (list base-face '(:underline t))
                                                base-face)))
                             (propertize padded 'face final-face)))
                         (number-sequence 0 (1- (length cells)))
                         styled-pipe)
                        styled-pipe)))
                   (number-sequence 0 (1- max-lines))
                   "\n"))
                 ;; Create overlay for entire row (including pipes)
                 (ov (make-overlay row-start row-end)))
            (overlay-put ov 'category 'markdown-overlays)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'display row-display))))

        ;; Style pipe characters at row boundaries (only for simple non-wrapped rows)
        (when (and (not is-separator) (not needs-wrapping))
          (save-excursion
            (goto-char row-start)
            (while (re-search-forward "|" row-end t)
              (let ((ov (make-overlay (1- (point)) (point))))
                (overlay-put ov 'category 'markdown-overlays)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'face markdown-tables-border-face)
                (when markdown-tables-use-unicode-borders
                  (overlay-put ov 'display
                               (propertize markdown-tables--border-pipe
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
