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
;; - Inline markdown formatting (bold, italic, code, links, strikethrough)
;;
;; Before: | Name | Role |
;;         |------|------|
;;         | **Alice** | [Engineer](http://x.com) |
;;
;; After:  │ Name      │ Role     │
;;         ├───────────┼──────────┤
;;         │ Alice     │ Engineer │  (with bold and clickable link)
;;
;; Usage: Simply load this file after markdown-overlays is available.
;; It advises `markdown-overlays-put' to also process tables.
;;
;; Note on implementation strategy:
;; Tables use the overlay `display' property to replace entire cell regions
;; with formatted strings. This differs from other markdown elements in
;; markdown-overlays which use buffer overlays to hide markup while styling
;; text in-place.
;;
;; We chose the display property approach because:
;; - Tables require precise column alignment and width control
;; - Cell content may wrap to multiple lines
;; - Unicode box-drawing characters replace ASCII pipes/dashes
;; - A single overlay per cell is simpler than multiple hide/show overlays
;;
;; Trade-offs:
;; - Copy/paste from tables gets the rendered text, not raw markdown
;; - Search won't find markdown syntax hidden in table cells
;; - Consistent with how wrapped/multi-line content must work anyway

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

;;; Inline Markdown Processing for Table Cells

(defun markdown-tables--replace-markup (str regex group-num face &optional extra-props)
  "Replace REGEX matches in STR with propertized text.
GROUP-NUM is the capture group containing the text to keep.
FACE is applied to the replacement text.
EXTRA-PROPS is an optional plist of additional text properties."
  (let ((new-result "")
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (text (match-string group-num str))
             (propertized (apply #'propertize text 'face face extra-props)))
        (setq new-result (concat new-result (substring str pos match-start) propertized))
        (setq pos match-end)))
    (concat new-result (substring str pos))))

(defun markdown-tables--replace-markup-alt (str regex group1 group2 face)
  "Replace REGEX matches in STR, using GROUP1 or GROUP2 for text.
Used for patterns like **text** or __text__ where either group may match."
  (let ((new-result "")
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (text (or (match-string group1 str) (match-string group2 str))))
        (setq new-result (concat new-result
                                 (substring str pos match-start)
                                 (propertize text 'face face)))
        (setq pos match-end)))
    (concat new-result (substring str pos))))

(defun markdown-tables--process-cell-content (content)
  "Process markdown syntax in CONTENT string, returning propertized string.
Handles: links [text](url), bold **text**/__text__, italic *text*/_text_,
bold-italic ***text***, inline code `text`, and strikethrough ~~text~~."
  (let ((result content))
    ;; Process in order: links (special), bold-italic, bold, italic, code, strikethrough

    ;; Links need special handling for keymap
    (let ((link-re "\\[\\([^]]+\\)\\](\\([^)]+\\))")
          (new-result "")
          (pos 0))
      (while (string-match link-re result pos)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (title (match-string 1 result))
               (url (match-string 2 result))
               (link-map (make-sparse-keymap)))
          (setq new-result (concat new-result (substring result pos match-start)))
          (define-key link-map [mouse-1]
                      (lambda () (interactive) (browse-url url)))
          (define-key link-map (kbd "RET")
                      (lambda () (interactive) (browse-url url)))
          (setq new-result (concat new-result
                                   (propertize title
                                               'face 'link
                                               'mouse-face 'highlight
                                               'keymap link-map
                                               'help-echo url)))
          (setq pos match-end)))
      (setq result (concat new-result (substring result pos))))

    ;; Simple patterns using helper
    (setq result (markdown-tables--replace-markup
                  result "\\*\\*\\*\\([^*]+\\)\\*\\*\\*" 1 '(:weight bold :slant italic)))
    (setq result (markdown-tables--replace-markup-alt
                  result "\\(?:\\*\\*\\([^*]+\\)\\*\\*\\|__\\([^_]+\\)__\\)" 1 2 'bold))
    (setq result (markdown-tables--replace-markup-alt
                  result "\\(?:\\*\\([^*]+\\)\\*\\|_\\([^_]+\\)_\\)" 1 2 'italic))
    (setq result (markdown-tables--replace-markup
                  result "`\\([^`]+\\)`" 1 'font-lock-doc-markup-face))
    (setq result (markdown-tables--replace-markup
                  result "~~\\([^~]+\\)~~" 1 '(:strike-through t)))

    result))

;;; Column Width Computation

(defun markdown-tables--compute-column-widths (table)
  "Compute the maximum width of each column in TABLE.
Returns a list of integers, one per column.
Widths are based on PROCESSED content (after markdown syntax is removed)."
  (let ((widths nil))
    (dolist (row (plist-get table :rows))
      (unless (plist-get row :separator)
        (let ((cells (markdown-tables--parse-row
                      (plist-get row :start)
                      (plist-get row :end)))
              (col 0))
          (dolist (cell cells)
            ;; Process markdown first to get actual display width
            (let* ((raw-content (plist-get cell :content))
                   (processed (markdown-tables--process-cell-content raw-content))
                   (content-width (string-width processed)))
              (if (nth col widths)
                  (setf (nth col widths)
                        (max (nth col widths) content-width))
                ;; Extend widths list
                (setq widths (append widths (list content-width))))
              (setq col (1+ col)))))))
    widths))

(defun markdown-tables--compute-min-column-widths (table)
  "Compute minimum width for each column in TABLE.
Minimum is the longest single word in the column (to avoid breaking words).
Based on PROCESSED content (after markdown syntax is removed)."
  (let ((min-widths nil))
    (dolist (row (plist-get table :rows))
      (unless (plist-get row :separator)
        (let ((cells (markdown-tables--parse-row
                      (plist-get row :start)
                      (plist-get row :end)))
              (col 0))
          (dolist (cell cells)
            ;; Process markdown first to get actual display content
            (let* ((raw-content (plist-get cell :content))
                   (processed (markdown-tables--process-cell-content raw-content))
                   (longest-word (markdown-tables--longest-word processed)))
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

;;; Text Wrapping (preserves text properties)

(defun markdown-tables--wrap-text (text width)
  "Wrap TEXT to fit within WIDTH, returning list of lines.
Preserves text properties across wrapped lines."
  (if (or (null text) (string-empty-p text))
      (list "")
    (if (<= (string-width text) width)
        (list text)
      ;; Wrap while preserving text properties
      (let ((lines '())
            (pos 0)
            (len (length text)))
        (while (< pos len)
          (let* ((remaining (- len pos))
                 (end-pos (min (+ pos width) len))
                 ;; Find a good break point (space) if we're not at the end
                 (break-pos (if (>= end-pos len)
                                end-pos
                              ;; Look backwards for a space
                              (let ((space-pos nil))
                                (save-match-data
                                  (when (string-match ".*\\s-" (substring text pos end-pos))
                                    (setq space-pos (+ pos (match-end 0)))))
                                (or space-pos end-pos))))
                 (line (substring text pos break-pos)))
            ;; Trim trailing whitespace from line
            (setq line (string-trim-right line))
            (push line lines)
            ;; Skip leading whitespace on next line
            (setq pos break-pos)
            (while (and (< pos len)
                        (memq (aref text pos) '(?\s ?\t)))
              (setq pos (1+ pos)))))
        (nreverse lines)))))

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

;;; Main Table Alignment

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

          ;; Content row - use unified approach that handles both simple and wrapped cases
          (let* ((wrapped-cells
                  (seq-map-indexed
                   (lambda (cell idx)
                     (let* ((width (or (nth idx col-widths) 10))
                            (content (plist-get cell :content))
                            ;; Process markdown FIRST, then wrap (preserves properties)
                            (processed (markdown-tables--process-cell-content content)))
                       (markdown-tables--wrap-text processed width)))
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
                                              (is-header markdown-tables-header-face)
                                              (is-zebra 'markdown-tables-zebra-face)
                                              (t 'markdown-tables-row-face)))
                                  (final-face (if (and markdown-tables-row-underline
                                                       is-last-line
                                                       (not is-header))
                                                  (list base-face '(:underline t))
                                                base-face)))
                             ;; Use add-face-text-property to preserve inline formatting
                             (add-face-text-property 0 (length padded) final-face t padded)
                             padded))
                         (number-sequence 0 (1- (length cells)))
                         styled-pipe)
                        styled-pipe)))
                   (number-sequence 0 (1- max-lines))
                   "\n"))
                 ;; Create overlay for entire row (including pipes)
                 (ov (make-overlay row-start row-end)))
            (overlay-put ov 'category 'markdown-overlays)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'display row-display)))))))

;;; Integration with markdown-overlays

(defun markdown-tables--fontify-tables (avoid-ranges)
  "Find and align all markdown tables, avoiding AVOID-RANGES."
  (dolist (table (markdown-tables--find-tables avoid-ranges))
    (markdown-tables--align-table table)))

(defvar markdown-tables--enabled nil
  "Whether markdown-tables processing is enabled.")

(defun markdown-tables--after-fontify (&rest _args)
  "Hook to process tables after markdown-overlays fontification."
  (when markdown-tables--enabled
    ;; Get avoid ranges from code blocks if available
    (let ((avoid-ranges '()))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*```" nil t)
          (let ((start (line-beginning-position)))
            (when (re-search-forward "^[ \t]*```" nil t)
              (push (cons start (point)) avoid-ranges)))))
      (markdown-tables--fontify-tables avoid-ranges))))

;;;###autoload
(defun markdown-tables-enable ()
  "Enable markdown table rendering."
  (interactive)
  (setq markdown-tables--enabled t)
  (advice-add 'markdown-overlays-put :after #'markdown-tables--after-fontify)
  (message "Markdown tables enabled"))

;;;###autoload
(defun markdown-tables-disable ()
  "Disable markdown table rendering."
  (interactive)
  (setq markdown-tables--enabled nil)
  (advice-remove 'markdown-overlays-put #'markdown-tables--after-fontify)
  (message "Markdown tables disabled"))

(provide 'markdown-tables)
;;; markdown-tables.el ends here
