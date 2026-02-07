;;; markdown-mermaid.el --- Mermaid rendering for markdown-overlays -*- lexical-binding: t -*-

;; Author: Edd Wilder-James
;; Description: Renders mermaid code fences as PNG overlays in markdown-overlays buffers
;; Designed for agent-shell output where mermaid fences are complete (not streaming)

;;; Commentary:
;;
;; Extends markdown-overlays to render mermaid code fences as inline PNG images.
;; Only renders complete fences (both opening and closing ``` present).
;;
;; Before: ```mermaid
;;         graph TD
;;             A --> B
;;         ```
;;
;; After:  [rendered PNG diagram]
;;
;; Usage: Load after markdown-overlays, then call (markdown-mermaid-enable).
;; Integrates via advice on `markdown-overlays-put', same pattern as markdown-tables.

;;; Code:

(require 'cl-lib)

(defgroup markdown-mermaid nil
  "Mermaid diagram rendering for markdown-overlays."
  :group 'markdown-overlays)

(defcustom markdown-mermaid-mmdc-path (executable-find "mmdc")
  "Path to the mmdc (mermaid-cli) binary."
  :type 'string
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-theme "default"
  "Mermaid theme to use for rendering."
  :type '(choice (const "default") (const "forest") (const "dark") (const "neutral"))
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-background "white"
  "Background color for rendered diagrams."
  :type 'string
  :group 'markdown-mermaid)

(defcustom markdown-mermaid-scale 2
  "Render scale factor for mermaid diagrams.
Higher values produce crisper images on retina displays.
The image is then scaled down in Emacs for proper sizing."
  :type 'number
  :group 'markdown-mermaid)

(defvar markdown-mermaid--cache (make-hash-table :test 'equal)
  "Cache mapping content hashes to rendered PNG file paths.")

(defvar markdown-mermaid--cache-dir
  (expand-file-name "markdown-mermaid" temporary-file-directory)
  "Directory for cached mermaid PNG renders.")

(defvar markdown-mermaid--enabled nil
  "Whether markdown-mermaid processing is enabled.")

(defun markdown-mermaid--ensure-cache-dir ()
  "Create the cache directory if it doesn't exist."
  (unless (file-directory-p markdown-mermaid--cache-dir)
    (make-directory markdown-mermaid--cache-dir t)))

(defcustom markdown-mermaid-render-timeout 15
  "Maximum seconds to wait for mmdc to render a diagram.
If exceeded, rendering is skipped and a warning is logged."
  :type 'integer
  :group 'markdown-mermaid)

(defun markdown-mermaid--render (content)
  "Render mermaid CONTENT to a PNG file, returning the file path.
Uses cache to avoid re-rendering identical content.
Renders at `markdown-mermaid-scale'x resolution for crisp display.
Aborts if rendering takes longer than `markdown-mermaid-render-timeout'."
  (let* ((sig (secure-hash 'sha1 (concat content "|" markdown-mermaid-theme
                                          "|" (number-to-string markdown-mermaid-scale))))
         (cached (gethash sig markdown-mermaid--cache)))
    (if (and cached (file-exists-p cached))
        cached
      (markdown-mermaid--ensure-cache-dir)
      (let* ((input-file (expand-file-name (concat sig ".mmd") markdown-mermaid--cache-dir))
             (output-file (expand-file-name (concat sig ".png") markdown-mermaid--cache-dir)))
        (write-region content nil input-file nil 'silent)
        (let* ((proc-buf (generate-new-buffer " *mmdc*"))
               (proc (start-process "mmdc" proc-buf
                                     markdown-mermaid-mmdc-path
                                     "-i" input-file
                                     "-o" output-file
                                     "-t" markdown-mermaid-theme
                                     "-b" markdown-mermaid-background
                                     "-s" (number-to-string markdown-mermaid-scale)
                                     "--quiet"))
               (deadline (+ (float-time) markdown-mermaid-render-timeout)))
          (while (and (process-live-p proc)
                      (< (float-time) deadline))
            (accept-process-output proc 0.5))
          (prog1
              (cond
               ((process-live-p proc)
                (delete-process proc)
                (message "markdown-mermaid: mmdc timed out after %ds" markdown-mermaid-render-timeout)
                nil)
               ((and (= (process-exit-status proc) 0) (file-exists-p output-file))
                (puthash sig output-file markdown-mermaid--cache)
                output-file)
               (t
                (message "markdown-mermaid: mmdc failed (exit %d)" (process-exit-status proc))
                nil))
            (ignore-errors (delete-process proc))
            (ignore-errors (and (buffer-live-p proc-buf)
                                (let ((buf proc-buf))
                                  (with-current-buffer buf
                                    (set-buffer-modified-p nil))
                                  (bury-buffer buf))))))))))

(defun markdown-mermaid--fontify-block (block-start block-end content)
  "Replace the mermaid fence from BLOCK-START to BLOCK-END with a rendered image.
CONTENT is the mermaid source code to render."
  (when-let* ((image-file (markdown-mermaid--render (string-trim content))))
    ;; Clean up markdown-overlays overlays that overlap our range.
    (dolist (ov (overlays-in block-start block-end))
      (when (and (eq (overlay-get ov 'category) 'markdown-overlays)
                 (not (overlay-get ov 'markdown-mermaid))
                 (>= (overlay-start ov) block-start))
        (delete-overlay ov)))
    (let* ((max-w (- (window-body-width nil t)
                     (line-number-display-width t)
                     20))
           (img (create-image image-file 'png nil
                              :max-width max-w
                              :scale (/ 1.0 markdown-mermaid-scale)))
           (ov (make-overlay block-start block-end)))
      (overlay-put ov 'category 'markdown-overlays)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display img)
      (overlay-put ov 'after-string "\n")
      (overlay-put ov 'markdown-mermaid t))))

(defun markdown-mermaid--find-blocks ()
  "Find all complete mermaid code fences in the buffer.
Returns a list of plists with :start, :end, and :content."
  (let ((blocks nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*```[ \t]*mermaid[ \t]*\n" nil t)
        (let ((block-start (match-beginning 0))
              (content-start (match-end 0)))
          ;; Find closing fence: ``` not followed by "mermaid"
          (let ((found nil))
            (while (and (not found)
                        (re-search-forward "^[ \t]*```\\([ \t]*\\)\\(.*\\)$" nil t))
              (let ((lang (string-trim (match-string 2))))
                (unless (string-match-p "\\`mermaid" lang)
                  (setq found t))))
            (when found
              (let ((block-end (min (point-max) (1+ (line-end-position))))
                    (content-end (match-beginning 0)))
                (push (list :start block-start :end block-end
                            :content (buffer-substring-no-properties
                                      content-start content-end))
                      blocks)))))))
    (nreverse blocks)))

(defun markdown-mermaid--after-fontify (&rest _args)
  "Render mermaid blocks after markdown-overlays fontification.
Finds mermaid fences, renders them as images, and cleans up
any markdown-overlays overlays that overlap the mermaid blocks."
  (when (and markdown-mermaid--enabled markdown-mermaid-mmdc-path)
    (dolist (block (markdown-mermaid--find-blocks))
      (markdown-mermaid--fontify-block
       (plist-get block :start) (plist-get block :end)
       (plist-get block :content)))))

;;;###autoload
(defun markdown-mermaid-enable ()
  "Enable mermaid diagram rendering in markdown-overlays buffers."
  (interactive)
  (setq markdown-mermaid--enabled t)
  (advice-add 'markdown-overlays-put :after #'markdown-mermaid--after-fontify)
  (message "Markdown mermaid rendering enabled"))

;;;###autoload
(defun markdown-mermaid-disable ()
  "Disable mermaid diagram rendering."
  (interactive)
  (setq markdown-mermaid--enabled nil)
  (advice-remove 'markdown-overlays-put #'markdown-mermaid--after-fontify)
  (message "Markdown mermaid rendering disabled"))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here
