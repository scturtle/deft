;;; deft.el --- Notes -*- lexical-binding: t -*-

;;; Author: Jason R. Blevins <jrblevin@xbeta.org>
;;;         scturtle <scturtle@gmail.com>
;;; Version: 1.0
;;; Package-Requires: ((emacs "29.1"))
;;; Keywords: files, matching, outlines
;;; URL: https://jblevins.org/projects/deft/
;;;      https://github.com/scturtle/deft

;;; Commentary:

;;; Notes management.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'f)

;; Customization

(defgroup deft nil
  "Emacs Deft mode."
  :group 'local)

(defcustom deft-directory (expand-file-name "~/.deft/")
  "Deft directory."
  :type 'directory
  :safe 'stringp
  :group 'deft)

(defcustom deft-extension "org"
  "File extension."
  :type 'string
  :group 'deft)

(defcustom deft-time-format " %Y-%m-%d %H:%M"
  "Format string for modification times in the Deft browser."
  :type 'string
  :group 'deft)

(defcustom deft-incremental-search t
  "Use incremental string search when non-nil and regexp search when nil.
During incremental string search, substrings separated by spaces are
treated as subfilters, each of which must match a file.  They need
not be adjacent and may appear in any order.  During regexp search, the
entire filter string is interpreted as a single regular expression."
  :type 'boolean
  :group 'deft)

(defcustom deft-strip-summary-regexp
  (concat "\\("
          "[\n\t]" ;; blank
          "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
          "\\)")
  "Regular expression to remove file contents displayed in summary.
Presently removes blank lines and `org-mode' metadata statements."
  :type 'regexp
  :safe 'stringp
  :group 'deft)

;; Faces

(defgroup deft-faces nil
  "Faces used in Deft mode"
  :group 'deft
  :group 'faces)

(defface deft-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Deft header."
  :group 'deft-faces)

(defface deft-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for Deft filter string."
  :group 'deft-faces)

(defface deft-filter-regex-error-face
  '((t :inherit font-lock-warning-face))
  "Face for Deft filter string when regexp is invalid."
  :group 'deft-faces)

(defface deft-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Deft file titles."
  :group 'deft-faces)

(defface deft-tags-face
  '((t :inherit font-lock-string-face))
  "Face for Deft file tags."
  :group 'deft-faces)

(defface deft-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for Deft file summary strings."
  :group 'deft-faces)

(defface deft-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Deft last modified times."
  :group 'deft-faces)

;; Constants

(defconst deft-buffer "*Deft*"
  "Deft buffer name.")

;; Global variables

(defvar deft-filter-regexp nil
  "A list of string representing the current filter used by Deft.

In incremental search mode, when `deft-incremental-search' is
non-nil, the elements of this list are the individual words of
the filter string, in reverse order.  That is, the car of the
list is the last word in the filter string.

In regexp search mode, when `deft-incremental-search' is nil,
this list has a single element containing the entire filter
regexp.")

(defvar deft-current-files nil
  "List of files matching current filter.")

(defvar deft-all-files nil
  "List of all files in `deft-directory'.")

(defvar deft-hash-properties nil
  "Hash containing properties for each file, keyed by filename.
Properties are `content', `mtime', `date', `title', `tags', `summary'.")

(defvar deft-window-width nil
  "Width of Deft buffer.")

(defvar deft-regexp-error nil
  "Flag for indicating invalid regexp errors.")

(defvar deft-pending-updates nil
  "Indicator of pending updates due to automatic saves, etc.")

;; Keymap definition

(defvar deft-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'deft-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'deft-filter-increment)
      (setq i (1+ i)))
    ;; Handle backspace and delete
    (define-key map (kbd "DEL") 'deft-filter-decrement)
    (define-key map (kbd "M-DEL") 'deft-filter-decrement-word)
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'deft-complete)
    ;; Filtering
    (define-key map (kbd "C-c C-c") 'deft-filter-clear)
    (define-key map (kbd "C-c C-y") 'deft-filter-yank)
    ;; File creation
    (define-key map (kbd "C-c C-m") 'deft-new-file-named)
    (define-key map (kbd "<C-return>") 'deft-new-file-named)
    ;; File management
    (define-key map (kbd "C-c C-d") 'deft-delete-file)
    (define-key map (kbd "C-c C-r") 'deft-rename-file)
    ;; Settings
    (define-key map (kbd "C-c C-t") 'deft-toggle-incremental-search)
    ;; Miscellaneous
    (define-key map (kbd "C-c C-f") 'deft-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    ;; Buttons
    (define-key map (kbd "<tab>") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    (define-key map (kbd "<S-tab>") 'backward-button)
    (define-key map (kbd "C-c C-o") 'deft-open-file-other-window)
    map)
  "Keymap for Deft mode.")

;; Helpers

(defun deft-whole-filter-regexp ()
  "Join incremental filters into one."
  (mapconcat 'identity (reverse deft-filter-regexp) " "))

(defun deft-toggle-incremental-search ()
  "Toggle the `deft-incremental-search' setting."
  (interactive)
  (cond
   (deft-incremental-search
    (setq deft-incremental-search nil)
    (message "Regexp search"))
   (t
    (setq deft-incremental-search t)
    (message "Incremental string search")))
  (deft-filter (deft-whole-filter-regexp) t))

(defun deft-filter-regexp-as-regexp ()
  "Return a regular expression corresponding to the current filter string.
When `deft-incremental-search' is non-nil, we must combine each individual
whitespace separated string.  Otherwise, the `car' of `deft-filter-regexp'
is the complete regexp."
  (if deft-incremental-search
      (mapconcat 'regexp-quote (reverse deft-filter-regexp) "\\|")
    (car deft-filter-regexp)))

;; File processing

(defun deft-base-filename (file)
  "Strip `deft-directory' and `deft-extension' from filename FILE."
  (let* ((deft-dir (file-name-as-directory (expand-file-name deft-directory)))
         (len (length deft-dir))
         (file (substring file len)))
    (file-name-sans-extension file)))

(defun deft-find-all-files ()
  (deft-find-files deft-directory))

(defun deft-find-all-files-no-prefix ()
  (let ((dir (expand-file-name deft-directory)))
    (mapcar (lambda (f) (replace-regexp-in-string dir "" f))
            (deft-find-all-files))))

(defun deft-find-files (dir)
  "Return a list of all files in the directory DIR."
  (when (file-exists-p dir)
    (let ((files (directory-files dir t "." t))
          (result nil))
      (dolist (file files)
        (when (and (file-readable-p file)
                   (not (backup-file-name-p file))
                   (string= (file-name-extension file) deft-extension))
          (push file result)))
      result)))

(defun deft-parse-title (contents)
  "Parse the given CONTENTS and determine the title."
  (when (string-match "#\\+TITLE:\\(.+\\)$" contents)
    (string-trim (match-string 1 contents))))

(defun deft-parse-tags (contents)
  "Parse the given CONTENTS and return list of tags."
  (let ((case-fold-search t))
    (when (string-match "#\\+FILETAGS:\\(.*\\)$" contents)
      (mapcan
       (lambda (k) (cl-remove-if #'string-empty-p (split-string k ":")))
       (split-string (match-string 1 contents))))))

(defconst deft-org-ts-regexp "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)")

(defun deft-org-parse-time-string (s)
  (unless (string-match deft-org-ts-regexp s)
    (error "not time string: %s" s))
  (list
   0
   (cond ((match-beginning 8) (string-to-number (match-string 8 s))) (t 0))
   (cond ((match-beginning 7) (string-to-number (match-string 7 s))) (t 0))
   (string-to-number (match-string 4 s))
   (string-to-number (match-string 3 s))
   (string-to-number (match-string 2 s))))

(defun deft-parse-date (contents)
  "Parse the given CONTENTS and return the last date time."
  (when (string-match "#\\+DATE:.*\\(\\[[^]]*\\]\\)\s*$" contents)
    (encode-time (deft-org-parse-time-string (match-string 1 contents)))))

(defun deft-cache-file (file)
  "Update file cache if FILE exists and outdated."
  (when (file-exists-p file)
    (add-to-list 'deft-all-files file)
    (let ((mtime-cache (deft-file-mtime file))
          (mtime-file (file-attribute-modification-time
                       (file-attributes (file-truename file)))))
      (if (or (not mtime-cache)
              (time-less-p mtime-cache mtime-file))
          (deft-cache-newer-file file mtime-file)))))

(defun deft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  (let* ((contents (f-read-text file))
         (date (or (deft-parse-date contents) mtime))
         (title (deft-parse-title contents))
         (tags (deft-parse-tags contents))
         (summary (string-trim-left
                   (replace-regexp-in-string
                    deft-strip-summary-regexp " " contents))))
    (puthash file `((mtime . ,mtime)
                    (date . ,date)
                    (contents . ,contents)
                    (title . ,title)
                    (tags . ,tags)
                    (summary . ,summary))
             deft-hash-properties)))

(defun deft-sort-all-files ()
  "Sort FILES in reverse order by modified time."
  (sort deft-all-files (lambda (f1 f2) (time-less-p (deft-file-date f2) (deft-file-date f1)))))

(defun deft-cache-update-all ()
  "Update file list and update cached information for each file."
  (setq deft-all-files (deft-find-all-files))
  (mapc 'deft-cache-file deft-all-files)
  (deft-sort-all-files))

(defun deft-cache-update-file (file)
  "Update cached information for a single file named FILE."
  (deft-cache-file file)
  (deft-sort-all-files))

;; Cache access

(defun deft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (alist-get 'contents (gethash file deft-hash-properties)))

(defun deft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (alist-get 'mtime (gethash file deft-hash-properties)))

(defun deft-file-date (file)
  "Retrieve date of FILE from cache."
  (alist-get 'date (gethash file deft-hash-properties)))

(defun deft-file-title (file)
  "Retrieve title of FILE from cache."
  (alist-get 'title (gethash file deft-hash-properties)))

(defun deft-file-tags (file)
  "Retrieve tags of FILE from cache."
  (alist-get 'tags (gethash file deft-hash-properties)))

(defun deft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (alist-get 'summary (gethash file deft-hash-properties)))

;; File list display

(defun deft-print-header ()
  "Prints the *Deft* buffer header."
  (progn
    (insert (propertize (if deft-incremental-search
                            "Deft(incr): " "Deft(regex): ")
                        'face 'deft-header-face))
    (insert (propertize (deft-whole-filter-regexp) 'face
                        (if (and (not deft-incremental-search) deft-regexp-error)
                            'deft-filter-regex-error-face
                          'deft-filter-string-face))))
  (insert "\n\n"))

(defun deft-current-window-width ()
  "Return current width of window displaying `deft-buffer'."
  (- (window-text-width (get-buffer-window deft-buffer)) 1))

(defun deft-buffer-render (&optional refresh)
  "Render the file browser in the *Deft* buffer.
When REFRESH is true, attempt to restore the point afterwards."
  (let ((orig-line (line-number-at-pos))
        (orig-col (current-column)))

    (when (deft-buffer-visible-p)
      (setq deft-window-width (deft-current-window-width)))

    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      ;; Print the header
      (deft-print-header)
      ;; Print the files list
      (cond
       ((not (file-exists-p deft-directory))
        (insert (concat "Directory " deft-directory " does not exist.\n")))
       ((null deft-current-files)
        (insert (if deft-filter-regexp
                    "No files match the current filter string.\n"
                  "No files found.")))
       (t
        (mapc 'deft-file-button deft-current-files))))

    (use-local-map deft-mode-map)
    (setq deft-pending-updates nil)

    ;; Position or reposition point
    (goto-char (point-min))
    (forward-line (if refresh (1- orig-line) 2))
    (forward-char (if refresh orig-col 0))))

(defun deft-string-width (str)
  "Return 0 if STR is nil and call `string-width` otherwise."
  (if str (string-width str) 0))

(define-button-type 'deft-button
  'action 'deft-open-button
  'face 'deft-title-face
  'follow-link t
  'help-echo "Edit this file")

(defun deft-file-button (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((full-title (deft-file-title file))
           (summary (deft-file-summary file))
           (time (format-time-string deft-time-format (deft-file-date file)))
           (time-width (deft-string-width time))
           (line-width (- deft-window-width time-width))
           (title-width (min line-width (deft-string-width full-title)))
           (title (if full-title
                      (truncate-string-to-width full-title title-width)
                    "[Empty file]"))
           (full-tags (string-join (deft-file-tags file) " "))
           (tags-width (min (- line-width title-width 1)
                            (deft-string-width full-tags)))
           (tags (truncate-string-to-width full-tags tags-width))
           (summary-width (min (deft-string-width summary)
                               (- line-width title-width 1
                                  (if (> tags-width 0) (+ tags-width 1) 0)))))
      (insert-text-button title
                          'type 'deft-button
                          'tag file)
      (when (> tags-width 0)
        (insert " ")
        (insert (propertize tags 'face 'deft-tags-face)))
      (when (> summary-width 0)
        (insert " ")
        (insert (propertize (truncate-string-to-width summary summary-width)
                            'face 'deft-summary-face)))
      (while (< (current-column) line-width)
        (insert " "))
      (insert (propertize time 'face 'deft-time-face))
      (insert "\n"))))

(defun deft-open-button (button)
  "Open the file tagged by BUTTON.
This is used as the action for buttons of type ``deft-button''."
  (deft-open-file (button-get button 'tag)))

(defun deft-buffer-visible-p ()
  "Return non-nil if a window is displaying `deft-buffer'."
  (get-buffer-window deft-buffer))

(defun deft-window-size-change-function (_)
  "Possibly refresh Deft buffer when size of a window in FRAME is changed.
If there are pending updates, refresh the filtered files list and
update the Deft browser.  Otherwise, if the window width changed,
only update the Deft browser."
  (when (deft-buffer-visible-p)
    (cond (deft-pending-updates (deft-refresh-filter))
          ((/= deft-window-width (deft-current-window-width))
           (deft-refresh-browser)))))

(defun deft-window-configuration-change-function ()
  "Possibly refresh Deft browser when window configuration is changed."
  (deft-window-size-change-function nil))

(defun deft-refresh ()
  "Update the file cache, reapply the filter, and refresh the *Deft* buffer."
  (interactive)
  (deft-cache-update-all)
  (deft-refresh-filter))

(defun deft-refresh-filter ()
  "Reapply the filter and refresh the *Deft* buffer.
Call this after any actions which update the cache."
  (interactive)
  (deft-filter-update)
  (deft-refresh-browser))

(defun deft-refresh-browser ()
  "Refresh the *Deft* buffer in the background.
Call this function after any actions which update the filter and file list."
  (when (get-buffer deft-buffer)
    (with-current-buffer deft-buffer
      (deft-buffer-render t))))

;; File list file management actions

(defun deft-absolute-filename (slug)
  "Return an absolute filename to file named SLUG."
  (let* ((slug (string-trim slug)))
    (setq slug (replace-regexp-in-string "\/" "_" slug))
    (setq slug (replace-regexp-in-string " " "_" slug))
    (setq slug (downcase slug))
    (concat (file-name-as-directory (expand-file-name deft-directory))
            slug "." deft-extension)))

(defun deft-open-file (file &optional other switch)
  "Open FILE in a new buffer and setting its mode.
When OTHER is non-nil, open the file in another window.  When
OTHER and SWITCH are both non-nil, switch to the other window.
FILE must be a relative or absolute path, with extension."
  (let ((buffer (find-file-noselect (file-truename file))))
    (with-current-buffer buffer
      (hack-local-variables)
      ;; Find the first appearance of filter string
      (when deft-filter-regexp
        (goto-char (point-min))
        (re-search-forward (deft-filter-regexp-as-regexp) nil t))
      ;; Ensure that Deft has been initialized
      (unless (get-buffer deft-buffer)
        (with-current-buffer (get-buffer-create deft-buffer)
          (deft-mode)))
      ;; Update file after saved
      (add-hook 'after-save-hook
                (lambda () (save-excursion
                             (deft-cache-update-file buffer-file-name)
                             (if (deft-buffer-visible-p)
                                 (deft-refresh-filter)
                               (setq deft-pending-updates t))))
                nil t))
    (if other
        (if switch
            (switch-to-buffer-other-window buffer)
          (display-buffer buffer other))
      (switch-to-buffer buffer))))

(defun deft-new-file-named (slug)
  "Create a new file named SLUG.
SLUG is the short file name, without a path or a file extension."
  (interactive "sNew filename (without extension): ")
  (let ((file (deft-absolute-filename slug)))
    (if (file-exists-p file)
        (message "Aborting, file already exists: %s" file)
      (write-region (concat "#+TITLE: " slug "\n\n") nil file nil)
      (deft-cache-update-file file)
      (deft-refresh-filter)
      (deft-open-file file)
      (goto-char (point-max)))))

(defun deft-filename-at-point ()
  "Return the name of the file represented by the button at the point.
Return nil if the point is not on a file button."
  (when-let ((button (button-at (point))))
    (button-get button 'tag)))

(defun deft-open-file-other-window (&optional arg)
  "When the point is at a button, open the file in the other window.
The argument ARG is passed to `deft-open-file'."
  (interactive "P")
  (when-let ((file (deft-filename-at-point)))
    (deft-open-file file t arg)))

(defun deft-delete-file ()
  "Delete the file represented by the button at the point.
If the point is not on a file button, do nothing.  Prompts before
proceeding."
  (interactive)
  (when-let ((filename (deft-filename-at-point)))
    (when (y-or-n-p
           (concat "Delete file " (file-name-nondirectory filename) "? "))
      (when-let ((buffer (get-file-buffer (file-truename filename))))
        (kill-buffer buffer))
      (delete-file filename)
      (delq filename deft-current-files)
      (delq filename deft-all-files)
      (deft-refresh))))

(defun deft-rename-file ()
  "Rename the file represented by the button at the point.
If the point is not on a file button, do nothing."
  (interactive)
  (let ((old-filename (deft-filename-at-point))
        (deft-dir (file-name-as-directory deft-directory))
        new-filename old-name new-name)
    (when old-filename
      (setq old-name (deft-base-filename old-filename))
      (setq new-name (read-string
                      (concat "Rename " old-name " to (without extension): ")
                      old-name))
      (setq new-filename (concat deft-dir new-name "." deft-extension))
      (rename-file old-filename new-filename)
      ;; update visiting buffers
      (when-let ((buffer (get-file-buffer (file-truename old-filename))))
        (with-current-buffer buffer
          (set-visited-file-name new-filename nil t)
          (hack-local-variables))))
    (deft-refresh)))

;; File list filtering

(defun deft-search-forward (str)
  "Function to use when matching files against filter strings STR.
This function calls `search-forward' when `deft-incremental-search'
is non-nil and `re-search-forward' otherwise."
  (let ((case-fold-search t))  ;; case in-sensitive
    (if deft-incremental-search
        (search-forward str nil t)
      (re-search-forward str nil t))))

(defun deft-filter-match-file (file)
  "Return FILE if it is a match against the current filter regexp."
  (with-temp-buffer
    (insert file)
    (let ((title (deft-file-title file))
          (tags (deft-file-tags file))
          (contents (deft-file-contents file)))
      (when title (insert title))
      (when contents (insert contents))
      (cl-every
       (lambda (filter)
         ;; if filter starts with ":", match it with tags of file
         (if (string-prefix-p ":" filter)
             (cl-some (lambda (tag) (string-prefix-p (substring filter 1) tag)) tags)
           (goto-char (point-min))
           (deft-search-forward filter)))
       deft-filter-regexp))))

(defun deft-filter-files (files)
  "Filter FILES with `deft-filter-match-file'."
  (condition-case nil
      (progn
        (setq deft-regexp-error nil)
        (cl-remove-if-not 'deft-filter-match-file files))
    ;; Upon an error (`invalid-regexp'), set an error flag
    (error
     (progn
       (setq deft-regexp-error t)
       files))))

(defun deft-filter-update ()
  "Update the filtered files list using the current filter regexp.
Starts from scratch using `deft-all-files'.  Does not refresh the
Deft buffer."
  (if (not deft-filter-regexp)
      (setq deft-current-files deft-all-files)
    (setq deft-current-files
          (deft-filter-files deft-all-files))))

;; Filters that cause a refresh

(defun deft-filter-clear ()
  "Clear the current filter string and refresh the file browser."
  (interactive)
  (when deft-filter-regexp
    (setq deft-filter-regexp nil)
    (setq deft-current-files deft-all-files)
    (deft-refresh-filter))
  (message "Filter cleared."))

(defun deft-filter (str &optional reset)
  "Update the filter with STR and update the file browser.

In incremental search mode, the car of `deft-filter-regexp' will
be replaced with STR.  If STR has zero length and the length of
the list is greater than one, the empty string will be retained
to simulate whitespace.  However, if STR has zero length and the
list is of length one, then the filter will be cleared.  If STR
is nil, then the car is removed from the list.

In regexp search mode, the current filter string will be replaced
with STR.

When RESET, replace the entire filter string."
  (if deft-incremental-search
      ;; Incremental search mode
      (if reset
          (if (= (length str) 0)
              (setq deft-filter-regexp nil)
            (setq deft-filter-regexp (reverse (split-string str " "))))
        (if (not str)
            ;; If str is nil, remove it and filter with the cdr
            (setq deft-filter-regexp (cdr deft-filter-regexp))
          ;; Use STR it as the new car, even when empty (to simulate
          ;; whitespace), unless this is the only element in the list.
          (if (and (= (length deft-filter-regexp) 1)
                   (= (length str) 0))
              (setq deft-filter-regexp nil)
            (setcar deft-filter-regexp str))))
    ;; Regexp search mode
    (if (> (length str) 0)
        (setq deft-filter-regexp (list str))
      (setq deft-filter-regexp nil)))
  (deft-refresh-filter))

(defun deft-filter-increment ()
  "Append character to the filter regexp and update `deft-current-files'."
  (interactive)
  (let ((char last-command-event))
    (if (= char ?\S-\ )
        (setq char ?\s))
    (setq char (char-to-string char))
    (if (and deft-incremental-search (string= char " "))
        (setq deft-filter-regexp (cons "" deft-filter-regexp))
      (if (car deft-filter-regexp)
          (setcar deft-filter-regexp (concat (car deft-filter-regexp) char))
        (setq deft-filter-regexp (list char)))
      ;; filter and render
      (setq deft-current-files (deft-filter-files deft-current-files))
      (setq deft-current-files (delq nil deft-current-files))
      (deft-refresh-browser))))

(defun deft-filter-decrement ()
  "Remove last character from the filter, if possible, and update.

In incremental search mode, the elements of `deft-filter-regexp'
are the words of the filter string in reverse order.  In regexp
search mode, the list is a single element containing the entire
filter regexp.  Therefore, in both cases, only the car of
`deft-filter-regexp' is modified."
  (interactive)
  (let ((str (car deft-filter-regexp)))
    (deft-filter
     (if (> (length str) 0)
         (substring str 0 -1)
       nil))))

(defun deft-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update."
  (interactive)
  (deft-filter
   (if deft-incremental-search
       ;; In incremental search mode, remove the car
       nil
     ;; In regexp search mode, remove last "word" component
     (replace-regexp-in-string
      "[[:space:]]?[^[:space:]]*$" "" (car deft-filter-regexp)))))

(defun deft-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive)
  (deft-filter
   (concat (deft-whole-filter-regexp) (current-kill 0 t)) t))

(defun deft-complete ()
  "Complete the current action.
If there is a button at the point, press it.
If a filter is applied and there is at least one match,
open the first matching file."
  (interactive)
  (cond
   ;; Activate button
   ((button-at (point))
    (push-button))
   ;; Active filter string with match
   ((and deft-filter-regexp deft-current-files)
    (deft-open-file (car deft-current-files)))))

;;; Org-link

(declare-function org-link-store-props "org")
(declare-function org-store-link-props "org")
(declare-function org-link-set-parameters "org")
(declare-function org-open-file "org")

(defun deft--org-store-link ()
  "Store the Deft button at point as an org-mode link."
  (when (equal major-mode 'deft-mode)
    (let ((link (concat "deft:" (file-name-nondirectory (deft-filename-at-point))))
          (title (deft-file-title (deft-filename-at-point))))
      (org-link-store-props
       :type "deft"
       :link link
       :description title))))

(defun deft--org-follow-link (handle)
  (org-open-file (expand-file-name handle deft-directory)))

(defun deft--org-complete ()
  (let ((file (completing-read "file " (deft-find-all-files-no-prefix))))
    (concat "deft:" (substring file 1))))

(with-eval-after-load 'org
  (org-link-set-parameters
   "deft"
   :follow 'deft--org-follow-link
   :store 'deft--org-store-link
   :complete 'deft--org-complete))

;;; Mode definition

;; Deft mode is suitable only for specially-prepared text
(put 'deft-mode 'mode-class 'special)

(defun deft-mode ()
  (message "Deft initializing...")
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory (expand-file-name deft-directory))
  (setq deft-window-width (if (deft-buffer-visible-p)
                              (deft-current-window-width)
                            (frame-text-cols)))
  (use-local-map deft-mode-map)
  (setq major-mode 'deft-mode)
  (setq mode-name "Deft")

  (setq deft-hash-properties (make-hash-table :test 'equal))
  (deft-cache-update-all)
  (setq deft-current-files deft-all-files)
  (deft-buffer-render)

  (add-hook 'window-size-change-functions
            'deft-window-size-change-function t)
  (add-hook 'window-configuration-change-hook
            'deft-window-configuration-change-function t)
  (message "Deft loaded %d files." (length deft-all-files)))

(put 'deft-mode 'mode-class 'special)

;;;###autoload
(defun deft ()
  "Switch to *Deft* buffer and load files."
  (interactive)
  (switch-to-buffer deft-buffer)
  (unless (eq major-mode 'deft-mode) (deft-mode)))

(provide 'deft)

;;; deft.el ends here
