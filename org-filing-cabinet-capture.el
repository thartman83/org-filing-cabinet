;;; org-filing-cabinet-capture --- Capture utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(require 'f)
(require 's)
(require 'org-filing-cabinet-scan)
(require 'org-filing-cabinet-auto-commit)

(defcustom org-fc/filing-cabinet-directory "~/filing-cabinet"
  "Path to filing cabinet."
  :group 'org-fc
  :type 'string)

(defcustom org-fc/org-file "filing-cabinet.org"
  "Name of the filing cabinet org file."
  :group 'org-fc
  :type 'string)

(defcustom org-fc/whose "Me"
  "| seperated list of people for whom the file belongs to."
  :group 'org-fc
  :type 'string)

(defcustom org-fc/capture-template-base
  "* [[%c][%(f-filename (car kill-ring))]] %^g
:PROPERTIES:
:WHOSE: *WHOSE*
:DATE-FILED: %t
:DATE-RECEIVED: %^t
:FROM: %^{Who was the file from? }
:END:\n%?"
  "Base template for the capture template."
  :group 'org-fc
  :type 'string)

(defun org-fc/capture-file (file-path)
  "Capture FILE-PATH as a new org-filing-cabinet records."
  (interactive "FFile: ")
  (let ((org-capture-templates
         `(("f" "Filing Cabinet" entry
            (file+headline ,(f-join org-fc/filing-cabinet-directory org-fc/org-file)
                           ,(org-fc/get-create-category))
            ,(org-fc/get-capture-template))))
        (filing-path (if (f-parent-of? (org-fc/current-filing-cabinet-dir) file-path)
                         file-path
                       (f-join (org-fc/current-filing-cabinet-dir)
                               (f-filename file-path)))))
    (when (not (f-exists? file-path))
      (error "The file `%s' does not exist and can't be added to the filing cabinet"
             file-path))
    (when (not (f-same? file-path filing-path))
      (when (f-exists? filing-path)
        (error "The file `%s' already exists can not refile"))
      (if (y-or-n-p "Move file to filing cabinet (no to Copy) ? ")
          (f-move file-path filing-path)
        (f-copy file-path (f-join (org-fc/current-filing-cabinet-dir)
                                  (f-filename file-path)))))
    (kill-new filing-path)
    (org-capture nil "f")))

(defun org-fc/get-capture-template ()
  "Get the capture template."
  (replace-regexp-in-string (regexp-quote "*WHOSE*") (org-fc/parse-whose)
                            org-fc/capture-template-base))

(defun org-fc/capture-scan-file (file-name)
  "Scan and capture FILE-NAME into the filing cabinet."
  (interactive
   (let ((timestamp (format-time-string "%Y%m%d-%H%M")))
     (list (read-string "Scan file name: " (format "%s-" timestamp)))))
  (let ((file-path (org-fc/scan-file file-name)))
    (when (not (f-exists? file-path))
      (error "File `%s' does not exist, can not be filed" file-path))
    (let ((filing-cabinet-path (f-join (org-fc/current-filing-cabinet-dir)
                                       (f-filename file-path))))
      (condition-case f-error
          (f-move file-path filing-cabinet-path)
        (error
         (error "Unable to move scan file into cabinet: `%s: %s'"
                (cadr f-error)
                (mapconcat #'identity (cdr (cdr f-error)) ", "))))
      (org-fc/capture-file filing-cabinet-path))))

(defun org-fc/current-filing-cabinet-dir ()
  "Return the name of the directory of the current filing cabinet.
The current filing cabinet is defined as YYYY-mm off the root directory
of org-fc/filing-cabinet-directory.

If the directory does not exist create it."
  (when (not (f-directory? org-fc/filing-cabinet-directory))
    (error "Unable to locate filing cabinet directory `%s'"
           org-fc/filing-cabinet-directory))
  (when (not (f-writable? org-fc/filing-cabinet-directory))
    (error "Unable to write to filing cabinet directory `%s'"))
  (let ((retval (f-join org-fc/filing-cabinet-directory
                        (format-time-string "%Y-%m"))))
    (when (not (f-directory? retval))
      (message "Filing cabinet directory `%s' does not exists, creating."
               retval)
      (f-mkdir retval))
    retval))

(defun org-fc/get-create-category ()
  "Return the current time frame category name in the format 'YYYY-MM'.
If the current time frame category does not exist append it to
  the document as a new entry."
  (let ((time-frame (format-time-string "%Y-%m"))
        (org-path (f-join org-fc/filing-cabinet-directory org-fc/org-file)))
    (when (not (f-exists? org-path))
      (error "Could not find filing cabinet org file `%s'" org-path))
    (when (not (string-match-p (format "* %s" time-frame)
                               (f-read-text org-path)))
      (message "Creating new category `%s'" time-frame)
      (with-current-buffer (find-file-noselect org-path)
        (goto-char (point-max))
        (insert "#+CATEGORY: " time-frame "\n")
        (insert "* " time-frame)
        (save-buffer)
        (kill-buffer)))
    time-frame))

(defun org-fc/parse-whose ()
  "Parse `org-fc/whose' variable.

`org-fc/whose' should be either a string containing the name of
the person to whom the file belong to or a pipe delimited list of
persons.  If the the variable contains a single entry, return
it.  Otherwise create a `org-capture' expansions string that will
prompt during capture for the member of the list to use as the
value."
  (if (s-contains? "|" org-fc/whose)
      (s-concat "%^{Whose file? |" org-fc/whose "}")
    org-fc/whose))

(provide 'org-filing-cabinet-capture)
;;; org-filing-cabinet-capture.el ends here
