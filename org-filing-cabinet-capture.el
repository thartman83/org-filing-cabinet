;;; org-filing-cabinet-capture --- Capture utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(require 'f)
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
                           ,(format-time-string "%Y-%m"))
            ,(org-fc/get-capture-template)))))
    (kill-new file-path)
    (org-capture nil "f")))

(defun org-fc/get-capture-template ()
  "Get the capture template."
  (replace-regexp-in-string (regexp-quote "*WHOSE*") org-fc/whose
                            org-fc/capture-template-base))

(defun org-fc/capture-scan-file (file-name)
  "Scan and capture FILE-NAME into the filing cabinet."
  (interactive
   (let ((timestamp (format-time-string "%Y%m%d-%H%M")))
     (list (read-string "Scan file name: " (format "%s-" timestamp)))))
  (let* ((file-path (org-fc/scan-file file-name))
         (filing-cabinet-path (f-join (org-fc/current-filing-cabinet-dir)
                              (f-filename file-path))))
    (f-move file-path filing-cabinet-path)
    (org-fc/capture-file filing-cabinet-path)))

(defun org-fc/current-filing-cabinet-dir ()
  "Return the name of the directory of the current filing cabinet.
The current filing cabinet is defined as YYYY-mm off the root directory
of org-fc/filing-cabinet-directory.

If the directory does not exist create it."
  (let ((retval (f-join org-fc/filing-cabinet-directory
                        (format-time-string "%Y-%m"))))
    (when (not (f-directory? retval))
      (message "Filing cabinet directory `%s' does not exists, creating."
               retval)
      (f-mkdir retval))
    retval))

(provide 'org-filing-cabinet-capture)
;;; org-filing-cabinet-capture.el ends here
