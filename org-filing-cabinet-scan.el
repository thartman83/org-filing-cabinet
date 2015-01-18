;;; org-filing-cabinet-scan --- Scanning utilities and functions
;;; Commentary:
;;; Code:

(require 'f)
(require 'org-filing-cabinet-dir-utils)

(defcustom org-fc/scan-cmd "scanimage > '%s'"
  "Command invoked to scan file for filing."
  :group 'org-fc
  :risky t
  :type 'string)

(defcustom org-fc/merge-pdf-cmd "convert *.pnm '%s'"
  "Command invoked to merge scanned images into a single pdf."
  :group 'org-fc
  :risky t)

(defun org-fc/scan-file (file-name)
  "Use an attached scanner to scan multiple pages to FILE-NAME."
  (interactive "sFile name: ")
  (org-fc/with-temp-dir "/tmp"
    (let ((page-num 0)
          (page-format (f-join default-directory "%s%d.pnm")))
      (while (org-fc/continue-scan? page-num)
        (org-fc/scan-page (format page-format (f-base file-name) page-num))
        (setf page-num (1+ page-num)))
      (org-fc/merge-pages-to-pdf default-directory
                                 (f-join (f-parent default-directory) file-name)))))

(defun org-fc/continue-scan? (page-num)
  (if (= page-num 0)
      (y-or-n-p "Ready to scan first page? ")
    (y-or-n-p "Ready to scan next page? ")))

(defun org-fc/scan-page (page-file)
  "Use an attached scanner to scan a page to PAGE-FILE."
  (interactive "F")
  (message "Scanning %s... " page-file)
  (let ((retval (call-process-shell-command (format org-fc/scan-cmd page-file))))
    (message "Finished scanning %s." page-file)
    (if (= retval 0) t nil)))

(defun org-fc/merge-pages-to-pdf (image-path pdf-file)
  "Merge all .PNM files in IMAGE-PATH into PDF-FILE."
  (message "Merging pages into %s... " pdf-file)
  (call-process-shell-command
   (format org-fc/merge-pdf-cmd pdf-file))
  pdf-file)

(provide 'org-filing-cabinet-scan)
;;; org-filing-cabinet-scan ends here
