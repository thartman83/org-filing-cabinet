;;; org-filing-cabinet-capture --- Capture utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(require 'org-filing-cabinet-scan)
(require 'org-filing-cabinet-auto-commit)

(defcustom org-fc/org-file "~/filing-cabinet/filing-cabinet.org"
  "Name of the filing cabinet org file."
  :group 'org-fc
  :type 'string)

(defcustom org-fc/whose "Me"
  "| seperated list of people for whom the file belongs to."
  :group 'org-fc
  :type 'string)

(defcustom org-fc/capture-template-base
  "* [[%c]] %^g
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
            (file+headline org-fc/org-file "2015-01")
            ,(org-fc/get-capture-template)))))
    (kill-new file-path)
    (org-capture nil "f")))

(defun org-fc/get-capture-template ()
  "Get the capture template."
  (replace-regexp-in-string (regexp-quote "*WHOSE*") org-fc/whose
                            org-fc/capture-template-base))

(provide 'org-filing-cabinet-capture)
;;; org-filing-cabinet-capture.el ends here
