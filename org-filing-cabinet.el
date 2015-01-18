;;; org-filing-cabinet --- Filing cabinet functions and utiltys for org mode
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'f)

(defgroup org-fc nil
  "org-fc"
  :prefix "org-fc/"
  :group 'org)

(defconst org-fc/version "0.1"
  "Version of org-filing-cabinet package.")

(defun org-filing-cabinet-version ()
  "Version of the org-filer package."
  (interactive)
  (message "org-filer version: %d" org-fc/version)
  "0.1")

(require 'org-filing-cabinet-scan)
(require 'org-filing-cabinet-auto-commit)
(require 'org-filing-cabinet-capture)

(provide 'org-filing-cabinet)
;;; org-filing-cabinet.el ends here
