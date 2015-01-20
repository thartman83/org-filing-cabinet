;;; org-filing-cabinet-auto-commit --- Auto commit utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(defcustom org-fc/auto-commit t
  "Automatically run git commit after filing a new file."
  :group 'org-fc)

(defcustom org-fc/auto-commit-message "%s\nAuto Commiting %s"
  "Control for auto commit messages."
  :group 'org-fc)

(defcustom org-fc/auto-commit-pre-command nil
  "Command to run before an auto commit."
  :group 'org-fc
  :risky t)

(defcustom org-fc/auto-commit-post-command nil
  "Command to run after an auto commit."
  :group 'org-fc
  :risky t)

(defun org-fc/auto-commit-filing-cabinet (file-name)
  "Automatically commit FILE-NAME to the filing cabinet repository."
  (when (not (null org-fc/auto-commit-pre-command))
    (funcall org-fc/auto-commit-pre-command file-name))
  (let ((commit-msg (format org-fc/auto-commit-message file-name file-name)))
    (call-process-shell-command (format  "git add '%s'" file-name))
    (call-process-shell-command (format "git commit -am '%s'" commit-msg)))
  (when (not (null org-fc/auto-commit-post-command))
    (funcall org-fc/auto-commit-post-command file-name)))

(provide 'org-filing-cabinet-auto-commit)
;;; org-filing-cabinet-auto-commit.el ends here
