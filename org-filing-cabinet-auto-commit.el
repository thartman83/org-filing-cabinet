;;; org-filing-cabinet-auto-commit --- Auto commit utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(defcustom org-fc/auto-commit nil
  "Automatically run git commit after filing a new file."
  :group 'org-fc)

(defcustom org-fc/auto-commit-message "'%s\nAuto Commiting %s'"
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

(defmacro org-fc/run-git-command (command &rest args)
  "Run `git COMMAND ARGS'."
  (when (not (string= command "init"))
    (when (not (org-fc/is-git-repo?))
      (error "Not in a git repository")))
  (if (null args)
      `(call-process "git" nil nil nil ,command)
    `(call-process "git" nil nil nil ,command ,@args)))

(defun org-fc/auto-commit-filing-cabinet (file-name org-file)
  "Automatically commit FILE-NAME and ORG-FILE to the filing cabinet."
  (when (not (null org-fc/auto-commit-pre-command))
    (funcall org-fc/auto-commit-pre-command file-name))
  (let ((commit-msg (format org-fc/auto-commit-message file-name file-name)))
    (org-fc/run-git-command "add" file-name)
    (org-fc/run-git-command "add" org-file)
    (org-fc/run-git-command "commit" "-m" commit-msg))
  (when (not (null org-fc/auto-commit-post-command))
    (funcall org-fc/auto-commit-post-command file-name)))

(defun org-fc/is-git-repo? ()
  "Return t is the current directory is a git repository."
  (= 0 (call-process-shell-command "git status")))

(provide 'org-filing-cabinet-auto-commit)
;;; org-filing-cabinet-auto-commit.el ends here
