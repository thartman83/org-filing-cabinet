;;; org-filing-cabinet-auto-commit --- Auto commit utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(require 'cl)

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

(defun org-fc/run-git-cmd (command &rest args)
  "Run `git COMMAND ARGS'."
  (let ((cmd (list "git" nil nil nil command)))
    (when (not (null args))
      (setf cmd (append cmd args)))
    (let ((retval (apply #'call-process cmd)))
      (if (not (= retval 0))
          (error "Git command `%s' failed with error code %d"
                 (apply #'concat (append (list command) args))
                 retval)))))

(defmacro org-fc/with-dir (dir &rest body)
  "For the duration of the macro scope, use DIR as default directory when executing BODY."
  (declare (indent defun))
  (let ((old-dir (gensym))
        (retval (gensym)))
    `(let ((,old-dir default-directory)
           (,retval))
       (condition-case err
           (progn
             (cd ,dir)
             (setf ,retval ,@body)
             (cd ,old-dir)
             ,retval)
         (error
          (cd ,old-dir)
          (error err))))))

(defun org-fc/is-git-repo? ()
  "Return t is the current directory is a git repository."
  (= 0 (call-process-shell-command "git status")))

(defun org-fc/auto-commit-filing-cabinet (file-name org-file)
  "Automatically commit FILE-NAME and ORG-FILE to the filing cabinet."
  (org-fc/with-dir org-fc/filing-cabinet-directory
    (let* ((f (f-relative file-name default-directory))
           (o (f-relative org-file default-directory))
           (commit-msg (format org-fc/auto-commit-message f f)))
      (when (not (null org-fc/auto-commit-pre-command))
        (funcall org-fc/auto-commit-pre-command f))
      (org-fc/run-git-command "add" f)
      (org-fc/run-git-command "add" o)
      (org-fc/run-git-command "commit" "-m" commit-msg)
      (when (not (null org-fc/auto-commit-post-command))
        (funcall org-fc/auto-commit-post-command f)))))

(provide 'org-filing-cabinet-auto-commit)
;;; org-filing-cabinet-auto-commit.el ends here
