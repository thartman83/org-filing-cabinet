;;; test-org-filing-cabinet-auto-commit --- Tests for org-fc auto commit
;;; Commentary:
;;; Code:

(require 'org-filing-cabinet-dir-utils)

(ert-deftest good-repo-test ()
  (org-fc/with-temp-dir "/tmp"
    (call-process-shell-command "git init")
    (should (org-fc/is-git-repo?))))

(ert-deftest not-a-repo-test ()
    (org-fc/with-temp-dir "/tmp"
      (should (not (org-fc/is-git-repo?)))))

(ert-deftest org-fc/run-git-command-test ()
  (org-fc/with-temp-dir "/tmp"
    (should-not (org-fc/is-git-repo?))
    (should (= (org-fc/run-git-command "init") 0))
    (should (org-fc/is-git-repo?))
    (f-touch "foo.txt")
    (should (string= "??" (org-fc/git-file-status "foo.txt")))
    (should (= (org-fc/run-git-command "add" "foo.txt") 0))
    (should (string= "A " (org-fc/git-file-status "foo.txt")))
    (f-write-text "foobar" 'utf-8 "foo.txt")
    (should (string= "AM" (org-fc/git-file-status "foo.txt")))
    (should (= (org-fc/run-git-command "add" "foo.txt") 0))
    (should (string= "A " (org-fc/git-file-status "foo.txt")))
    (should (= (org-fc/run-git-command "commit" "-m" "'foo'") 0))
    (should (string= "" (org-fc/git-file-status "foo.txt")))))

(ert-deftest org-fc/auto-commit-filing-cabinet-test ()
  (org-fc/with-temp-dir "/tmp"
    (org-fc/run-git-command "init")
    (f-touch "new-file.pdf")
    (f-touch "filing-cabinet.org")
    (let ((cur default-directory))
      (cd "~/")
      (org-fc/auto-commit-filing-cabinet (f-join cur "new-file.pdf")
                                         (f-join cur "filing-cabinet.org"))
      (cd cur))
    (should (string= (org-fc/git-file-status "filing-cabinet.org") ""))
    (should (string= (org-fc/git-file-status "new-file.pdf") ""))))

(ert-deftest org-fc/with-dir-test ()
  (let ((cur-dir default-directory))
    (org-fc/with-dir "/tmp"
      (message "HODOR!"))
    (should (string= cur-dir default-directory))))

(defun org-fc/git-file-status (file-name)
  "Return the short form status of FILE-NAME."
  (let ((status (shell-command-to-string (format "git status -s %s" file-name))))
    (if (< (length status) 2) "" (substring status 0 2))))

(provide 'test-org-filing-cabinet-auto-commit)
;;; test-org-filing-cabinet-auto-commit ends here
