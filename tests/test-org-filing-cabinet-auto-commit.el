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
    (should (= (org-fc/run-git-command "add" "foo.txt") 0))))

(provide 'test-org-filing-cabinet-auto-commit)
;;; test-org-filing-cabinet-auto-commit ends here
