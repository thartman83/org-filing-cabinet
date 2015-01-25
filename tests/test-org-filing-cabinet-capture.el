;;; test-org-filing-cabinet-capture.el --- tests for org-filing-cabinet-capture
;;; Commentary:
;;; Code:

(require 'ert)
(require 'noflet)
(require 'org-filing-cabinet-capture)
(require 'org-filing-cabinet-dir-utils)

(ert-deftest test-org-fc/get-create-category ()
  ; test against a non-existant org file, result should be an error
  (org-fc/with-temp-dir "/tmp"
    (let ((org-fc/filing-cabinet-directory default-directory)
          (org-fc/org-file "test.org"))
      (should (string= (cadr (should-error (org-fc/get-create-category)))
                       (format "Could not find filing cabinet org file `%s'"
                               (f-join default-directory "test.org"))))))
  ; test against a blank org file, result should be a new category
  ; with the current time-frame
  (org-fc/with-temp-dir "/tmp"
    (let ((org-fc/filing-cabinet-directory default-directory)
          (org-fc/org-file "test.org")
          (time-frame (format-time-string "%Y-%m")))
      (f-touch (f-join default-directory "test.org"))
      (should (string= (org-fc/get-create-category) time-frame))
      (should (string= (format "#+CATEGORY: %s\n* %s\n" time-frame time-frame)
                       (f-read-text (f-join default-directory "test.org"))))
  ; call function again to make sure the file is unmodified
      (let ((text (f-read-text (f-join default-directory "test.org"))))
        (should (string= (org-fc/get-create-category) time-frame))
        (should (string= text (f-read-text (f-join default-directory "test.org")))))))
  ; test against an existing org file with previous time-frames
  (org-fc/with-temp-dir "/tmp"
     (let ((org-fc/filing-cabinet-directory default-directory)
           (org-fc/org-file "test.org")
           (time-frame (format-time-string "%Y-%m"))
           (existing-text "#+CATEGORY: 2014-12\n* 2014-12\n ** something"))
       (f-write-text existing-text
                     'utf-8 (f-join default-directory "test.org"))
       (should (string= (org-fc/get-create-category) time-frame))
       (should (string= (format "%s#+CATEGORY: %s\n* %s\n" existing-text time-frame
                                time-frame)
                        (f-read-text (f-join default-directory "test.org")))))))

(ert-deftest test-org-fc/current-filing-cabinet-dir ()
  ; test when the filing cabinet root directory doesn't exist
  (org-fc/with-temp-dir "/tmp"
   (let ((org-fc/filing-cabinet-directory "/path/to/nowhere/")
         (time-frame (format-time-string "%Y-%m")))
     (should (string= (cadr (should-error (org-fc/current-filing-cabinet-dir)))
                      (format "Unable to locate filing cabinet directory `%s'"
                              org-fc/filing-cabinet-directory)))))
  ; test when time-frame folder doesn't exist
  (org-fc/with-temp-dir "/tmp"
     (let ((org-fc/filing-cabinet-directory default-directory)
           (time-frame (format-time-string "%Y-%m")))
       (should-not (f-directory? (f-join org-fc/filing-cabinet-directory time-frame)))
       (should (f-same? (f-join org-fc/filing-cabinet-directory time-frame)
                        (org-fc/current-filing-cabinet-dir)))
       (should (f-directory? (f-join org-fc/filing-cabinet-directory time-frame)))))
  ; test when time-frame folder already exists
  (org-fc/with-temp-dir "/tmp"
     (let ((org-fc/filing-cabinet-directory default-directory)
           (time-frame (format-time-string "%Y-%m")))
       (f-mkdir "%Y-%m")
       (should (f-same? (f-join org-fc/filing-cabinet-directory time-frame)
                        (org-fc/current-filing-cabinet-dir)))))
  ; test when the root filing directory is not writable
  ; in any sane system / is not writable by USER
  (let ((org-fc/filing-cabinet-directory (f-root)))
    (should-error (org-fc/current-filing-cabinet-dir))))

(ert-deftest test-org-fc/capture-scan-file ()
  ; Test when org-fc/scan-file doesn't create the file `FILE-NAME'
  (let ((file-name "foo.pdf"))
    (noflet ((org-fc/capture-file (file-name)
                                  file-name)
             (org-fc/scan-file (file-name)
                               file-name))
      (should (string= (cadr (should-error (org-fc/capture-scan-file file-name)))
                       (format "File `%s' does not exist, can not be filed"
                               file-name))))))

;; org-fc/parse-whose tests

(ert-deftest test-org-fc/parse-whose ()
  (let ((org-fc/whose "ME"))
    (should (string= (org-fc/parse-whose) org-fc/whose)))
  (let ((org-fc/whose "ME,SOMEONE,ELSE,ANOTHER"))
    (should (string= (org-fc/parse-whose) org-fc/whose)))
  (let ((org-fc/whose "ME|SOMEONELSE"))
    (should (string= (org-fc/parse-whose)
                     (concat "%^{Whose file? |" org-fc/whose "}")))))

;; org-fc/capture-file tests

(ert-deftest test-capturing-non-existant-file ()
    ;; Tests for capturing a file that doesn't exist
    (let ((file-name "nowhere.pdf"))
      (noflet ((org-capture (goto keys) nil)
               (y-or-n-p (description) t))
        (should (string= (cadr (should-error (org-fc/capture-file file-name)))
                         (format (concat "The file `%s' does not exist "
                                         "and can't be added to the filing "
                                         "cabinet")
                                 file-name))))))

(ert-deftest capture-existing-file-test ()
  (noflet ((org-capture (goto keys) t)
           (y-or-n-p (description) t))
    (org-fc/with-temp-dir "/tmp"
       (let ((org-fc/filing-cabinet-directory
              (f-join default-directory "filing-cabinet"))
             (test-file (f-join default-directory "test.pdf")))
         (f-mkdir org-fc/filing-cabinet-directory)
         (f-touch (f-join org-fc/filing-cabinet-directory org-fc/org-file))
         (f-touch test-file)
         (should (org-fc/capture-file test-file))
         (should-error (org-fc/capture-file test-file))))))

(ert-deftest capture-and-move-test ()
  (noflet ((org-capture (goto keys) t)
           (y-or-n-p (description) t))
    (org-fc/with-temp-dir "/tmp"
       (let ((org-fc/filing-cabinet-directory
              (f-join default-directory "filing-cabinet"))
             (test-file "test.pdf"))
         (f-mkdir org-fc/filing-cabinet-directory)
         (f-touch (f-join org-fc/filing-cabinet-directory org-fc/org-file))
         (f-touch test-file)
         (should (org-fc/capture-file test-file))
         (should (f-exists? (f-join (org-fc/current-filing-cabinet-dir)
                                    test-file)))
         (should (not (f-exists? (f-join default-directory test-file))))))))

(ert-deftest capture-and-copy-test ()
  (noflet ((org-capture (goto keys) t)
           (y-or-n-p (description) nil))
    (org-fc/with-temp-dir "/tmp"
       (let ((org-fc/filing-cabinet-directory
              (f-join default-directory "filing-cabinet"))
             (test-file "test.pdf"))
         (f-mkdir org-fc/filing-cabinet-directory)
         (f-touch (f-join org-fc/filing-cabinet-directory org-fc/org-file))
         (f-touch test-file)
         (should (org-fc/capture-file test-file))
         (should (f-exists? (f-join (org-fc/current-filing-cabinet-dir)
                                    test-file)))
         (should (f-exists? (f-join default-directory test-file)))))))

(ert-deftest capture-kill-ring-test ()
  (noflet ((org-capture (goto keys) t)
           (y-or-n-p (description) t))
    (org-fc/with-temp-dir "/tmp"
       (let ((org-fc/filing-cabinet-directory
              (f-join default-directory "filing-cabinet"))
             (test-file "tests.pdf"))
         (f-mkdir org-fc/filing-cabinet-directory)
         (f-touch (f-join org-fc/filing-cabinet-directory org-fc/org-file))
         (f-touch test-file)
         (should (org-fc/capture-file test-file))
         (should (string= (car kill-ring)
                          (f-join (org-fc/current-filing-cabinet-dir)
                                  test-file)))))))

(provide 'test-org-filing-cabinet-capture)
;;; test-org-filing-cabinet-capture.el ends here
