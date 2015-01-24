;;; test-org-filing-cabinet-capture.el --- testing functions for org-filing-cabinet-capture
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
           (existing-text "#+CATEGORY: 2014-12\n* 2014-12\n ** something\n ** something else\n"))
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
                        (org-fc/current-filing-cabinet-dir))))))

(ert-deftest test-org-fc/capture-scan-file ()
  ; Test when org-fc/scan-file doesn't create the file `FILE-NAME`
  (let ((file-name "foo.pdf"))
    (noflet ((org-fc/capture-file (file-name)
                                  file-name)
             (org-fc/scan-file (file-name)
                               nil))
      (should (string= (cadr (should-error (org-fc/capture-scan-file "foo.pdf")))
                       (format "File `%s' does not exist, can not be filed"
                               file-name)))))
  ;; 
  (noflet ((org-fc/capture-file (file-name)
                                file-name)
           (org-fc/scan-file (file-name)
                             (f-touch file-name)))
    ))

(provide 'test-org-filing-cabinet-capture)
;;; test-org-filing-cabinet-capture.el ends here
