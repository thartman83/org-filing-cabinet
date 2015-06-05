;;; org-filing-cabinet-dir-utils --- Directory Utilities for org-filing-cabinet
;;; Commentary:
;;; Code:

(require 'f)

(defun org-fc/get-new-dir-name (path)
  "Return an unused random directory name in PATH."
  (let ((rand-str (org-fc/get-random-string 6)))
    (if (not (f-directory? (f-join path rand-str)))
        (f-join path rand-str)
      (org-fc/get-new-dir-name path))))

(defmacro org-fc/with-temp-dir (root-dir &rest body)
  "Create a temporary directory in ROOT-DIR and execute BODY in pwd.
Removes directory and its contents at the end of execution.
Returns the value of body."
  (declare (indent defun))
  (let ((olddir default-directory)
        (dir (org-fc/get-new-dir-name root-dir)))
    `(unwind-protect
         (progn
           (make-directory ,dir)
           (cd ,dir)
           ,@body)
       (progn (cd ,olddir)
              (when (file-exists-p ,dir)
               (delete-directory ,dir t))))))

(defun org-fc/get-random-string (length)
  "Return a random string of letters and number of size LENGTH."
  (let ((chars "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (if (= length 1)
        (string (elt chars (random (length chars))))
      (concat (string (elt chars (random (length chars))))
              (org-fc/get-random-string (1- length))))))

(provide 'org-filing-cabinet-dir-utils)

;;; org-filing-cabinet-dir-utils.el ends here
