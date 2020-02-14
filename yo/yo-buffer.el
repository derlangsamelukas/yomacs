;;; yo-buffer.el
;; this file contains all functions related to buffers

(defun yo-kill-all-buffers-under-dir (directory)
  "Kills all buffers that have a `default-directory' equal or under the given directory"
  (interactive)
  (cl-remove-if-not
   (lambda (buffer)
     (string-match-p
      (concat "^" directory)
      (regexp-quote
       (with-current-buffer buffer
         default-directory))))
   (buffer-list)))

(provide 'yo-buffer)
