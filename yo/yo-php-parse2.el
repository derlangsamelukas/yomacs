;;; yo-php-parse2.el -*- lexical-binding: t -*-

(defvar yo-php-proc nil)

(defun yo-php-run-parser ()
  (let* ((stdout (generate-new-buffer "*yo php proc*"))
         (stderr (generate-new-buffer "*yo php proc error*"))
         (process (make-process
                  :name "yo php proc"
                  :buffer stdout
                  :stderr stderr
                  :command (list "/usr/bin/php"
                                 (yo-pathifism "/home"
                                               (user-login-name)
                                               "/Programming/html/php-ast/run.php")))))
    (buffer-disable-undo stdout)
    (buffer-disable-undo stderr)
    ;; (set-process-filter process 'ignore)
    (setf yo-php-proc process)))

(defun yo-php-parse-something (string cc)
  (let ((string string;; (string-trim (substring-no-properties string))
                ))
    (when (string-empty-p string)
      (error "string is empty"))
    (send-string yo-php-proc (format "%d\n" (string-bytes string)))
    (send-string yo-php-proc string)
    (funcall cc nil t)))
