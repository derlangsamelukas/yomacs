(defvar *yo-php-parse-process* nil) ; process
(defvar *yo-php-parse-process-read* nil) ; number
(defvar *yo-php-parse-finished* nil) ; function list
(defvar *yo-php-parse-error* nil) ; nil | t

(defun yo-php-parse-filter (process string)
  ;; (with-current-buffer "*aaaa*" (goto-char (point-max)) (insert "input arrived:\n" string "\n------------\n"))
  (if (not *yo-php-parse-process-read*)
      (let* ((index-of-newline (cl-position 10 string)) ; it is expected that there is always a newline
             (first-line (substring string 0 index-of-newline)))
        (setf *yo-php-parse-process-read* (string-to-number first-line))
        ;; (message "READING: %d" *yo-php-parse-process-read*)
        (setf *yo-php-parse-error* (not (string-match-p " ok$" first-line)))
        (with-current-buffer (process-buffer process)
          (setf (buffer-string) "")
          (when (< (1+ index-of-newline) (length string))
            (yo-php-parse-filter process (substring string (1+ index-of-newline))))))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert string)
      (setf *yo-php-parse-process-read*
            (- *yo-php-parse-process-read*
               (length string)))
      (when (<= *yo-php-parse-process-read* 0)
        (progn
          (let ((after-cleanup (or (funcall (pop *yo-php-parse-finished*)
                                            (current-buffer)
                                            (not *yo-php-parse-error*))
                                   'ignore)))
            (setf *yo-php-parse-error* nil)
            (setf *yo-php-parse-process-read* nil)
            (setf (buffer-string) "")
            (funcall after-cleanup)))))))

(defun yo-php-parse-error-arrived (&rest ignored)
  (error "stderr got filled"))

(defun yo-php-start-parse-process ()
  (let* ((error-buffer (generate-new-buffer "*yo php process stderr*"))
         (process
          (make-process
           :name "yo php process"
           :buffer "*yo php process*"
           :command (list "/usr/bin/php" (concat "/home/" (user-login-name) "/Programming/html/php-ast/run.php"))
           :stderr error-buffer)))
    (buffer-disable-undo (process-buffer process))
    ;; (with-current-buffer error-buffer
    ;;   (add-hook 'after-change-functions 'yo-php-parse-error-arrived nil t))
    (set-process-filter process 'yo-php-parse-filter)
    (setf *yo-php-parse-process* process)))

(defun yo-php-parse-something (string cc)
  (let ((string string;; (string-trim (substring-no-properties string))
         ))
    (when (string-empty-p string)
      (error "string is empty"))
    (setf *yo-php-parse-finished* (append *yo-php-parse-finished* (list cc)))
    (send-string *yo-php-parse-process* (format "%d\n" (string-bytes (buffer-string))))
    (send-string *yo-php-parse-process* (buffer-string))
    ;; (process-send-string *yo-php-parse-process* (format "%d\n" (string-bytes string)))
    ;; (process-send-string *yo-php-parse-process* string)
    ;; (process-send-eof *yo-php-parse-process*)
    ;; (message "SEND: %d (%s)" (length string) *yo-php-parse-process-read*)
    
    ;; (process-send-eof *yo-php-parse-process*)
    ))

;; (yo-php-start-parse-process)
;; (yo-php-parse-something "<?php echo 'hello';"
;;                         '(lambda (buffer ok)
;;                            (message "was ok: %s" ok ;; (with-current-buffer buffer (buffer-string))
;;                             )))

(provide 'yo-php-parse)
