;; -*- lexical-binding: t -*-

(setq yo-php-todo-list nil)
;; (while (> (length yo-php-todo-list) 100)
;;   (pop yo-php-todo-list))

;; (defun yo-php-todo-list-worker (&rest ignored)
;;   (when yo-php-todo-list
;;     (let ((file (pop yo-php-todo-list))
;;           (buffer (generate-new-buffer "*yo php temp buffer*")))
;;       (with-current-buffer buffer
;;         (insert-file file)
;;         (yo-php-ast--parse
;;          (lambda (error)
;;            (kill-buffer buffer)
;;            (message "error in file: %s" file))
;;          (lambda (dom)
;;            (print "aaaaaa")
;;            (kill-buffer buffer)
;;            (yo-php-todo-list-worker)))))))

(defun yo-load-screen-log (string string*)
  (let ((buffer (current-buffer)))
    (switch-to-buffer "*load screen*")
    (goto-char (point-max))
    (insert string string* "\n")
    (switch-to-buffer buffer)))

(defun yo-php-todo-list-worker (&rest ignored)
  (run-at-time
   0.5
   nil
   (lambda ()
     (when yo-php-todo-list
       (let ((file (pop yo-php-todo-list)))
         (yo-load-screen-log "started: " file)
         ;; (message "::%s" file)
         (yo-php-parse-something
          (with-temp-buffer
            (insert-file file)
            (buffer-string))
          (lambda (buffer success)
            (if success
                (progn
                  (yo-load-screen-log "finished: " file)
                  (yo-php-todo-list-worker))
              (yo-load-screen-log "error parsing file: " file)
              nil))))))))

(defun yo-php-create-index (dir)
  (setf
   yo-php-todo-list
   (cl-reduce
    (lambda (files folder)
      (append files (directory-files-recursively (car folder) "^[^.].*\\.php$")))
    (cl-remove-if-not
     (lambda (file)
       (and (file-attribute-type (cdr file))
            (not (string-match-p "^\\." (car file)))))
     (directory-files-and-attributes dir))
    :initial-value nil))
  ;;(yo-php-todo-list-worker)
  )

(defun yo-php-project-init (dir)
  (let ((php-index-files-dir (yo-pathifism user-emacs-directory "php-index-files")))
    (unless (file-exists-p php-index-files-dir)
      (dired-create-directory php-index-files-dir))
    (unless (file-directory-p php-index-files-dir)
      (error (concat "why is " php-index-files-dir " not a directory...")))
    (let ((index-filename
           (concat
            (mapcar
             (lambda (char)
               (if (char-equal char ?/)
                   ?-
                 char))
             dir)
            ".lisp")))
      )))

(defun yo-php-read-index-file (filename)
  (json-read-file filename))

(provide 'yo-php-indexer)
