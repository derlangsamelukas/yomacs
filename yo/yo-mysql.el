
(defvar *yo-mysql-process* nil)
(defvar *yo-mysql--process-position* nil)
(defvar *yo-mysql-process-last-result* nil)
(defvar *yo-mysql--continue-with* 'ignore)

(defun yo-php-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (when (looking-back "^MariaDB \\[([[:word:]_]+)\\]>[[:space:]]*")
      (let ((rows nil))
        (goto-char *yo-mysql--process-position*)
        (while (< (point) (point-max))
          (push nil rows)
          (while (re-search-forward "|[[:space:]]+\\([^|]+\\)[[:space:]]+|" (line-end-position) t)
            (push (string-trim (match-string 1)) (car rows))
            (backward-char 1))
          (unless (car rows)
            (pop rows))
          (forward-line))
        (goto-char (point-max))
        (let ((rows (reverse rows)))
          (setf *yo-mysql--process-position* (point-max))
          (setf *yo-mysql-process-last-result*
                (mapcar
                 (lambda (row) (cl-mapcar 'cons (car rows) row))
                 (cdr rows)))
          (funcall *yo-mysql--continue-with* *yo-mysql-process-last-result*)
          (setf *yo-mysql--continue-with* 'ignore))))))

(defun yo-mysql-show-table (database table)
  (yo-mysql-send-query
   (concat "desc " database "." table)
   `(lambda (result)
      (switch-to-buffer (concat "*yo mysql table " ,database "." ,table))
      (let ((inhibit-read-only t))
        (read-only-mode 1)
        (setf (buffer-string) "")
        (mapc
         (lambda (pair)
           (insert (car pair))
           (let ((max-line (- (line-end-position) (line-beginning-position))))
             (insert "\n")
             (mapc
              (lambda (row)
                (insert (cdr (assoc (car pair) row)))
                (setf max-line (max max-line (- (line-end-position) (line-beginning-position))))
                (insert "\n"))
              result)
             (while (> (point) (point-min))
               (beginning-of-line)
               (previous-line)
               (insert (spaces-string (- max-line (- (line-end-position) (line-beginning-position))))))))
         (car result))
        (goto-char (point-min))))))

(defun yo-mysql-show-tables (database)
  (yo-mysql-send-query
   (concat "show tables from " database)
   `(lambda (result)
      (switch-to-buffer "*yo mysql tables*")
      (let ((inhibit-read-only t))
        (read-only-mode 1)
        (setf (buffer-string) "")
        (mapc
         (lambda (row) (yo-insert-button (cdr (assoc (concat "Tables_in_" ,database) row)) (list 'lambda nil '(interactive) (list 'yo-mysql-show-table ,database (cdr (assoc (concat "Tables_in_" ,database) row))))) (insert "\n"))
         result)
        (goto-char (point-min))))))

(defun yo-mysql-show-databases ()
  (yo-mysql-send-query
   "show schemas"
   '(lambda (result)
      (switch-to-buffer "*yo mysql databases*")
      (let ((inhibit-read-only t))
        (read-only-mode 1)
        (setf (buffer-string) "")
        (mapc
         (lambda (row)
           (yo-insert-button
            (cdr (assoc "Database" row))
            `(lambda () (interactive) (yo-mysql-show-tables ,(cdr (assoc "Database" row)))))
           (insert "\n"))
         result)
        (goto-char (point-min))))))

(defun yo-mysql-connect ()
  (interactive)
  (let ((user (read-from-minibuffer "User: "))
        (password (password-read "Password: "))
        (host (read-from-minibuffer "Host: " "localhost"))
        (database (read-from-minibuffer "Database: ")))
    (setf *yo-mysql-process*
          (apply
           'start-process
           "yo mysql process"
           "*yo mysql process*"
           "/usr/bin/mysql"
           "-u"
           (shell-quote-argument user)
           "-h"
           (shell-quote-argument host)
           (append
            (unless (string-empty-p password) '("-p"))
            (unless (string-empty-p database) (list (shell-quote-argument database))))))
    (set-process-filter
     *yo-mysql-process*
     `(lambda (process string)
        (unless (string-empty-p ,password)
          (send-string *yo-mysql-process* (concat ,password "\n")))
        (with-current-buffer (process-buffer process)
          (setf *yo-mysql--process-position* (point-max)))
        (set-process-filter *yo-mysql-process* 'yo-php-process-filter)))))

(defun yo-mysql-send-query (query cc)
  (send-string *yo-mysql-process* (concat query ";\n"))
  (setf *yo-mysql--continue-with* cc))

;; (send-string x "root\n")
;; (send-string *yo-mysql-process* "show schemas;\n")
;; (send-string *yo-mysql-process* "show tables from testdb;\n")
;; (send-string *yo-mysql-process* "desc testdb.hui;\n")
;; (send-string *yo-mysql-process* "exit\n")

;; (length *yo-mysql-process-last-result*)

(provide 'yo-mysql)
