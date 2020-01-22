
(defun yo-php-goto-parameters-of-defun (&optional point)
  (let ((start (or point (point))))
    (if (re-search-backward "function\\([[:space:]]+[[:word:]_]+\\)?[[:space:]]*(" nil t)
        (let ((match-length (length (match-string 0))))
          (if (< start (save-excursion (yo-php--end-of-defun) (point)))
              (forward-char match-length)
            (yo-php-goto-parameters-of-defun start)))
      (beginning-of-buffer))))

(defun yo-php--end-of-defun ()
  (search-forward "{")
  (backward-char 1)
  (forward-sexp))

(defun yo-php-end-of-defun ()
  (yo-php-goto-parameters-of-defun)
  (yo-php--end-of-defun))

;; (delete-overlay x)
(defvar *yo-php-highlighted-vars* nil)
(defvar *yo-php-highlighted-vars-timer* nil)

(defun yo-php-highlight-vars ()
  (mapc 'delete-overlay *yo-php-highlighted-vars*)
  (setf *yo-php-highlighted-vars* nil)
  (save-excursion
    (let* ((start (1- (beginning-of-thing 'word)))
           (end (end-of-thing 'word))
           (word (buffer-substring-no-properties start end))
           (end-of-defun (save-excursion (yo-php-end-of-defun))))
      (yo-php-goto-parameters-of-defun)
      (while (search-forward word end-of-defun t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'face 'js2-highlight-vars-face)
          (push overlay *yo-php-highlighted-vars*))))))

(defun yo-php-highlight-vars-post-command-hook ()
  (mapc 'delete-overlay *yo-php-highlighted-vars*)
  (setf *yo-php-highlighted-vars* nil)
  (when *yo-php-highlighted-vars-timer*
    (cancel-timer *yo-php-highlighted-vars-timer*)
    (setf *yo-php-highlighted-vars-timer* nil))
  (when (and (thing-at-point 'word) (or (eq (face-at-point) 'php-variable-name) (eq (face-at-point) 'web-mode-variable-name-face)))
    (setf *yo-php-highlighted-vars-timer* (run-with-timer 0.5 nil 'yo-php-highlight-vars))))

(defvar *yo-php-projects* nil)

(defun yo-php-get-project (cc)
  (let* ((file (or (buffer-file-name) ""))
         (helper (lambda (list)
                   (when list
                     (if (string-match-p (concat "^" (regexp-quote (caar list))) file)
                         (funcall cc (car list))
                       (funcall helper (cdr list)))))))
    (funcall helper *yo-php-projects*)))

(defun yo-php-get-docroot (ccc)
  (yo-php-get-project (lambda (x) (funcall ccc (car x)))))

(defmacro yo-php-with-docroot (name &rest body)
  `(yo-php-get-docroot (lambda (,name) ,@body)))

(defun yo-php-find-all-src-files ()
  (yo-php-with-docroot
   docroot
   (split-string (shell-command-to-string (concat "find " (shell-quote-argument docroot) " -type f \\( -not -wholename '*/.*' -and -name '*.php' \\)")) "\n" t)))

(defun yo-php--find-match (regexp match-number)
  (save-excursion
    (let ((list))
      (while (re-search-forward regexp nil t)
        (push (match-string match-number) list))
      list)))

(defun yo-php--parse-files ()
  (cl-reduce
   (lambda (map file)
     (with-temp-buffer
       (insert-file file)
       (goto-char (point-min))
       (let ((namespace (car (yo-php--find-match "^[[:space:]]*namespace[[:space:]]+\\([^[:space:]]+\\)[[:space:]]*;" 1)))
             (class (yo-php--find-match "^[[:space:]]*[[:word:]_]*[[:space:]]*class[[:space:]]+\\([^[:space:]{]+\\)" 1))
             (interface (yo-php--find-match "^[[:space:]]*interface[[:space:]]+\\([^[:space:]{]+\\)" 1))
             (public (yo-php--find-match "^[[:space:]]*public[[:space:]]+function[[:space:]]+\\([[:word:]_]+\\)" 1))
             (protected (yo-php--find-match "^[[:space:]]*protected[[:space:]]+function[[:space:]]+\\([[:word:]_]+\\)" 1))
             (private (yo-php--find-match "^[[:space:]]*private[[:space:]]+function[[:space:]]+\\([[:word:]_]+\\)" 1)))
         (let ((full-namespace (concat namespace "\\" (or (car class) (car interface)))))
           ;; (with-current-buffer "*hej*"
           ;;   (insert full-namespace ": " file "\n"))
           (puthash full-namespace
                    `((namespace ,full-namespace)
                      (class ,@class)
                      (interface ,@interface)
                      (public ,@public)
                      (protected ,@protected)
                      (private ,@private)
                      (file ,file))
                    map)
           map))))
   (yo-php-find-all-src-files)
   :initial-value (make-hash-table :test 'equal)))

(defun yo-php-parse-files ()
  (yo-php-get-project
   (lambda (project)
     (let ((parsed (cadr project)))
       (if parsed
           parsed
         (let ((parsed (yo-php--parse-files)))
           (setf (cdr project) (list parsed))
           parsed))))))

(defun yo-php-get-namespace-at-point (&optional point)
  (save-excursion
    (goto-char (or point (point)))
    (buffer-substring-no-properties
     (save-excursion (1+ (re-search-backward "[^[:word:]\\_]")))
     (save-excursion (1- (re-search-forward "[^[:word:]\\_]"))))))

(defun yo-php-goto-src ()
  (let* ((parsed (yo-php-parse-files))
         (word (yo-php-get-namespace-at-point))
         (def (gethash word parsed)))
    (if def
        (find-file (cadr (assoc 'file def)))
      (let ((finish 'ignore))
        (save-excursion
          (let ((first-part (substring word 0 (or (cl-position ?\\ word) (length word))))
                (last-part (substring word (or (cl-position ?\\ word) (length word)) (length word))))
            (goto-char (point-min))
            (setf
             finish
             (if (re-search-forward (concat "^[[:space:]]*use[[:space:]]+\\(.*\\)" (regexp-quote first-part) "[[:space:]]*;") nil t)
                 (let* ((namespace (concat (yo-php-get-namespace-at-point (match-beginning 1)) last-part))
                        (def (gethash namespace parsed)))
                   (if def
                       `(lambda () (find-file ,(cadr (assoc 'file def))))
                     '(lambda () (message "Sorry..."))))
               '(lambda () (message "Sorry..."))))))
        (funcall finish)))))

;; (add-hook 'post-command-hook 'yo-php-highlight-vars-post-command-hook nil t)
;; (remove-hook 'post-command-hook 'yo-php-highlight-vars-post-command-hook t)
