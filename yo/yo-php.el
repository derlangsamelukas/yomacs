(defun yo-php-goto-parameters-of-defun (&optional point)
  (interactive)
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
  (interactive)
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

(defun yo-highlight-line (line)
  (let ((inhibit-modification-hooks t))
    (make-face 'temp-face2)
    (set-face-underline 'temp-face2 "red")
    (save-excursion
      (goto-line line)
      (put-text-property (line-beginning-position) (line-end-position) 'face 'temp-face2))))

(defun yo-php-lint ()
  (interactive "")
  (save-buffer)
  (let ((output (shell-command-to-string (concat "php -l " (buffer-file-name)))))
    (unless (string-match-p "^No syntax errors detected in" output)
      (let* ((first-line (car (split-string output "\n")))
             (line (string-to-number (car (reverse (split-string first-line " "))))))
        (yo-highlight-line line)
        (message first-line)))))

(defun yo-add-simple-doc-block ()
  (beginning-of-thing 'word)
  (delete-word 1)
  (let ((start (point))
        (point (progn (insert "/**\n* ") (point-marker))))
    (insert "\n*/")
    (indent-region start (point))
    (goto-char point)))

(defun yo-add-doc-block ()
  (interactive "")
  (save-excursion
    (search-forward "(")
    (backward-char 1)
    (let* ((start (point))
           (end (save-excursion (forward-sexp) (point)))
           (arguments (mapcar (lambda (pair) (split-string pair " " t))
                              (split-string (buffer-substring-no-properties (1+ start) (1- end)) "," t))))
      (beginning-of-line)
      (newline-and-indent)
      (next-line -1)
      (print arguments)
      (let ((start (point)))
        (insert "/**\n*\n")
        (cl-loop for pair in arguments
                 do (if (cdr pair) (insert "* @param " (car pair) " " (cadr pair) "\n") (insert "* @param mixed " (car pair) "\n")))
        (insert "*/")
        (indent-region start end)))))

(defun yo-php-find-docroot ()
  (shell-command-to-string "/home/lscharmer/Programming/bash/find-docroot.sh"))

(defun yo-insert-namespace (classname)
  (interactive "sClassname: ")
  (let* ((command (concat
                   "find "
                   (shell-quote-argument (string-trim (yo-php-find-docroot)))
                   " -name "
                   (shell-quote-argument (string-trim (concat classname ".php")))))
         (_ (print command))
         (result (string-trim
                  (shell-command-to-string command)))
         (grep-results (cl-reduce (lambda (matches result)
                                 (let ((grep-result (or (shell-command-to-string (concat "grep namespace " (shell-quote-argument result))) "")))
                                   (if (string-match "namespace \\(.+\\);" grep-result)
                                       (cons (match-string 1 grep-result) matches)
                                     matches)))
                               (split-string result "\n")
                               :initial-value nil)))
    (if grep-results
        (progn  (insert "\\" (if (cdr grep-results) (completing-read "Select namespace: " grep-results nil t) (car grep-results)) "\\" classname) t)
      (progn (message "no namespace found for class: '%s'" classname) nil))))

(defun yo-correct-namespace ()
  (interactive "")
  (let* ((start (save-excursion (beginning-of-thing 'word) (point)))
         (end (save-excursion (end-of-thing 'word) (point)))
         (classname (buffer-substring-no-properties start end)))
    (delete-region start end)
    (unless (yo-insert-namespace classname)
      (insert classname))))

(defun yo-add-namespace ()
  (interactive "")
  (save-excursion
    (let ((word (word-at-point)))
      (goto-char (point-min))
      (re-search-forward "^class ")
      (unless (re-search-backward "^use " nil t)
        (re-search-backward "^namespace ")
        (end-of-line)
        (insert "\n"))
      (next-line)
      (beginning-of-line)
      (insert "use " word ";\n")
      (backward-char 2)
      (yo-correct-namespace)
      (beginning-of-line)
      (forward-char 4)
      (delete-char 1)
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (line-pos (line-number-at-pos)))
        (if (search-backward line nil t)
            (progn (goto-line line-pos)
                   (beginning-of-line)
                   (delete-char (1+ (length line)))
                   (message "namespace already present"))
          (message "added namespace"))))))

(defun yo-php-const-to-dollar ()
  (interactive)
  (when (looking-back "[^[:word:]_]\\(const \\)")
    (delete-region (match-beginning 1) (match-end 1))
    (insert "$")))

(provide 'yo-php)

