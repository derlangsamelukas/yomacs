;; -*- lexical-binding: t -*-

(defun yo-php-ast-clip-dom (php-buffer dom)
  (let ((walk
         (lambda (walk)
           (lambda (node)
             (let ((start (1+ (string-to-number (dom-attr node 'start)))) ; 1+ because buffer index starts at 1 not at 0
                   (end (+ 2 (string-to-number (dom-attr node 'end)))))
               (put-text-property start end 'yo-php-node (cons node (get-text-property start 'yo-php-node)))
               (mapc (funcall walk walk) (dom-children node)))))))
    (with-current-buffer php-buffer
      (put-text-property (point-min) (point-max) 'yo-php-node nil)
      (mapc (funcall walk walk) (dom-children dom)))))

(defun yo-create-or-clear-buffer (buffer-or-name)
  (with-current-buffer (get-buffer-create buffer-or-name)
    (setf (buffer-string) "")
    (current-buffer)))

(defun yo-php-parse ()
  (unless (yo-php-lint)
    (error "file contains sytax errors"))
  (let ((php-buffer (current-buffer))
        (content (buffer-substring-no-properties (point-min) (point-max)))
        (buffer (yo-create-or-clear-buffer "*yo php output buffer*")))
    (let ((process (start-process
                    "yo php process"
                    "*yo php process*"
                    "/usr/bin/php"
                    (concat "/home/" (user-login-name) "/Programming/html/php-ast/run.php"))))
      (set-process-filter
       process
       (lambda (process string)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert string))))
      (set-process-sentinel
       process
       (lambda (process message)
         (kill-buffer (process-buffer process))
         (let ((dom (with-current-buffer buffer
                      (libxml-parse-xml-region (point-min) (point-max)))))
           (kill-buffer buffer)
           (yo-php-ast-clip-dom php-buffer dom))))
      (send-string process (concat content))
      (process-send-eof process))))

(defun yo-php-ast-at-point (&optional point)
  (get-text-property (or point (point)) 'yo-php-node))

(defun yo-php-ast--goto-x-var (cc)
  (let ((ast (yo-php-ast-at-point)))
    (unless ast
      (error "not in a php node"))
    (unless (eq 'Expr_Variable (dom-tag (car ast)))
      (error "not in a php variable"))
    (let ((name (dom-attr (car ast) 'name))
          (parent-scope (assoc 'Stmt_Function ast)))
      (unless parent-scope
        (error "there is no outer function"))
      (funcall
       cc
       (car ast)
       (cl-remove-if-not
        (lambda (node) (eq 'Expr_Variable (dom-tag node)))
        (dom-elements parent-scope 'name name))))))

(defun yo-php-ast-goto-prev-var ()
  (interactive "")
  (yo-php-ast--goto-x-var
   (lambda (node variables)
     (let ((prev-node nil))
       (while (and (not prev-node) (cdr variables))
         (when (eq node (cadr variables))
           (setq prev-node (car variables)))
         (pop variables))
       (if prev-node
           (goto-char (1+ (string-to-number (dom-attr prev-node 'start))))
         (message "no previous variable"))))))

(defun yo-php-ast-goto-next-var ()
  (interactive "")
  (yo-php-ast--goto-x-var
   (lambda (node variables)
     (if-let ((next-node
               (cl-loop
                while variables
                when (eq node (car variables))
                return (cadr variables)
                do (pop variables))))
         (goto-char (1+ (string-to-number (dom-attr next-node 'start))))
       (message "no next variable")))))

(provide 'yo-php-ast)
