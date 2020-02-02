;; -*- lexical-binding: t -*-

(require 'dom)

(defun yo-make-marker (position buffer)
  (let ((marker (make-marker)))
    (set-marker marker position buffer)
    marker))

(defun yo-php-ast-clip-dom (php-buffer dom)
  (let ((walk
         (lambda (walk)
           (lambda (node)
             (dom-set-attribute
              node
              'start
              (yo-make-marker
               (1+ (string-to-number (dom-attr node 'start))) ; 1+ because buffer index starts at 1 not at 0
               (current-buffer)))
             (dom-set-attribute
              node
              'end
              (yo-make-marker
               (+ 2 (string-to-number (dom-attr node 'end))) ; + 2 because buffer index starts at 1 not at 0
               (current-buffer)))
             (put-text-property
              (dom-attr node 'start)
              (min (dom-attr node 'end) (point-max))
              'yo-php-node
              (cons node (get-text-property (dom-attr node 'start) 'yo-php-node)))
             (mapc (funcall walk walk) (dom-children node))))))
    (with-current-buffer php-buffer
      (with-silent-modifications
        (put-text-property (point-min) (point-max) 'yo-php-node nil)
        (mapc (funcall walk walk) (dom-children dom))))))

(defun yo-create-or-clear-buffer (buffer-or-name)
  (with-current-buffer (get-buffer-create buffer-or-name)
    (setf (buffer-string) "")
    (current-buffer)))

(defun yo-php-ast--parse (error cc)
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
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
         (when (or (string-match-p "^finsished" message)
                   (string-match-p "^deleted" message)
                   (string-match-p "^exited" message)
                   (string-match-p "^failed with" message))
           (kill-buffer (process-buffer process))
           (if (= 0 (process-exit-status process))
               (let ((dom (with-current-buffer buffer
                            (libxml-parse-xml-region (point-min)
                                                     (point-max)))))
                 (kill-buffer buffer)
                 (funcall cc dom))
             (funcall error (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))
             (kill-buffer buffer)))))
      (send-string process (concat content))
      (process-send-eof process))))

(defvar *yo-php-ast-process-running* nil)

;; (defun yo-php-ast-parse (php-buffer)
;;   (unless *yo-php-ast-process-running*
;;     (with-current-buffer php-buffer
;;       (yo-php-ast--parse
;;        (lambda (error)
;;          (setf *yo-php-ast-process-runing* nil)
;;          (message "Syntax error: %s" error))
;;        (lambda (dom)
;;          (setf *yo-php-ast-process-runing* nil)
;;          (yo-php-ast-clip-dom php-buffer dom))))))

(defun yo-php-ast-parse (php-buffer)
  (unless *yo-php-ast-process-running*
    (yo-php-parse-something
     (with-current-buffer php-buffer (buffer-string))
     (lambda (buffer success)
        (setf *yo-php-ast-process-runing* nil)
        (with-current-buffer buffer
          (if success
              (yo-php-ast-clip-dom
               php-buffer
               (libxml-parse-xml-region
                (point-min)
                (point-max)))
            (message "Error: %s" (buffer-string))))))))

(defun yo-php-ast-at-point (&optional point)
  (get-text-property (or point (point)) 'yo-php-node))

(defun yo-php-ast--get-variable-or-error ()
  (let ((ast (yo-php-ast-at-point)))
    (unless ast
      (error "not in a php node"))
    (unless (eq 'Expr_Variable (dom-tag (car ast)))
      (error "not in a php variable"))
    ast))

(defun yo-php-ast--containing-scope (ast)
  (or (assoc 'Stmt_Function ast)
      (assoc 'Stmt_ClassMethod ast)
      (dom-node 'root `((start . ,(point-min)) (end . ,(point-max))))))

(defun yo-dom-filter (dom filter-fn)
  (let ((matches (cl-loop for child in (dom-children dom)
			              for matches = (and (not (stringp child))
					                         (yo-dom-filter child filter-fn))
			              when matches
			              append matches)))
    (if (funcall filter-fn dom)
	    (cons dom matches)
      matches)))

(defun yo-php-ast--with-variables-in-scope (cc)
  (let ((ast (yo-php-ast--get-variable-or-error)))
    (let ((name (dom-attr (car ast) 'name))
          (parent-scope (yo-php-ast--containing-scope ast)))
      (unless parent-scope
        (error "there is no outer function"))
      (funcall
       cc
       (car ast)
       (yo-dom-filter
        parent-scope
        (lambda (node)
          (and (eq (dom-tag node)
                   (dom-tag (car ast)))
               (string-equal (dom-attr node 'name) (dom-attr (car ast) 'name)))))))))

(defun yo-php-ast-goto-prev-var ()
  (interactive "")
  (yo-php-ast--with-variables-in-scope
   (lambda (node variables)
     (let ((prev-node nil))
       (while (and (not prev-node) (cdr variables))
         (when (eq node (cadr variables))
           (setq prev-node (car variables)))
         (pop variables))
       (if prev-node
           (goto-char (dom-attr prev-node 'start))
         (message "no previous variable"))))))

(defun yo-php-ast-goto-next-var ()
  (interactive "")
  (yo-php-ast--with-variables-in-scope
   (lambda (node variables)
     (if-let ((next-node
               (cl-loop
                while variables
                when (eq node (car variables))
                return (cadr variables)
                do (pop variables))))
         (goto-char (dom-attr next-node 'start))
       (message "no next variable")))))

(defun yo-php-ast--get-type-of-assignment (assignment-node)
  nil)

(defun yo-php-ast--get-type-of-param (variable param-node ast)
  (if (cdr (dom-children param-node))
    (cl-case (dom-tag (car (dom-children param-node)))
      (Name
       (dom-attr (car (dom-children param-node)) 'parts))
      (Identifier
       (dom-attr (car (dom-children param-node)) 'name)))
    (when (and (dom-children param-node)
               (string-match (concat "^[[:space:]]*\\*[[:space:]]*@param[[:space:]]+\\([[:word:]_]+\\)[[:space:]]+\\$"
                                     (regexp-quote (dom-attr variable 'name))
                                     "[[:space:]]*$")
                             (or (dom-attr (yo-php-ast--containing-scope ast) 'comment) "")))
      (match-string 1 (or (dom-attr (yo-php-ast--containing-scope ast) 'comment) "")))))

(defun yo-php-ast-get-type-of-variable-at-point ()
  (yo-php-ast--with-variables-in-scope
   (lambda (node variables)
     (let ((last-assignment (car variables)))
       (cl-loop
        for node* in variables
        when (eq node node*) return nil
        when (eq 'Expr_Assign (dom-tag node*)) do (setf last-assignment (node*))); @todo thats always nil
       (let* ((ast (get-text-property (dom-attr last-assignment 'start) 'yo-php-node))
              (type
               (cond
                ((assoc 'Expr_Assign ast)
                 (yo-php-ast--get-type-of-assignment (assoc 'Expr_Assign ast)))
                ((assoc 'Param ast)
                 (yo-php-ast--get-type-of-param last-assignment (assoc 'Param ast) ast))
                (t nil))))
         type)))))

(defvar *yo-php-ast-debounce-timer* nil)

(defun yo-php-ast-debounce-parsing ()
  (when (eq major-mode 'php-mode)
    (when *yo-php-ast-debounce-timer*
      (cancel-timer *yo-php-ast-debounce-timer*))
    (setf *yo-php-ast-debounce-timer*
          (run-at-time 1 nil 'yo-php-ast-parse (current-buffer)))))

(defun yo-php-ast-start-parsing ()
  (interactive)
  (add-hook 'post-command-hook 'yo-php-ast-debounce-parsing nil t))

(defun yo-php-ast-stop-parsing ()
  (interactive)
  (remove-hook 'post-command-hook 'yo-php-ast-debounce-parsing t))

(provide 'yo-php-ast)
