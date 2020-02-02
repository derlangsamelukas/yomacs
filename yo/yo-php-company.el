(defun yo-php-company-backend (command &optional arg &rest ignored)
  (cl-case command
    (interactive (company-begin-backend 'yo-php-backend))
    (prefix
     (and (derived-mode-p 'php-mode)
          (looking-back "->\\([[:word:]_]*\\)")
          (cons (match-string 1) t)))
    (candidates
     (when (looking-back (concat "\\$this->" (regexp-quote arg)))
       (mapcar
        (lambda (node) (dom-attr node 'name))
        (dom-by-tag
         (assoc 'Stmt_Class (yo-php-ast-at-point))
         'Stmt_ClassMethod))))))

(add-to-list 'company-backends 'yo-php-company-backend)

(provide 'yo-php-company)
