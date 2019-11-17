(defvar *yo-projects* nil)

(defvar *yo-project-templates*
  '(("None" 'find-file)
    ("React"
     (lambda (dir)
       (yo-js-project-init (yo-pathifism dir "package.json"))
       (find-file dir)))
    ("Chicken"
     (lambda (dir)
       (find-file dir)
       (geiser 'chicken)))))

(defun yo-insert-button (label callback)
  "@todo look at button package, how to use them correctly"
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") callback)
    (insert-text-button label 'keymap keymap)))

(defun yo-startup-read-projects-file ()
  (let ((file (yo-pathifism user-emacs-directory "projects")))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file file)
        (car (read-from-string (buffer-string)))))))

(defun yo-startup-projects-to-buttons (projects space)
  (mapc
   (lambda (project)
     (yo-insert-button
      (car project)
      `(lambda ()
         (interactive)
         (funcall ,(cadr (assoc (caddr project) *yo-project-templates*)) ,(cadr project))))
     (funcall space))
   projects))

(defun yo-startup-create-new-project ()
  (interactive)
  (let* ((name (read-from-minibuffer "Please enter the project's name: "))
         (dir (read-file-name "Please enter the project's root: "))
         (template (completing-read "Please enter a template for this project: " *yo-project-templates* nil t)))
    (if (assoc name *yo-projects*)
        (message "Sorry a project with the given name does already exist")
      (progn
        (push
         (list name dir template)
         *yo-projects*)
        (with-temp-file (yo-pathifism user-emacs-directory "projects")
          (insert (pp-to-string *yo-projects*)))
        (yo-startup-refresh)
        (message (concat "Successfully added " name " as a new project"))))))

(defun yo-startup-refresh ()
  (when (string-equal (buffer-name) "*Startscreen*")
    (read-only-mode t)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert "Greetings, where do you want to start?\n\n")
      (unless (yo-startup-projects-to-buttons *yo-projects* (lambda () (insert "\n")))
        (insert "Wow, such empty\n"))
      (insert "\n")
      (yo-insert-button "Create new project" 'yo-startup-create-new-project)
      (goto-char (point-min))
      (current-buffer))))

(defun yo-startup-screen ()
  (let ((projects (yo-startup-read-projects-file)))
    (setf *yo-projects*  projects)
    (with-current-buffer (get-buffer-create "*Startscreen*")
      (yo-startup-refresh))))

(provide 'yo-startup)
