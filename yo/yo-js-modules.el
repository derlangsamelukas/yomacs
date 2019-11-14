
(defvar *yo-js-scopes* (make-hash-table :test 'equal))

(unless (symbol-function 'join)
  (defun join (glue list)
    (apply 'concat (cdr (mapcan (lambda (x) (list glue x)) list)))))

(unless (symbol-function 'first)
  (defalias 'first 'car))

(defun yo-pathifism (&rest fragments)
  (require 'subr-x)
  (join
   "/"
   (append
    (when (and (car fragments) (char-equal ?/ (aref (car fragments) 0))) '(""))
    (mapcar (lambda (fragment) (string-trim fragment "/" "/")) (cl-remove-if (lambda (s) (string-equal s "")) fragments)))))

(defun yo-js-project-init (package-json-file)
  (interactive "f")
  (require 'json)
  (let ((directory (file-name-directory package-json-file)))
    (setf (gethash directory *yo-js-scopes*)
          `((node-modules ,(yo-js-load-node-modules directory (mapcar 'symbol-name (mapcar 'car (cdr (assoc 'dependencies (json-read-file package-json-file)))))))
            (modules ,(yo-js-load-modules directory))))))

(defun yo-js-project-rehash ()
  (interactive)
  (let ((project (yo-js-files-project)))
    (unless project
      (error "you are not in a managed file"))
    (setf (cdr (assoc 'modules project))
          (list (yo-js-load-modules (car project))))))

(defun yo-js-files-project (&optional filename)
  (let* ((filename (or filename (buffer-file-name)))
         (matches (lambda (key) (string-match-p (concat "^" (regexp-quote key)) filename))))
    (pcase (cl-remove-if-not matches (hash-table-keys *yo-js-scopes*))
      ('() nil)
      (`(,x . ,xs)
       (let ((directory (cl-reduce (lambda (string* string) (if (> (length string*) (length string)) string* string)) xs :initial-value x)))
         (cons directory (gethash directory *yo-js-scopes*)))))))

(defun yo-js-navigate-path (from to)
  (when (or (string-match-p (regexp-quote from) to)
            (string-match-p (regexp-quote to) from))
    (error "to is a substring of from or from of to..."))
  (let ((from (split-string from "/"))
        (to (split-string to "/")))
    (cl-labels ((runner (from to)
                        (when (and from to)
                          (if (string-equal (car from) (car to))
                              (runner (cdr from) (cdr to))
                            (let ((path (concat (join "/" (or (mapcar (lambda (_) "..") (cdr from)) '("."))) "/" (join "/" to))))
                              (substring path 0 (- (length (file-name-extension path t)))))))))
      (runner from to))))

(defun yo-js-module-find (&optional name)
  (interactive)
  (let* ((name (or name (current-word)))
         (addit (lambda (alist)
                 (yo-js-add-import (yo-js-navigate-path (buffer-file-name) (cadr (assoc 'file alist)))
                                name
                                (cadr (assoc 'default alist))))))
    (pcase (cl-remove-if-not
            (lambda (x) (string-equal name (car x)))
            (cadr (assoc 'modules (cdr (yo-js-files-project)))))
      ('() (message "no module found which exports '%s'" name))
      (`((,_ . ,alist))
       (funcall addit alist))
      (list
       (print list)
       (funcall addit (cdr (assoc (completing-read "From which namespace do want to import? " (mapcar 'car list) nil t) list)))))))

(defun yo-js-word-after-match (data cc)
  (save-excursion
    (pcase data
      (`(,_ ,_ ,_ ,end . ,_)
       (goto-char end)
       (forward-word)
       (backward-char)
       (let ((word (thing-at-point 'word)))
         (when word
           (set-text-properties 0 (length word) nil word)
           (funcall cc word)))))))

(defun yo-js-load-modules (directory)
  (let ((files (cl-remove-if
                'string-empty-p
                (split-string
                 (shell-command-to-string (concat "find " (regexp-quote (yo-pathifism directory "src")) " -name '*.js' -type f")) "\n")))
        (dict))
    (cl-labels ((add (name file &optional default)
                     (push `(,name (file ,file) (default ,default)) dict))
                (maybe-add-next (data file)
                                (yo-js-word-after-match data (lambda (word) (add word file)))))
      (mapc
       (lambda (file)
         (with-current-buffer (find-file file)
           (unless (eq 'rjsx-mode major-mode)
             (rjsx-mode))
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward "^[[:space:]]*export[[:space:]]+\\([[:word:]]+\\)[[:space:]]+" nil t)
               (pcase (match-string-no-properties 1)
                 ("default" (add (file-name-base file) file t))
                 ("function" (maybe-add-next (match-data 1) file))
                 ("let" (maybe-add-next (match-data 1) file))
                 ("const" (maybe-add-next (match-data 1) file))
                 ("var" (maybe-add-next (match-data 1) file))
                 (x (add x file)))))
           (quit-window)))
       files))
    dict))

(defun yo-js-load-node-modules (directory list)
  (car
   (read-from-string
    (shell-command-to-string
     (concat
      "cd "
      (shell-quote-argument directory)
      " && node -e "
      (shell-quote-argument
       (concat
        "deps = ["
        (join ", " (mapcar (lambda (name) (concat "\"" name "\"")) list))
        "];"
        "const f = (n) => {try {return require(n);}catch(e){return {};}};"
        "console.log('(' + deps.map((n) => '(' + n + ' ' + Object.keys(f(n)).join(' ') + ')').join(' ') + ')')")))))))

(defun yo-js-import-insert-name (name end)
  (unless (string-equal name (current-word))
    (if (>= (point) end)
        (progn
          (search-backward "}")
          (while (char-equal (char-before) ?\ ) (backward-char))
          (insert ", " name))
      (progn
        (forward-word)
        (when (string-equal "as" (current-word)) (forward-word 2))
        (yo-js-import-insert-name name end)))))

(defun yo-js-add-import (package name &optional is-default)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^[[:space:]]*import[[:space:]]+\\(.+\\)[[:space:]]+from ['\"]" (regexp-quote package) "['\"];?$") nil t)
        (let* ((data (match-data 1))
               (group (buffer-substring-no-properties (caddr data) (cadddr data))))
          (goto-char (caddr data))
          (if is-default
              (unless (thing-at-point 'word)
                (insert name ", "))
            (progn
              (if (and (thing-at-point 'word) (not (string-match-p "{" group)))
                  (progn (forward-word) (insert ", { " name " }"))
                (progn
                  (forward-word)
                  (yo-js-import-insert-name name (cadddr data)))))))
      (let ((import (concat "import " (if is-default name (concat "{ " name " }")) " from '" package "';")))
        (while (re-search-forward "^[[:space:]]*import[[:space:]]+\\(.*\\)[[:space:]]+from ['\"][^[:space:]]+['\"];?$" nil t))
        (if (= (point) (point-min))
            (insert import "\n")
          (insert "\n" import))
        t))))

(defun yo-js-node-module-find (&optional name)
  (interactive)
  (let* ((name (intern (or name (current-word))))
         (found-ones
          (cl-remove-if-not
           (lambda (pair) (cl-find name (cdr pair)))
           (cadr (assoc 'node-modules (cdr (yo-js-files-project)))))))
    (when found-ones
      (yo-js-add-import (format "%s" (caar found-ones)) (format "%s" name)))))


(defun yo-js-node-module-search ()
  (interactive)
  (yo-js-node-module-find (completing-read "Search: " (cl-reduce (lambda (l x) (append l (cdr x))) deps :initial-value nil))))

(defun yo-js-project-find-candidates-for-node-modules (project)
  (mapcan
   'cdr
   (mapcar
    (lambda (x) (mapcar (lambda (x) (propertize (symbol-name x) 'yo-data 'yo-js-node-module-find)) x))
    (cadr
     (assoc
      'node-modules
      (cdr (yo-js-files-project)))))))

(defun yo-js-project-find-candidates-for-modules (project)
  (mapcar
   (lambda (x)
     (propertize (car x) 'yo-data 'yo-js-module-find))
   (cadr (assoc 'modules (yo-js-files-project)))))

(defun yo-js-project-find-candidates (project f)
  (cl-remove-if-not
   f
   (append (yo-js-project-find-candidates-for-node-modules project)
           (yo-js-project-find-candidates-for-modules project))))

(defun yo-js-company-backend (command &optional arg &rest ignored)
  (cl-case command
    (interactive (company-begin-backend 'yo-js-company-backend))
    ('prefix
     (when (derived-mode-p 'rjsx-mode)
       (let* ((project (yo-js-files-project))
              (word (company-grab-word))
              (regexp (concat "^" (regexp-quote word))))
         (and project
              (yo-js-project-find-candidates project (lambda (x) (string-match-p regexp x)))
              word))))
    ('candidates
     (let ((regexp (concat "^" (regexp-quote arg))))
       (yo-js-project-find-candidates (yo-js-files-project) (lambda (x) (string-match-p regexp x)))))
    ('post-completion
     (let ((f (get-text-property 0 'yo-data arg)))
       (when f
         (funcall f))))))

;; xxxx

(defun yo-js-src-folder ()
  (yo-pathifism (car (yo-js-files-project)) "src/"))

(defun yo-js-test-folder ()
  (yo-pathifism (car (yo-js-files-project)) "test/"))

(defun yo-js-new-component* (folder folder* name)
  (let ((file (yo-pathifism (yo-js-src-folder) folder folder* (concat name ".js"))))
    (if (file-exists-p file)
        (find-file file)
      (with-current-buffer (find-file file)
        (insert "component")
        (rjsx-mode)
        (yas-expand)
        (insert name)))))

(defun yo-js-new-component (name folder)
  (interactive "sName of Component: \nsFolder: ")
  (yo-js-new-component* "components" folder name))

(defun yo-js-new-view (name folder)
  (interactive "sName of Component: \nsFolder: ")
  (yo-js-new-component* "views" folder name))

(defun yo-js-switch-to (file from to snippet &optional cc)
  (let ((target-file (yo-pathifism (yo-js-src-folder) to (substring file (1+ (length from))))))
          (if (file-exists-p target-file)
              (find-file target-file)
            (when (y-or-n-p (concat "file " target-file " does not exist, create? "))
              (find-file target-file)
              (insert snippet)
              (rjsx-mode)
              (yas-expand)
              (insert (file-name-base target-file))
              (funcall (or cc (lambda (_))) target-file)
              (save-buffer)))))

(defun yo-js-switch-to-container (file)
  (yo-js-switch-to
   file
   "views"
   "container"
   "container"
   (lambda (target-file)
     (yas-next-field)
     (insert
      (join
       "/"
       (append
        (cdr
         (mapcar
          (lambda (_) "..")
          (split-string (string-trim file "/") "/")))
        (list (string-trim (substring file 0 (- 0 1 (length (file-name-extension file)))) "/"))))))))

(defun yo-js-switch-to-view (file)
  (yo-js-switch-to file "container" "views" "component"))

(defun yo-js-switch-to-container* (file)
  (let ((container-file (yo-pathifism (yo-js-src-folder) "container" (substring file (length "/views")))))
          (if (file-exists-p container-file)
              (find-file container-file)
            (when (y-or-n-p (concat "file " container-file " does not exist, create? "))
              (find-file container-file)
              (insert "container")
              (rjsx-mode)
              (yas-expand)
              (insert (file-name-base container-file))
              (yas-next-field)
              (insert
               (join
                "/"
                (append
                 (cdr
                  (mapcar
                   (lambda (_) "..")
                   (split-string (string-trim file "/") "/")))
                 (list (string-trim (substring file 0 (- 0 1 (length (file-name-extension file)))) "/")))))
              (save-buffer)))))

(defun yo-js-switch-to-view/container ()
  (interactive)
  (unless (string-match-p (concat "^" (regexp-quote (yo-js-src-folder))) (or (buffer-file-name) ""))
    (error "you are not in a file under the react root"))
  (let ((file (substring (buffer-file-name) (length (yo-js-src-folder)))))
    (cond
     ((string-match-p "^/views" file)
      (yo-js-switch-to-container file))
     ((string-match-p "^/container" file)
      (yo-js-switch-to-view file))
     (t (message "this is no view or container")))))

(defun yo-js-split-view/container (how)
  (funcall how)
  (other-window 1)
  (yo-js-switch-to-view/container))

(defun yo-js-split-view/container-vertically ()
  (interactive)
  (yo-js-split-view/container 'split-window-vertically))

(defun yo-js-split-view/container-horizontally ()
  (interactive)
  (yo-js-split-view/container 'split-window-horizontally))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (define-key rjsx-mode-map (kbd "C-c C-c") 'yo-js-switch-to-view/container)
            (define-key rjsx-mode-map (kbd "C-c 2") 'yo-js-split-view/container-vertically)
            (define-key rjsx-mode-map (kbd "C-c 3") 'yo-js-split-view/container-horizontally)))


;; yyyy

(defun yo-js-project-hook ()
  (when (and (not (eq 'rjsx-mode major-mode)) (yo-js-files-project))
    (rjsx-mode)))

(provide 'yo-js-modules)