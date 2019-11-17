
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
          `((node-modules ,(yo-js-load-node-modules directory (cdr (assoc 'dependencies (json-read-file package-json-file)))))
            (modules ,(yo-js-load-modules directory))))))

(defun yo-js-project-rehash ()
  (interactive)
  (let ((project (yo-js-files-project)))
    (unless project
      (error "you are not in a managed file"))
    (setf (cdr (assoc 'modules project))
          (list (yo-js-load-modules (car project))))))

(defun yo-js-project-add-node-submodule (symbol)
  (interactive "Senter module: ")
  (let ((project (yo-js-files-project)))
    (unless project
      (error "you are not in a managed file"))
    (nconc (cadr (assoc 'node-modules project))
           (yo-js-load-node-modules (car project) `((,symbol))))))

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
         (let ((quit-window nil))
           (with-current-buffer (find-file file)
             (unless (eq 'rjsx-mode major-mode)
               (rjsx-mode)
               (setf quit-window t))
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
             (when quit-window
               (quit-window)))))
       files))
    dict))

(defun yo-js-load-node-modules-node-function ()
  "@todo"
  (concat
   "const sig = (str) => /^function/.test(str) ? str.substring(str.indexOf('('), str.indexOf(')') +1) : str.substring(0, str.indexOf('=>'));"
   "const typ = (x) => typeof x === 'function' ? sig(x.toString()) : typeof x;"
   "const req = (n) => {try {return require(n);}catch(e){return {};}};"
   "const filter = (x) => !/DO_NOT_USE/.test(x);"
   "const mapit = (l, n, obj) => l.concat(Object.keys(obj).filter(filter).map((k) => k === 'default' ?`(\"${n.split('-')[0]}\" (module \"${n}\") (default t) (type \"${typ(obj[k])}\"))` :`(\"${k}\" (module \"${n}\") (type \"${typ(obj[k])}\"))`));"
   "const map = (l, n) => mapit(l, n, (req(n)));"
   "const listit = (l) => `(${l.reduce(map, []).join(' ')})`;"))

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
        "const DEPENDENCIES = ["
        (join ", " (mapcar (lambda (name) (concat "\"" (symbol-name (car name)) "\"")) list))
        "];"
        (yo-js-load-node-modules-node-function)
        "console.log(listit(DEPENDENCIES));")))))))

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

(defun yo-js-project-find-candidates-for-node-modules (project)
  (mapcar
   'car
   (cadr (assoc 'node-modules (cdr (yo-js-files-project))))))

(defun yo-js-project-find-candidates-for-modules (project)
  (mapcar
   'car
   (cadr (assoc 'modules (cdr (yo-js-files-project))))))

(defun yo-js-project-find-candidates (project f)
  (cl-remove-duplicates
   (cl-remove-if-not
    f
    (append (yo-js-project-find-candidates-for-node-modules project)
            (yo-js-project-find-candidates-for-modules project)))
   :test 'string-equal))

(defun yo-js-company-backend (command &optional arg &rest ignored)
  (cl-case command
    (interactive (company-begin-backend 'yo-js-company-backend))
    ('prefix
     (when (derived-mode-p 'rjsx-mode)
       (let* ((project (yo-js-files-project))
              (word (company-grab-word))
              (regexp (concat "^" (regexp-quote (or word "")))))
         (and project
              word ;; this maybe nil, so the candidates don't need to be collected
              (yo-js-project-find-candidates project (lambda (x) (string-match-p regexp x)))
              word))))
    ('candidates
     (let ((regexp (concat "^" (regexp-quote arg))))
       (yo-js-project-find-candidates (yo-js-files-project) (lambda (x) (string-match-p regexp x)))))
    ('meta
     "huuihui")
    ;; ('annotation "<a>")
    ;; ('post-completion
    ;;  (let ((f (get-text-property 0 'yo-data arg)))
    ;;    (when f
    ;;      (funcall f))))
    ))

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

(defun yo-js-thingis-info (key word &optional project)
  (let ((modules (cadr (assoc key (cdr (or project (yo-js-files-project)))))))
    (cl-remove-if-not (lambda (x) (string-equal word (car x))) modules)))

(defun yo-js-modules-info (word &optional project)
  (yo-js-thingis-info 'modules word project))

(defun yo-js-node-modules-info (word &optional project)
  (yo-js-thingis-info 'node-modules word project))

(defun yo-js-info (word module-cc node-module-cc not-found-cc &optional package)
  (let* ((project (yo-js-files-project))
         (candidates
          (append
           (mapcar
            (lambda (info)
              (list (substring (cadr (assoc 'file info)) (length (car project)))
                    module-cc
                    info))
            (yo-js-modules-info word project))
           (mapcar
            (lambda (info)
              (list (cadr (assoc 'module info))
                    node-module-cc
                    info))
            (yo-js-node-modules-info word project)))))
    (if candidates
        (pcase
            (if (cdr candidates)
                (assoc
                 (or
                  package
                  (completing-read
                   (format "select module for %s: " word)
                   candidates
                   nil
                   t))
                 candidates)
              (car candidates))
          (`(,name ,f ,i)
           (funcall f i project)))
      (funcall not-found-cc))))

(defun yo-js-find-missing-module ()
  (interactive)
  (yo-js-info
   (current-word)
   (lambda (info project)
     (yo-js-add-import
      (yo-js-navigate-path (buffer-file-name) (cadr (assoc 'file info)))
      (car info)
      (cadr (assoc 'default info))))
   (lambda (info project)
     (yo-js-add-import
      (cadr (assoc 'module info))
      (car info)
      (cadr (assoc 'default info))))
   (lambda ()
     (message "no module found..."))))

(defun yo-json-new-object (prefix)
  (interactive "p")
  (print prefix)
  (if (equal prefix 1)
      (progn
        (insert "{}")
        (backward-char))
      (let ((start (point)))
        (insert "{\n}")
        (backward-char 2)
        (insert "\n")
        (indent-region start (+ 2 (point)))
        (indent-for-tab-command))))

(defun yo-json-new-array ()
  (interactive)
  (let ((start (point)))
    (insert "[\n]")
    (backward-char 2)
    (insert "\n")
    (indent-region start (+ 2 (point)))
    (indent-for-tab-command)))

(defun yo-js-get-vars-in-scope (&optional pos)
  "mhmh"
  (interactive)
  (let ((node (js2-node-at-point pos))
        (tokens nil)
        name
        scope)
    (unless (js2-name-node-p node)
      (setq node (js2-node-at-point (- (or pos (point)) 1))))
    (when (and node (js2-name-node-p node))
      (setq scope (js2-node-get-enclosing-scope node)
            name (js2-name-node-name node))
      (setq scope (js2-get-defining-scope scope name))
      (with-silent-modifications
        (js2-visit-ast
         scope
         (lambda (node end-p)
           (when (and (not end-p)
                      (js2-name-node-p node)
                      (string= name (js2-name-node-name node)))
             (let* ((beg (js2-node-abs-pos node))
                    (end (+ beg (js2-node-len node)))
                    (new-scope (js2-node-get-enclosing-scope node))
                    (new-scope (js2-get-defining-scope new-scope name)))
               (add-to-list 'tokens beg t)))
           t)))
      tokens)))

(defun yo-js-get-package-or-marker (&optional pos)
  (let ((pos (or pos (point))))
    (let ((tokens (yo-js-get-vars-in-scope pos)))
      (when tokens
        (save-excursion
          (goto-char (car tokens))
          (if (re-search-forward "['\"]" (line-end-position) t)
            (let ((start (point)))
              (backward-char)
              (forward-sexp)
              (backward-char)
              (buffer-substring-no-properties start (point)))
            (car tokens)))))))

(defun yo-js-goto-definition (name package)
  (if (string-match-p "^\\.\\.?/" package)
      (progn
        (find-file (concat package ".js"))
        (if (string-equal name (file-name-base package))
            (re-search-forward "^[[:space:]]*export default ")
          (re-search-forward (concat "^[[:space:]]*export[[:space:]]+.*" (regexp-quote name)))))
    (let ((project (yo-js-files-project)))
      (when project
        (let ((matches (split-string
                        (shell-command-to-string
                         (concat "grep -P -n -R --include '*.js' "
                                 (shell-quote-argument (concat "((var)|(let)|(const)|(function)) " name " (=|\\()"))
                                 " "
                                 (shell-quote-argument (yo-pathifism (car project) "node_modules" package))))
                        "\n")))
          (when matches
            (pcase (split-string (car matches) ":")
              (`(,file ,line-nr . ,_)
               (find-file file)
               (goto-line (string-to-number line-nr))))))))))

(defun yo-js-goto-thing-at-point ()
  (interactive)
  (let ((p-or-m (yo-js-get-package-or-marker)))
    (when p-or-m
      (if (number-or-marker-p p-or-m)
          (goto-char p-or-m)
        (yo-js-goto-definition (current-word) p-or-m)))))

(defun yo-js-type-of-thing-at-point ()
  (interactive)
  (let ((p-or-m (yo-js-get-package-or-marker)))
    (when (and p-or-m (stringp p-or-m))
      (yo-js-info
       (current-word)
       'ignore
       (lambda (info _)
         (message (cadr (assoc 'type (cdr info)))))
       'ignore
       p-or-m))))

(defun yo-js-project-hook ()
  (when (and (not (eq 'rjsx-mode major-mode)) (yo-js-files-project))
    (rjsx-mode)))

(provide 'yo-js-modules)
