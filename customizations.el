(let ((add-path (lambda (path) (add-to-list 'load-path (concat user-emacs-directory path)))))
  (mapc
   add-path
   '("yo")))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; default variable definitions
(setq
 make-backup-files nil
 auto-save-default nil
 c-default-style "linux"
 c-basic-offset 4
 autopair-autowrap t
 inhibit-startup-echo-area-message (user-login-name)
 inhibit-startup-message t
 scroll-preserve-screen-position t
 initial-scratch-message ";; ¡Bienvenido, Hoy es un buen dia!"
 initial-major-mode 'emacs-lisp-mode
 company-idle-delay 0
 ring-bell-function 'ignore;;stop the annoying BEEPS when for eg. you are at the beginning/end of a file
 httpd-port 4321
 httpd-root "~/Programming/html/skewer"
 inferior-lisp-program "/usr/lib/ccl/lx86cl64"
 pop-up-windows nil
 show-paren-delay 0
 js2-highlight-level 3 
 enable-local-variables nil;; prevents emacs to automatically execute so called 'local variable lists' that are normally executed when opening a file where they are defined at the bottom of the file
 org-hide-leading-stars t)

(setq-default
 c-basic-offset 4
 tab-width 4
 indent-tabs-mode nil
 truncate-lines t
 cursor-type 'bar
 line-spacing 4)

;; miscellaneous functions for key bindings (not related to one specific mode)
(require 'yo-delete);; miscellaneous functions to delete things
(require 'yo-swap);; functions to wap lines
(require 'yo-tmp-bookmark)

(defun yo-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun yo-duplicate-current-line ()
  "copies the current line (not to the kill ring). keeps the mouse position +one line)"
  (interactive)
  (let ((starting-point (point))
	(start (progn (beginning-of-line) (point)))
	(end (progn (end-of-line) (point))))
    (terpri 'insert)
    (insert (buffer-substring start end))
    (goto-char starting-point)
    (next-line)))

(defun yo-backspace (&optional delete-char)
  "acts like (delete-char -1) unless there is only [:space:] left until the biginning of line,
then it deletes all whitespaces with one newline.
Indents the line at the end."
  (interactive "")
  (funcall
   (or delete-char 'delete-char)
   (if (looking-back "^[[:space:]]+")
       (- (line-beginning-position) (point) 1)
     -1))
  (indent-for-tab-command))

(defun yo-yank ()
  "yanks and indents the yanked region"
  (interactive)
  (let ((start (point)))
    (yank)
    (indent-region start (point))))

;; miscellaneous functions
(defun yo-set-background-transparent ()
  "makes the background transparent on terminals"
  (unless (window-system)
    (set-face-attribute 'default nil :background "unspecified-bg")
    (set-face-attribute 'mode-line nil :background "unspecified-bg")
    (set-face-attribute 'mode-line-inactive nil :background "unspecified-bg")))

(defun yo-c-set-offset-to-zero ()
  "sets the c-set-offset to 0 for opening braces"
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'block-open 0))

;; ocaml
(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook
 'merlin-mode-hook
 (lambda ()
   (define-key
     merlin-mode-map
     (kbd "RET")
     (lambda ()
       (interactive)
       (merlin-error-check)
       (insert "\n")))))

;; css
(add-hook
 'css-mode-hook
 (lambda ()
   (define-key css-mode-map "DEL" 'yo-backspace)))

;; html
(add-hook
 'html-mode-hook
 (lambda ()
   (define-key html-mode-map "DEL" 'yo-backspace)))

;; web mode
(add-hook
 'web-mode-hook
 (lambda ()
   (define-key web-mode-map "DEL" 'yo-backspace)))

;; js
(require 'yo-js-modules)

(add-hook
 'company-mode-hook
 (lambda ()
   (add-to-list 'company-backends 'yo-js-company-backend t)))

(add-hook
 'rjsx-mode-hook
 (lambda ()
   (define-key rjsx-mode-map (kbd "DEL") 'yo-backspace)
   (define-key rjsx-mode-map (kbd "C-c RET") 'yo-js-find-missing-module)
   (define-key rjsx-mode-map (kbd "C-c C-c") 'yo-js-switch-to-view/container)
   (define-key rjsx-mode-map (kbd "C-c 2") 'yo-js-split-view/container-vertically)
   (define-key rjsx-mode-map (kbd "C-c 3") 'yo-js-split-view/container-horizontally)))

(add-hook 'js-mode-hook 'yo-js-project-hook)

(add-hook
 'rjsx-mode-hook
 (lambda ()
   (require 'swiper)
   (define-key rjsx-mode-map (kbd "C-d") 'yo-duplicate-current-line)
   (define-key rjsx-mode-map (kbd "C-c s") 'swiper-all)
   (js2-highlight-undeclared-vars)
   (js2-highlight-unused-variables)
   (js2-highlight-vars-mode)))

(add-hook
 'js-mode-hook
 (lambda ()
   (defun js2-node-at-point (&optional pos skip-comments)
     "This overwrites the default implementation of js2-node-at-point.
Sometime the function throws an error, this modified version silently ignores this error.
Original documenation:

Return AST node at POS, a buffer position, defaulting to current point.
The `js2-mode-ast' variable must be set to the current parse tree.
Signals an error if the AST (`js2-mode-ast') is nil.
Always returns a node - if it can't find one, it returns the root.
If SKIP-COMMENTS is non-nil, comment nodes are ignored."
   (let ((ast js2-mode-ast)
         result)
     (when ast
       ;; Look through comments first, since they may be inside nodes that
       ;; would otherwise report a match.
       (setq pos (or pos (point))
             result (if (> pos (js2-node-abs-end ast))
                        ast
                      (if (not skip-comments)
                          (js2-comment-at-point pos))))
       (unless result
         (setq js2-discovered-node nil
               js2-visitor-offset 0
               js2-node-search-point pos)
         (unwind-protect
             (catch 'js2-visit-done
               (js2-visit-ast ast #'js2-node-at-point-visitor))
           (setq js2-visitor-offset nil
                 js2-node-search-point nil))
         (setq result js2-discovered-node))
       ;; may have found a comment beyond end of last child node,
       ;; since visiting the ast-root looks at the comment-list last.
       (if (and skip-comments
                (js2-comment-node-p result))
           (setq result nil))
       (or result js2-mode-ast))))))

;; org
(add-hook
 'org-mode-hook
 (lambda ()
   ;; (define-key org-mode-map (kbd "RET") 'yo-newline&indent)
   ))

;; dired
(add-hook
 'dired-mode-hook
 (lambda ()
   (dired-hide-details-mode 1)))

;; eshell
(defun yo-eshell-get-prompt-regex ()
  "returns the a custom regexp that is used for the eshell prompt in conjunction of yo-eshell-prompt-function."
  "^ →  ")

(defun yo-eshell-prompt-function ()
  "returns the prompt for the ehsell mode"
  (concat
   " "
   (propertize "→ " 'face '(:foreground "#eb0e69"))
   " "))

(setq eshell-prompt-regexp (yo-eshell-get-prompt-regex))
(setq eshell-prompt-function 'yo-eshell-prompt-function)

(add-hook
 'eshell-mode-hook
 (lambda ()
   (company-mode -1)
   (add-to-list 'eshell-visual-subcommands '("git" "diff" "log" "show"))
   (add-to-list 'eshell-visual-subcommands '("npm" "test" "start" "install"))
   ;; C-a is bound to eshell-bol, so this fixes the missing home key to act the same
   (define-key eshell-mode-map [home] 'eshell-bol)))

;; c
(add-hook
 'c-mode-hook
 (lambda () (define-key c-mode-map (kbd "C-d") 'yo-duplicate-current-line)))

;; c++
(add-hook
 'c++-mode-hook
 (lambda ()
   (yo-c-set-offset-to-zero)
   (define-key c++-mode-map (kbd "C-d") 'yo-duplicate-current-line)))
 
;; java
(add-hook 'java-mode-hook  'yo-c-set-offset-to-zero)

;; paredit
(defun yo-paredit-backspace ()
  "Deletes the line if there are only white spaces left, calls the paredit delete char function."
  (interactive "")
  (yo-backspace (lambda (x) (paredit-backward-delete (- x)))))

(when (package-installed-p 'paredit)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook
   'paredit-mode-hook
   (lambda ()
     (define-key paredit-mode-map (kbd "DEL") 'yo-paredit-backspace))))

;; yo overlay
(when (package-installed-p 'auto-overlays)
  (require 'yo-overlay)
  (yo-overlay-add-hooks))

;; ivy
(add-hook
 'ivy-mode-hook
 (lambda ()
   (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)))

;; global key bindings
(global-set-key (kbd "<f6>")    'iedit-mode)
(global-set-key (kbd "C-h")     'yo-delete-word-backward)
(global-set-key [(C delete)]    'yo-delete-word)
(global-set-key (kbd "C-k")     'yo-delete-line)
(global-set-key (kbd "C-x x")   'compile)
(global-set-key [home]          'yo-smart-beginning-of-line)
(global-set-key (kbd "C-a")     'yo-smart-beginning-of-line)
(global-set-key (kbd "C-c C-d") 'ace-jump-mode)
(global-set-key (kbd "C-d")     'yo-duplicate-current-line)
(global-set-key [(M up)]        'swap-lines-up)
(global-set-key [(M down)]      'swap-lines-down)
(global-set-key [(M left)]      'pop-tmp-bookmark)
(global-set-key (kbd "M-#")     'add-tmp-bookmark-at-point)
(global-set-key (kbd "C-s")     'isearch-forward)
(global-set-key (kbd "C-c -") 'comment-or-uncomment-region)
(global-set-key (kbd "C-y") 'yo-yank)

(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "<insertchar>"))

;; general definitions that don't belog to any specific mode
(put 'narrow-to-region 'disabled nil)

(unless (window-system)
      (yo-set-background-transparent))
