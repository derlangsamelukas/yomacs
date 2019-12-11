(require 'auto-overlay-word)
(require 'auto-overlay-common)

(defun yo-overlay-load (name)
  (let ((defs
          '((lisp
             (word (("\\(^\\|[^[:word:]-]\\)\\(lambda\\)\\([^[:word:]-]\\|$\\)" . 2)
                    (display "λ"))))
            (js
             (word (("\\(^\\|[^[:word:]-]\\)\\(=>\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display "→ ")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(return\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display "← ")));; 
             (word (("\\(^\\|[^[:word:]-]\\)\\(function\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display "ƒ")))
             (word (("\\(^\\|[^[:word:]-]*?\\)\\( default\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display " ")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(export\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display " ")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(\\.\\.\\.\\)[[:word:]]" . 2)
                    (display "…")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(import\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display " ")))
             (word (("\\(^\\|[^[:word:]-]\\)import .*\\(from\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display "…")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(const\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display " ")))
             (word (("\\(^\\|[^[:word:]-]\\)\\(React.memo\\)\\([^[:word:]]\\|$\\)" . 2)
                    (display "  "))))
            (php
             (word (("\\(^\\|[^[:word:]-]\\)\\(\\$\\)[[:word:]]" . 2)
                    (display "")))))))
    (mapc
     (lambda (def) (auto-overlay-load-definition name def))
     (cdr (assoc name defs)))))

(defun yo-overlay-load-and-start (name)
  (when (< (buffer-size) 10000)
    (yo-overlay-load name)
    (auto-overlay-start name)))

(defvar *yo-overlay-hooks*
  `((lisp-mode-hook ,(lambda () (yo-overlay-load-and-start 'lisp)))
    (emacs-lisp-mode-hook ,(lambda () (yo-overlay-load-and-start 'lisp)))
    (rjsx-mode-hook ,(lambda () (yo-overlay-load-and-start 'js)))
    (php-mode-hook ,(lambda () (yo-overlay-load-and-start 'php)))))

(defmacro fmatch (parameter &rest body)
  `(pcase-lambda (,parameter) ,@body))

(defun yo-overlay-add-hooks ()
  (mapc
   (pcase-lambda (`(,name ,hook)) (add-hook name hook))
   *yo-overlay-hooks*))

(defun yo-overlay-remove-hooks ()
  (mapc
   (pcase-lambda (`(,name ,hook)) (remove-hook name hook))
   *yo-overlay-hooks*))

;; (yo-overlay-add-hooks)
;; (yo-overlay-remove-hooks)

(provide 'yo-overlay)
