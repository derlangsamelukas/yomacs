
(defvar yo-mode-line-updated (lambda () ""))

(make-variable-buffer-local 'yo-mode-line-updated)

(defun yo-colorize-string (string colored-chars)
  (cl-reduce
   (lambda (str char)
     (concat str (if (cl-find char colored-chars)
                     (propertize
                      (char-to-string char)
                      'face
                      '(:foreground "#eb0e69"))
                   (char-to-string char))))
   string
   :initial-value ""))

(defun yo-update-mode-line ()
  "custom mode line function that shows only some minimal information"
  (setq-default
   mode-line-format
   `(" "
     (:eval (if (and buffer-file-name (buffer-modified-p)) (propertize "*" 'face '(:foreground "#eb0e69")) ""))
     " "
     (:eval (yo-colorize-string (buffer-name) ".:*/"))
     " %lL"
     (:eval (if buffer-read-only " READ ONLY" ""))
     "   "
     (:eval (yo-colorize-string (format-time-string "%H:%M %d.%m.%Y") ".:*/"))
     " "
     (:eval (format "%d" (string-to-number (cdr (cadddr (cddddr (battery-linux-sysfs)))))))
     "%%"
     " "
     (:eval (yo-colorize-string (funcall yo-mode-line-updated) ".:*/")))))

(provide 'yo-mode-line)
