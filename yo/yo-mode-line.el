(defun yo-update-mode-line ()
  "custom mode line function that shows only some minimal information"
  (let ((colorize
         (lambda (str)
           (cl-reduce (lambda (str char)
                        (concat str (if (cl-find char ".:*")
                                        (propertize (char-to-string char) 'face '(:foreground "#eb0e69"))
                                      (char-to-string char))))
                      str
                      :initial-value ""))))
    (setq-default
     mode-line-format
     `(" "
       (:eval (if (and buffer-file-name (buffer-modified-p)) (propertize "*" 'face '(:foreground "#eb0e69")) ""))
       " "
       (:eval (,colorize (buffer-name)))
       " %lL"
       (:eval (if buffer-read-only " READ ONLY" ""))
       "   "
       (:eval (,colorize (format-time-string "%H:%M %d.%m.%Y")))
       " "
       (:eval (format "%d" (string-to-number (cdr (cadddr (cddddr (battery-linux-sysfs)))))))
       "%%"))))

(provide 'yo-mode-line)
