
(defun yo-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))


(defun yo-delete-word-backward (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word (- arg)))

(defun yo-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (beginning-of-line)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (backward-char)
  (delete-char 1))

(provide 'yo-delete)
