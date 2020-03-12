
(defvar yo-markdown-component-map (make-sparse-keymap))
(define-key yo-markdown-component-map (kbd "C-c C-c") 'yo-markdown-component-save-and-kill)

(defvar yo-markdown-component-src nil)
(make-variable-buffer-local 'yo-markdown-component-src)

(define-minor-mode
  yo-markdown-component
  "a minor mode to save the component in this buffer to the corresponding markdown region"
  nil
  nil
  yo-markdown-component-map)

(defun yo-js-markdown-edit-component ()
  (interactive)
  (let* ((start (save-excursion
                  (search-backward "```")
                  (search-forward "\n")
                  (set-marker (make-marker)
                              (match-end 0))))
         (end (save-excursion
                (search-forward "```")
                (make-marker)
                (set-marker (make-marker)
                            (match-beginning 0))))
         (buffer (generate-new-buffer "*edit markdown component*"))
         (component (buffer-substring start end)))
    (switch-to-buffer buffer)
    (rjsx-mode)
    (yo-markdown-component 1)
    (setf yo-markdown-component-src (list start end))
    (insert component)))

(defun yo-markdown-component-save-and-kill ()
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max)))
        (start (car yo-markdown-component-src))
        (end (cadr yo-markdown-component-src))
        (component-buffer (current-buffer)))
    (with-current-buffer (marker-buffer start)
      (replace-region-contents
       start
       end
       (lambda ()
         string)))
    (switch-to-buffer (marker-buffer start))
    (goto-char start)
    (kill-buffer component-buffer)))

(provide 'yo-js-markdown)
