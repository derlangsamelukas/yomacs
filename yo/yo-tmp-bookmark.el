;; @todo add yo- prefix

(defvar tmp-bookmark-markers nil)

(defun jump-to-f-on-tmp-bookmark (markers on-success)
  (if tmp-bookmark-markers
      (let ((mark (car markers)))
	(switch-to-buffer (marker-buffer mark))
	(goto-char mark)
	(funcall on-success)
	t)
    (progn
      (message "No temporary marker is left.")
      nil)))

(defun add-tmp-bookmark (position)
  (set-marker
   (car (push (make-marker) tmp-bookmark-markers))
   position))


(defun add-tmp-bookmark-at-point ()
  (interactive)
  (add-tmp-bookmark (point)))

(defun jump-to-tmp-bookmark ()
  (interactive)
  (jump-to-f-on-tmp-bookmark tmp-bookmark-markers
			     (lambda ())))

(defun pop-tmp-bookmark ()
  (interactive)
  (jump-to-f-on-tmp-bookmark tmp-bookmark-markers
			     (lambda () (pop tmp-bookmark-markers))))


(defun jump-to-last-tmp-bookmark&push-old-point ()
  (interactive)
  (let ((point (point)))
    (and
     (jump-to-tmp-bookmark)
     (add-tmp-bookmark point))))


(provide 'yo-tmp-bookmark)
