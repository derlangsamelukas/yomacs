
(defun yo-swap-lines-up ()
  "swap point's line with the line above"
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun yo-swap-lines-down ()
  "swap point's line with the line beneath"
  (interactive)
  (previous-line -1)
  (transpose-lines 1)
  (previous-line))


(provide 'yo-swap)
