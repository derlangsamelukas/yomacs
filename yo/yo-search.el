
(defun yo-find-file (filename)
  (interactive "sFile to search for: ")
  (let ((files (split-string (shell-command-to-string (concat "find `pwd` -name " (shell-quote-argument filename))) "\n" t)))
    (cond
     ((cdr files)
      (switch-to-buffer "*yo find file*")
      (let ((inhibit-read-only t))
        (setf (buffer-string) "")
        (mapc
         (lambda (line)
           (insert line ":1:\n"))
         files)
        (grep-mode)
        (goto-char (point-min))))
     (files
      (find-file (car files)))
     (nil (message "no files found")))))

(defun yo-grep (needle)
  (interactive "sPattern to search for: ")
  (let* ((out (shell-command-to-string (concat "find . -type d \\( -path \\*/SCCS -o -path \\*/RCS -o -path \\*/CVS -o -path \\*/MCVS -o -path \\*/.src -o -path \\*/.svn -o -path \\*/.git -o -path \\*/.hg -o -path \\*/.bzr -o -path \\*/_MTN -o -path \\*/_darcs -o -path \\*/\\{arch\\} \\) -prune -o \\! -type d \\( -name .\\#\\* -o -name \\*.hi -o -name \\*.cmti -o -name \\*.cmt -o -name \\*.annot -o -name \\*.cmi -o -name \\*.cmxa -o -name \\*.cma -o -name \\*.cmx -o -name \\*.cmo -o -name \\*.o -o -name \\*\\~ -o -name \\*.bin -o -name \\*.lbin -o -name \\*.so -o -name \\*.a -o -name \\*.ln -o -name \\*.blg -o -name \\*.bbl -o -name \\*.elc -o -name \\*.lof -o -name \\*.glo -o -name \\*.idx -o -name \\*.lot -o -name \\*.fmt -o -name \\*.tfm -o -name \\*.class -o -name \\*.fas -o -name \\*.lib -o -name \\*.mem -o -name \\*.x86f -o -name \\*.sparcf -o -name \\*.dfsl -o -name \\*.pfsl -o -name \\*.d64fsl -o -name \\*.p64fsl -o -name \\*.lx64fsl -o -name \\*.lx32fsl -o -name \\*.dx64fsl -o -name \\*.dx32fsl -o -name \\*.fx64fsl -o -name \\*.fx32fsl -o -name \\*.sx64fsl -o -name \\*.sx32fsl -o -name \\*.wx64fsl -o -name \\*.wx32fsl -o -name \\*.fasl -o -name \\*.ufsl -o -name \\*.fsl -o -name \\*.dxl -o -name \\*.lo -o -name \\*.la -o -name \\*.gmo -o -name \\*.mo -o -name \\*.toc -o -name \\*.aux -o -name \\*.cp -o -name \\*.fn -o -name \\*.ky -o -name \\*.pg -o -name \\*.tp -o -name \\*.vr -o -name \\*.cps -o -name \\*.fns -o -name \\*.kys -o -name \\*.pgs -o -name \\*.tps -o -name \\*.vrs -o -name \\*.pyc -o -name \\*.pyo \\) -prune -o  -type f \\( -name \\* -o -name .\\[\\!.\\]\\* -o -name ..\\?\\* \\) -exec grep --color -n -e " (shell-quote-argument needle) " /dev/null \\{\\} +")))
         (found (length (split-string out "\n" t))))
    (cl-case found
      (0 (message "no matches found"))
      (1 (with-temp-buffer (insert out) (grep-mode) (goto-char (point-min)) (compile-goto-error)))
      (t
       (switch-to-buffer "*yo grep*")
       (setf (buffer-string) "")
       (insert out)
       (grep-mode)
       (goto-char (point-min)))))
  ;; (rgrep needle "*" "." nil)
  )

(provide 'yo-search)
