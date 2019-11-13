
;; install missing packages given from variable package-selected-packages (see documentation)
;; (package-install-selected-packages)

;; initialize all global modes

(autopair-global-mode 1)
(global-company-mode)
(ivy-mode 1)
(show-paren-mode)
(smooth-scrolling-mode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fringe-mode 20)
(my-update-mode-line)
(yas-global-mode 1)
(blink-cursor-mode -1)

(when (window-system)
  (yo-set-background-transparent))
