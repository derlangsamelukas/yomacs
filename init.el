(when (< (string-to-number emacs-version) 27)
  (load (concat user-emacs-directory "early-init"))
  (package-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(circe-default-nick "lluke")
 '(company-auto-complete nil)
 '(company-minimum-prefix-length 2)
 '(counsel-spotify-client-id "29f6a833a29d4b8eb0b437062443c610")
 '(counsel-spotify-client-secret "aae13f25e70e47cfade44f841683b94d")
 '(custom-enabled-themes '(cyberpunk))
 '(custom-safe-themes
   '("6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bd82c92996136fdacbb4ae672785506b8d1d1d511df90a502674a51808ecc89f" "5d75f9080a171ccf5508ce033e31dbf5cc8aa19292a7e0ce8071f024c6bcad2a" "a5b1d671532f27c524264b433ad58df329297b7bb21966eddda1d385e7a9b055" "2df493c5c7f329eef362290abdcd42a45abad98ffe33f639ecc55af084224e8b" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "87a431903d22fa1cbb2becd88572e7d985e28c2253935448d0d754c13e85a980" "fd1dd4d022ece05400c7bd1efc2ae5cca5cd64a53f3670da49d0c8f0ef41f4e3" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "e1943fd6568d49ec819ee3711c266a8a120e452ba08569045dd8f50cc5ec5dd3" "a95e86f310e90576a64ef75e5be5d8d8dfc051435af3be59c5f8b5b1b60d82c2" "e03d2efd989b91f29ed5f91f160b0054031e6b6574950f7d64792a3acf0c9565" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "725a0ac226fc6a7372074c8924c18394448bb011916c05a87518ad4563738668" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f" "bd51a329aa9b8e29c6cf2c8a8cf136e0d2960947dfa5c1f82b29c9178ad89a27" "d877e1a4d47a671efbd90e5b3c6b64afc05c66aa18a08318da63727db2abaaa7" "41feeef0bef5ef4bac29db2118416d32181801267438ae2321736b2dd30261e5" "45d0b54d516864e8815ce653344ba935117b67550499f5bab04a952da055ff87" "e0e199a0810b32c37021d5197fb71805439ed644552078db01b7270981596ddf" "bd3b647e1c0d5ba5aae4640a2c69efa483b876b9f53661f47729b8774492d36d" "a0866444d7eaee8c081972c439b160cb4d337e98d8c2523c88a97bbdddacabce" "c728d8dc84657ec2a866329d8554377372ba9fc562ef1de36d7ae34ed09132e6" "b4262d9f265d2418275bf1d6515bfc7dc5e8deb30ccce1b00cc8855a1fceb91c" "fff94a9822d54b0753fa7983f5c46e278586f35a7a0c0580d446c21b5e8cacba" "fd77eeef97944af263ad8ec1bc8bb36dfebb3377b5c823549f3ffc6d85818402" "6d39b389b2ee03273a5eb954f368d1292508728a3e0912fbd8aeb0853024017f" "639841500d82c971b60dcc64f22f503e26c32a9f99151c55df5f86721c8d651b" "307e4dcf783d5955e26b4ed47278dba13f8ed942b1f04b16d8e4d71600b8bc3c" "7a52ab1b6270fc1148c02d1a2b26e85182535487ce006edee6813a55a13a9108" "b71ab83076ddf43ddd8fc349fa656439c06ceabd96a9044eec672cf2e7fb89c9" "e4f25abea14db5eb1442eb9bee413c5d806046428713a148ac3fb662b80e3158" "c83338bc595683f52e96d266ee76b0a0a9e644ddd79aad46010aac4a8192a6e4" "c7595b962bdca295ebf883321b28fc70d437c91f0012ecf8d9be984f29adf82d" "e254b86b05de46b1e3ea268ec7d7450f955fe5683c2241fc49d5ea7c5073fcc1" "5abbade0350fe430fe54fe66984499f4f4f2d0da1427d9d0af54d4074e5c1314" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "929a6cd88721dee8a11c1313053542849a9833a9c4024226081b76747752b67d" "eff104046d7c68bbe6ffc62cfc5966b666f093f90dfca72b8c81ca64fb87d07f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" "b65a3bb7dd1c43bf2e301143969a456a5cc380627076196f5529ce8fbf9fb8ac" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "08f5da7e1f5064a2917af94f0dab946adfb25665b25450168ded749ec78a1145" "1ed04f2c712898f2147975dbe30a1a5caaa51bf63ac8fcc1174c4e9c5817c2b6" "1606911c24fbe70270bfbb1223b527746abc5e2f4cb48b04800a1618eab5c046" "39f1ea024bd84e45de3099927eb635ca659919ed3f9974331b6b59d2c9387f85" "56cf939017c38d07895b0f1020b441c02c2b2b8062f03cb745351287d1a9a256" "d146f66c1b992e85482b2f89cee52b527f6ed82f0360ba0743896a1bf648d49d" "1ecb4fe7ca201f13efdd181aa06faa231be2748493d4b5befb2eb911b5813d5b" "44378be188147bd2d63f9be93127db4486be359fcf1f2d91f2b0ae6deaeabc02" "34a28c8bccdbff86e6dbb8ce3e09b5e80e478270539ab6fc31354d0096ee2cdc" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "17cda1304ba8d26d62bf247cab2c161d12957054b6be4477abb5972a74eea4e1" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "839daf67f1e41db2bb2ab9ebd7e2dafb0bf93e8c76f8a3facb876c661b3a5185" "a0f33c18e1759db6be8ab33651a1d5a6da869942572112d3ab09b2d76bdf9c2e" "3ca9264ab0b6497c411ddd998c44fba4eed791a609478686cdf17f3f8a8a00d0" default))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(elfeed-feeds '("https://www.archlinux.org/feeds/news/"))
 '(elm-compile-command '("/home/lukas/Downloads/extracted/elm" "make"))
 '(elm-interactive-command '("/home/lukas/Downloads/extracted/elm" "repl"))
 '(fci-rule-color "gray80")
 '(geiser-chicken-binary "chicken-csi")
 '(highlight-symbol-colors
   '("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F"))
 '(hl-paren-background-colors
   '("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))
 '(hl-paren-colors '("#326B6B"))
 '(idris-interpreter-flags '("-p" "contrib"))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ivy-youtube-play-at "firefox")
 '(js2-ignored-warnings
   '("missing ; after statement" "missing ; before statement" "msg.no.semi.stmt" "msg.missing.semi"))
 '(menu-bar-mode nil)
 '(merlin-command "ocamlmerlin")
 '(org-src-block-faces '(("emacs-lisp" (:background "#F0FFF0"))))
 '(package-selected-packages
   '(scala-mode rjsx-mode auto-overlays all-the-icons-dired all-the-icons ess elm-mode treemacs magit js2-highlight-vars lua-mode neotree nyan-mode dionysos tagedit company-math counsel-spotify spotify soft-morning-theme tommyh-theme electric-case grandshell-theme gotham-theme gitlab exotica-theme ample-theme haskell-mode circe idris-mode dockerfile-mode docker forest-blue-theme kaesar yaml-mode geiser w3 lorem-ipsum clean-aindent-mode ac-etags web-server chicken-scheme chess vala-mode highlight-parentheses ggtags meghanada mvn javadoc-lookup java-imports live-py-mode jedi flycheck-pycheckers anaconda-mode tuareg merlin pixie-mode shen-mode smooth-scrolling cyberpunk-theme melancholy-theme lavender-theme monochrome-theme avk-emacs-themes afternoon-theme company-irony php-eldoc c-eldoc scheme-complete wttrin flappymacs diredful dired-single flycheck tern-auto-complete tern js2-refactor ac-js2 company-web company-statistics company-shell company-quickhelp company-php elfeed paredit flymake-gjshint web-mode moe-theme organic-green-theme foggy-night-theme ace-jump-mode slime show-css restclient xwidgete helm-dash bongo fish-mode counsel ivy-hydra ivy dumb-jump anzu iedit org-pdfview auto-complete-c-headers auto-complete company git wanderlust mbsync mu4e-maildirs-extension markdown-mode php-mode))
 '(scheme-mit-dialect nil)
 '(scheme-program-name "mit-scheme-c")
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(slime-company-complete-in-comments-and-strings t)
 '(slime-company-completion 'fuzzy)
 '(slime-company-major-modes
   '(lisp-mode clojure-mode slime-repl-mode scheme-mode slime-mode slime-repl-mode))
 '(sly-default-lisp nil)
 '(sly-kill-without-query-p t)
 '(smooth-scroll-margin 2)
 '(smooth-scrolling-mode t)
 '(smtpmail-smtp-server "mail.gmx.net")
 '(smtpmail-smtp-service 587)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tuareg-prettify-symbols-full t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")))
 '(vc-annotate-very-old-color nil)
 '(yaml-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1a161f" :foreground "#d3d3d3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "mlss" :family "Anonymous Pro"))))
 '(cursor ((t (:background "#bb099e"))))
 '(custom-button ((t (:background "#383838" :foreground "#4c83ff" :box (:line-width 1 :color "black")))))
 '(font-lock-comment-face ((t (:foreground "#8B8989" :slant normal))))
 '(fringe ((t (:background "#1a161f" :foreground "#dcdccc"))))
 '(js2-external-variable ((t (:foreground "color-197"))))
 '(mode-line ((t (:background "#1a161f" :foreground "#4c83ff" :box nil))))
 '(mode-line-inactive ((t (:background "#1a161f" :foreground "#4D4D4D" :box nil)))))

(load (concat user-emacs-directory "customizations"))
(add-hook
 'after-init-hook
 (lambda ()
   (load (concat user-emacs-directory "after-init"))))
