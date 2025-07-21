(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-type 'relative)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings '((org-agenda-tag-filter-preset (list "+personal"))))
 '(org-agenda-files '("~/.Org.d/Todo.org") t)
 '(org-cliplink-transport-implementation 'url-el)
 '(org-enforce-todo-dependencies nil)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc
              org-mhe org-rmail org-w3m))
 '(org-refile-use-outline-path 'file)
 '(package-selected-packages
   '(ag auctex clojure-mode cmake-mode company csharp-mode d-mode dash
        dash-functional dockerfile-mode editorconfig elpy glsl-mode
        go-mode graphviz-dot-mode gruber-darker-theme haskell-mode
        helm helm-cmd-t helm-git-grep helm-ls-git hindent
        ido-completing-read+ jinja2-mode js2-mode kotlin-mode
        love-minor-mode lua-mode magit markdown-mode move-text
        multiple-cursors nasm-mode nginx-mode nim-mode nix-mode
        org-cliplink org-latex paredit php-mode powershell
        proof-general purescript-mode qml-mode racket-mode
        rainbow-mode rust-mode scala-mode smex tide toml-mode tuareg
        yaml-mode yasnippet))
 '(safe-local-variable-values
   '((eval progn (auto-revert-mode 1) (rc/autopull-changes)
           (add-hook 'after-save-hook 'rc/autocommit-changes nil
                     'make-it-local))))
 '(warning-minimum-level :error)
 '(warning-suppress-log-types '((use-package) (use-package) (use-package)))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty
          space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
