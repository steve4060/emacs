(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-c C-x j") #'org-clock-jump-to-current-clock)

(setq org-agenda-files (list "~/.Org.d/Todo.org"))

(defun st/org-increment-move-counter ()
  (interactive)

  (defun default (x d)
    (if x x d))

  (let* ((point (point))
         (move-counter-name "MOVE_COUNTER")
         (move-counter-value (-> (org-entry-get point move-counter-name)
                                 (default "0")
                                 (string-to-number)
                                 (1+))))
    (org-entry-put point move-counter-name
                   (number-to-string move-counter-value)))
  nil)

(defun st/org-get-heading-name ()
  (nth 4 (org-heading-components)))

(defun st/org-kill-heading-name-save ()
  (interactive)
  (let ((heading-name (st/org-get-heading-name)))
    (kill-new heading-name)
    (message "Kill \"%s\"" heading-name)))

(global-set-key (kbd "C-x p w") 'st/org-kill-heading-name-save)

(setq org-agenda-custom-commands
      '(("u" "Unscheduled" tags "+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))
        ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))
        ))

;;; org-cliplink

(st/require 'org-cliplink)

(global-set-key (kbd "C-x p i") 'org-cliplink)

(defun st/cliplink-task ()
  (interactive)
  (org-cliplink-retrieve-title
   (substring-no-properties (current-kill 0))
   '(lambda (url title)
      (insert (if title
                  (concat "* TODO " title
                          "\n  [[" url "][" title "]]")
                (concat "* TODO " url
                        "\n  [[" url "]]"))))))
(global-set-key (kbd "C-x p t") 'st/cliplink-task)

;; org-LaTeX

(use-package ox-latex
  :ensure-system-package latexmk
  :ensure nil
  :after org
  :commands (org-export-dispatch)

  :custom
  (org-latex-pdf-process '("latexmk -xelatex -shell-escape -quiet -f %f"))

  (org-latex-sst-block-backend 'listings)
  (org-latex-listings-options
   '(("basicstyle" "\\ttfamily")
     ("showstringspaces" "false")
     ("keywordstyle" "\\color{blue}\\textbf")
     ("commentstyle" "\\color{gray}")
     ("stringstyle" "\\color{green!70!black}")
     ("stringstyle" "\\color{red}")
     ("frame" "single")
     ("numbers" "left")
     ("numberstyle" "\\ttfamily")
     ("columns" "fullflexible")))

  :config
  (add-to-list 'org-latex-logfiles-extensions "tex"))

  (use-package ox-beamer
    :ensure nil
    :after ox-latex)
