(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar st/package-contents-refreshed nil)

(defun st/package-refresh-contents-once ()
  (when (not st/package-contents-refreshed)
    (setq st/package-contents-refreshed t)
    (package-refresh-contents)))

(defun st/require-one-package (package)
  (when (not (package-installed-p package))
    (st/package-refresh-contents-once)
    (package-install package)))

(defun st/require (&rest packages)
  (dolist (package packages)
    (st/require-one-package package)))

(defun st/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (st/require theme-package)
    (load-theme theme t)))

(st/require 'dash)
(require 'dash)

(st/require 'dash-functional)
(require 'dash-functional)
