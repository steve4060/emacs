;;; TODO(c3bdae31-4329-4217-98a0-743b9dcbb6d2): extract autocommit into a separate package
;;;
;;; Once e266bfaa-2a01-4881-9e7f-ce2c592f7cdd is done, I think we can do that.

(defvar st/autocommit-local-locks
  (make-hash-table :test 'equal))

(defun st/file-truename-nilable (filename)
  (when filename
    (file-truename filename)))

(defun st/autocommit--id ()
  (let ((id (-> default-directory
                (locate-dominating-file ".git")
                (st/file-truename-nilable))))
    (when (not id)
      (error "%s is not inside of a git repository" default-directory))
    (unless (gethash id st/autocommit-local-locks)
      (puthash id nil st/autocommit-local-locks))
    id))

(defun st/autocommit--get-lock (lock)
  (-> (st/autocommit--id)
      (gethash st/autocommit-local-locks)
      (plist-get lock)))

(defun st/autocommit--set-lock (lock value)
  (puthash (st/autocommit--id)
           (-> (st/autocommit--id)
               (gethash st/autocommit-local-locks)
               (plist-put lock value))
           st/autocommit-local-locks))

(defun st/autocommit--toggle-lock (lock)
  (-> lock
      (st/autocommit--get-lock)
      (not)
      (st/autocommit--set-lock)))

(defun st/autocommit--create-dir-locals (file-name)
  (write-region "((nil . ((eval . (st/autocommit-dir-locals)))))"
                nil file-name))

(defun st/y-or-n-if (predicate question action)
  (when (or (not (funcall predicate))
            (y-or-n-p question))
    (funcall action)))

;;; TODO(4229cf9a-4768-4f5e-aca1-865256c64a23): st/autocommit-init-dir should modify dir locals file on AST level
;;;
;;; Right know it just overrides .dir-locals file on text level. I
;;; want it to
;;; - read .dir-locals,
;;; - parse the assoc list,
;;; - check if there is already autocommit stuff
;;; - add autocommit stuff to the assoc list if needed
;;; - and write it back to the file
;;;
;;; That will enable us with modifying dir locals that contains custom
;;; stuff unrelated to autocommit
(defun st/autocommit-init-dir (&optional dir)
  "Initialize autocommit folder."
  (interactive "DAutocommit directory: ")
  (let* ((autocommit-dir (if dir dir default-directory))
         (file-name (concat autocommit-dir
                            dir-locals-file)))
    (st/y-or-n-if (-partial #'file-exists-p file-name)
                  (format "%s already exists. Replace it?" file-name)
                  (-partial #'st/autocommit--create-dir-locals file-name))))

(defun st/autocommit-dir-locals ()
  "The function that has to be put into the .dir-locals.el file
of the autocommit folder as evaluated for any mode."
  (interactive)
  (auto-revert-mode 1)
  (st/autopull-changes)
  (add-hook 'after-save-hook
            'st/autocommit-changes
            nil 'make-it-local))

;;; TODO: st/toggle-autocommit-offline doesn't work correctly
;;;
;;; It should toggle offline for all of the folders at once
(defun st/toggle-autocommit-offline ()
  "Toggle between OFFLINE and ONLINE modes.

Autocommit can be in two modes: OFFLINE and ONLINE. When ONLINE
st/autocommit-changes does `git commit && git push'. When OFFLINE
st/autocommit does only `git commit'."
  (interactive)
  (st/autocommit--toggle-lock 'autocommit-offline)
  (if (st/autocommit--get-lock 'autocommit-offline)
      (message "[OFFLINE] Autocommit Mode")
    (message "[ONLINE] Autocommit Mode")))

(defun st/autopull-changes ()
  "Pull the recent changes.

Should be invoked once before working with the content under
autocommit. Usually put into the dir locals file."
  (interactive)
  (when (not (st/autocommit--get-lock 'autopull-lock))
    (st/autocommit--set-lock 'autopull-lock t)
    (if (st/autocommit--get-lock 'autocommit-offline)
        (message "[OFFLINE] NOT Syncing the Agenda")
      (if (y-or-n-p (format "Sync the Agenda? [%s]" (st/autocommit--id)))
          (progn
            (message (format "Syncing the Agenda [%s]" (st/autocommit--id)))
            (shell-command "git pull"))
        (progn
          (st/autocommit--set-lock 'autocommit-offline t)
          (message (format "[OFFLINE] NOT Syncing the Agenda [%s]"
                           (st/autocommit--id))))))))

(defun st/autocommit-changes ()
  "Commit all of the changes under the autocommit folder.

Should be invoked each time a change is made. Usually put into
dir locals file."
  (interactive)
  (if (st/autocommit--get-lock 'autocommit-lock)
      (st/autocommit--set-lock 'autocommit-changed t)
    (st/autocommit--set-lock 'autocommit-lock t)
    (st/autocommit--set-lock 'autocommit-changed nil)
    (set-process-sentinel (st/run-commit-process (st/autocommit--id))
                          (-partial 'st/autocommit-beat (st/autocommit--id)))))

(defun st/run-commit-process (autocommit-directory)
  (let ((default-directory autocommit-directory))
    (let ((autocommit-message (format-time-string "Autocommit %s")))
      (start-process-shell-command
       (format "Autocommit-%s" autocommit-directory)
       (format "*Autocommit-%s*" autocommit-directory)
       (format (if (st/autocommit--get-lock 'autocommit-offline)
                   "git add -A && git commit -m \"%s\""
                 "git add -A && git commit -m \"%s\" && git push origin master")
               autocommit-message)))))

(defun st/autocommit-beat (autocommit-directory process event)
  (let ((default-directory autocommit-directory))
    (message (if (st/autocommit--get-lock 'autocommit-offline)
                 "[OFFLINE] Autocommit: %s"
               "Autocommit: %s")
             event)
    (if (not (st/autocommit--get-lock 'autocommit-changed))
        (st/autocommit--set-lock 'autocommit-lock nil)
      (st/autocommit--set-lock 'autocommit-changed nil)
      (set-process-sentinel (st/run-commit-process autocommit-directory)
                            (-partial 'st/autocommit-beat autocommit-directory)))))
