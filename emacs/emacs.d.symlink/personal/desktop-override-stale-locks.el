;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here

;(defun emacs-process-p (pid)
;  "If pid is the process ID of an emacs process, return t, else nil.
;Also returns nil if pid is nil."
;  (when pid
;    (let ((attributes (process-attributes pid)) (cmd))
;      (dolist (attr attributes)
;        (if (string= "comm" (car attr))
;            (setq cmd (cdr attr))))
;      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))