;;;;;;; experimental stuff ;;;;;;;;;;


(defun elmord-exwm-window-pid (&optional id)
  (when (not id)
    (setq id exwm--id))
  (when (not id)
    (error "Window id not provided and could not be determined"))
  (let ((reply (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id))))
    (slot-value reply 'value)))

(defun elmord-exwm-window-command-info (&optional id)
  (let* ((pid (elmord-exwm-window-pid id))
         (cmdline (with-temp-buffer
                    (insert-file-contents (format "/proc/%d/cmdline" pid))
                    (buffer-string))))
    `((pid . ,pid)
      (command-line
       . ,(split-string (string-remove-suffix "\0" cmdline) "\0"))
      (working-directory
       . ,(file-chase-links (format "/proc/%d/cwd" pid))))))

(defun elmord-exwm-load-evince-info ()
  (let* ((info (elmord-exwm-window-command-info id))
         (pid (cdr (assoc 'pid info)))
         (basename (car (last (cdr (assoc 'command-line info)))))
         (fullname (elmord-find-matching-symlink
                    basename
                    (format "/proc/%d/fd" pid))))
    (setq elmord-evince-file-name fullname)
    (setq elmord-evince-url
          (elmord-read-command-output
           "get_firefox_download_url.py" fullname))
    (setq elmord-evince-title
          (elmord-read-command-output "pdftitle" fullname))
    (setq elmord-evince-sha1
          (car (split-string (elmord-read-command-output "sha1sum" fullname))))))
          

(defun elmord-read-command-output (command &rest args)
    (string-remove-suffix
     "\n"
     (with-temp-buffer
       (apply #'call-process command nil t nil args)
       (buffer-string))))

(defun elmord-find-matching-symlink (file dir)
  (cl-block nil
    (let ((basename (concat "/" (file-name-nondirectory file)))
          (entries (mapcar #'cl-second (directory-files-and-attributes dir t))))
      (dolist (entry entries)
        (when (and (stringp entry)
                   (string-suffix-p basename entry))
          (cl-return entry))))))

(defun elmord-exwm-evince-log ()
  (elmord-exwm-load-evince-info)
  (with-current-buffer (find-file-noselect "~/org/papers.org")
    (let* ((id (concat "sha1-" elmord-evince-sha1))
           (entry (org-find-entry-with-id id)))
      (when (not entry)
        (goto-char (point-max))
        (insert "\n* " elmord-evince-title "\n")
        (org-entry-put (point) "ID" id)
        (org-entry-put (point) "URL" elmord-evince-url)
        (org-entry-put (point) "DATE" (with-temp-buffer
                                        (org-insert-time-stamp (current-time) t t)))
        (save-buffer)))))

(add-hook 'exwm-manage-finish-hook
  (defun elmord-exwm-evince-hook ()
    (when (equal exwm-class-name "Evince")
      (elmord-exwm-evince-log)
      (local-set-key [remap save-buffer] 'elmord-exwm-evince-save)
      )))

(defun elmord-exwm-evince-save ()
  (interactive)
  (let ((filename (read-file-name
                   "Save PDF: "
                   "/home/xyz/Papers/"
                   nil
                   nil
                   (concat elmord-evince-title ".pdf"))))
    (copy-file elmord-evince-file-name filename)
    (with-current-buffer (find-file-noselect "~/org/papers.org")
      (let ((entry (org-find-entry-with-id (concat "sha1-" elmord-evince-sha1))))
        (when entry
          (org-entry-put entry "FILENAME" filename))))))
  
