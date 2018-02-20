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
  (let* ((info (elmord-exwm-window-command-info))
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


;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sgml-mode)

(defmacro -> (&rest forms)
  (if (null (cdr forms))
      (car forms)
    (let ((first (car forms))
          (second (cadr forms))
          (rest (cddr forms)))
      `(-> (,(car second) ,first ,@(cdr second))
           ,@rest))))


(defmacro ->> (&rest forms)
  (if (null (cdr forms))
      (car forms)
    (let ((first (car forms))
          (second (cadr forms))
          (rest (cddr forms)))
      `(->> (,@second ,first)
           ,@rest))))

(defvar elmord-org-blog-post-id)

(defun elmord-org-export-blog-post ()
  (interactive)
  (let ((elmord-org-blog-post-id (car (split-string (file-name-base) "-"))))
    (->> (elmord-org-export-blog-post-initial)
         (elmord-org-export-blog-post-fixup)
         (elmord-org-export-blog-post-add-header)
         )))

(defun elmord-org-export-blog-post-initial ()
  (save-current-buffer
    (let ((org-html-toplevel-hlevel 3)
          (org-export-with-toc nil)
          (org-export-with-section-numbers nil))
      (org-html-export-as-html
       nil ;; async
       nil ;; subtree-p
       nil ;; visible-only
       t   ;; body-only
       nil ;; ext-plist
       )
      )))

(defun elmord-replace-all (regex replacement)
  (beginning-of-buffer)
  (while (re-search-forward regex nil t)
    (replace-match replacement)))

(defun elmord-org-export-blog-post-fixup (output)
  (with-current-buffer output
    (html-mode)
    (beginning-of-buffer)
    (when (search-forward-regexp (rx (and point (1+ "\n"))) nil t)
      (replace-match ""))
    (dolist (regex '("outline-[^\"]*" "text-[^\"]*"))
      (elmord-remove-tags-with-id regex))
    (dolist (replacement `(("<\\(h[1-6]\\) id=\"sec-[^\"]*\"" . "<\\1")
                           ("<p>\n" . "<p>")
                           ("</p>" . "")
                           ("\n\n\n*" . "\n\n")
                           ("<a id=\"fnr\\.\\([0-9]+\\)\" name=\"fnr.\\1\" class=\"footref\" href=\"#fn.\\1\">" .
                            ,(replace-regexp-in-string
                              "@" elmord-org-blog-post-id
                              "<a id=\"ref\\1-@\" name=\"ref\\1-@\" class=\"footref\" href=\"#note\\1-@\">"))
                           ("<a id=\"fn\\.\\([0-9]+\\)\" name=\"fn.\\1\" class=\"footnum\" href=\"#fnr.\\1\">" .
                            ,(replace-regexp-in-string
                              "@" elmord-org-blog-post-id
                              "<a id=\"note\\1-@\" name=\"note\\1-@\" class=\"footnum\" href=\"#ref\\1-@\">"))
                           ("<a href=\"https://\\?" . "<a href=\"?")
                           ))
      (elmord-replace-all (car replacement) (cdr replacement)))
    )
  output)

(defun elmord-remove-tags-with-id (regex)
  (beginning-of-buffer)
  (while (re-search-forward (concat "<[^>]* id=\"" regex "\"") nil t)
    (search-backward "<")
    (save-excursion
      (sgml-skip-tag-forward 1)
      (backward-kill-sexp))
    (kill-sexp)))


;; Partly based on https://emacs.stackexchange.com/questions/21459/programmatically-read-and-set-buffer-wide-org-mode-property
(defun elmord-org-get-buffer-properties ()
  (org-element-map
      (org-element-parse-buffer)
      'keyword
    (lambda (el)
      (cond
       ((equal (org-element-property :key el) "PROPERTY")
        (let ((value (org-element-property :value el)))
          (when (string-match "^\\([^[:space:]]*\\)[[:space:]]+\\(.*\\)" value)
            (cons (match-string 1 value)
                  (match-string 2 value)))))
       ((member (org-element-property :key el) '("TITLE" "DATE"))
        (cons (org-element-property :key el)
              (org-element-property :value el)))))))


(defun elmord-org-export-blog-post-add-header (output)
  (let ((data (elmord-org-get-buffer-properties)))
    (with-current-buffer output
      (beginning-of-buffer)
      (insert
       (format "Title: %s\n" (cdr (assoc "TITLE" data)))
       (format "Created: %s\n" (elmord-org-format-date (cdr (assoc "DATE" data))))
       (format "Tags: %s\n" (cdr (assoc "Tags:" data)))
       "\n"))))

(defun elmord-org-format-date (stamp)
  (format-time-string
   "%Y-%m-%d %H:%M %z"
   (apply 'encode-time (org-parse-time-string stamp))))



;;;;;;;;; org-capture with EXWM buffers.

(defun elmord-exwm-get-firefox-url ()
  (exwm-input--fake-key ?\C-l)
  (sleep-for 0.05)
  (exwm-input--fake-key ?\C-c)
  (sleep-for 0.05)
  (gui-backend-get-selection 'CLIPBOARD 'STRING))

(defun elmord-exwm-org-store-link ()
  (when (and (equal major-mode 'exwm-mode)
             (member exwm-class-name '("Firefox" "Firefox-esr")))
    (org-store-link-props
     :type "http"
     :link (elmord-exwm-get-firefox-url)
     :description exwm-title)))

(add-to-list 'org-store-link-functions 'elmord-exwm-org-store-link)
