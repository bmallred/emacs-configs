;;;; init-exwm.el                                 -*- lexical-binding: t; -*-

(require 'exwm)
;; (require 'exwm-config)

;; Commented out because I'm using stalonetray instead of the EXWM tray.
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
;; (exwm-config-default)

(require 'exwm-randr)

;;;; Workspaces.
(setq exwm-workspace-number 10)
(setq exwm-workspace-show-all-buffers t)

(when (equal (system-name) "pythagoras")
  ;; Give workspaces 1~5 to my left monitor, and leave 6~0 to my right one.
  (setq exwm-randr-workspace-output-plist
        '(1 "VGA-1"
          2 "VGA-1"
          3 "VGA-1"
          4 "VGA-1"
          5 "VGA-1"))
  (exwm-randr-enable))

;; Update buffer name based on class and title.
(defvar elmord-exwm-buffer-name-limit 50)

(defun elmord-exwm-compute-buffer-name ()
  (let ((class (or exwm-class-name ""))
        (title (or exwm-title "")))
    (cond
     ((and (member class '("Firefox" "Firefox-esr" "Icedove"))
           (string-match "\\`\\(.*\\)\\( - [^-]*\\)\\'" title))
      (concat class ": " (match-string 1 title)))
     ((member class '("Telegram" "TelegramDesktop"))
      (if (equal title "") "Telegram" title))
     ((elmord-exwm-terminal-p class)
      (concat "Term" ": " title))
     ((equal class "skypeforlinux") "Skype")
     (t (concat class ": " title)))))

(defun elmord-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer
   (truncate-string-to-width
    (elmord-exwm-compute-buffer-name)
    elmord-exwm-buffer-name-limit
    nil  ; start
    nil  ; padding
    "…")
    ))

(add-hook 'exwm-update-class-hook 'elmord-exwm-update-buffer-name)
(add-hook 'exwm-update-title-hook 'elmord-exwm-update-buffer-name)

;; Pass on C-u intact to applications.
(setq exwm-input-prefix-keys
      (delq ?\C-u exwm-input-prefix-keys))

;;;; Global bindings.
(exwm-input-set-key (kbd "s-r") 'rename-buffer)
(exwm-input-set-key (kbd "s-R") 'exwm-reset)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
(exwm-input-set-key (kbd "s-q") 'delete-window)
(exwm-input-set-key (kbd "s-k") 'kill-buffer)

;; Switch workspace with Super+number
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
  (defun elmord-exwm-shell-command (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process "exwm-subproc" nil "nohup" "sh" "-c" command)))

;;;; EXWM line mode bindings.
;; C-x C-x to send C-x, C-c C-c to send C-c.
(define-key exwm-mode-map (kbd "C-x C-x")
  (lambda () (interactive) (exwm-input--fake-key ?\C-x)))
(define-key exwm-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (exwm-input--fake-key ?\C-c)))

;; Line-editing shortcuts
;; (exwm-input-set-simulation-keys
;;  '(([?\C-b] . left)
;;    ([?\C-f] . right)
;;    ([?\C-p] . up)
;;    ([?\C-n] . down)
;;    ([?\C-a] . home)
;;    ([?\C-e] . end)
;;    ([?\M-v] . prior)
;;    ([?\C-v] . next)
;;    ([?\C-d] . delete)
;;    ([?\C-k] . (S-end delete))))

(defun elmord-exwm-next-workspace (&optional incr)
  (interactive "p")
  (when (not incr) (setq incr 1))
  (let* ((count (exwm-workspace--count))
         (index (mod (+ exwm-workspace-current-index incr) count)))
    (exwm-workspace-switch index)))

(defun elmord-exwm-previous-workspace (&optional incr)
  (interactive "p")
  (when (not incr) (setq incr 1))
  (elmord-exwm-next-workspace (- incr)))

(exwm-input-set-key (kbd "s-<left>") 'elmord-exwm-previous-workspace)
(exwm-input-set-key (kbd "s-<right>") 'elmord-exwm-next-workspace)

(defun elmord-exwm-buffer-list (&optional predicate)
  (cl-remove-if-not
   (lambda (buffer)
     (and (eq (buffer-local-value 'major-mode buffer) 'exwm-mode)
          (if predicate (funcall predicate buffer) t)))
   (buffer-list)))

(defun elmord-ido-read-exwm-buffer (prompt)
  "Read an EXWM buffer name from the user."
  (let ((buffers (elmord-exwm-buffer-list)))
    (ido-completing-read prompt (mapcar 'buffer-name buffers))))

(defun elmord-ido-read-buffer-maybe-exwm (pre-prompt only-exwm)
  "Read a buffer name from the user. if ONLY-EXWM is non-nil,
only present EXWM buffers as options."
  (let ((prompt (format "%s%s: " pre-prompt (if only-exwm " (EXWM)" "")))
        (prompter (if only-exwm 'elmord-ido-read-exwm-buffer 'ido-read-buffer)))
    (funcall prompter prompt)))

(defun elmord-exwm-ido-jump-to-buffer (only-exwm)
  "Jump to the workspace containing an EXWM buffer."
  (interactive "P")
  (let ((bufname (elmord-ido-read-buffer-maybe-exwm "Jump to buffer" only-exwm)))
    (exwm-workspace-switch-to-buffer bufname)))

(defun elmord-exwm-ido-bring-buffer (only-exwm)
  "Bring an EXWM buffer to the current workspace."
  (interactive "P")
  (let* ((bufname (elmord-ido-read-buffer-maybe-exwm "Bring buffer" only-exwm))
         (id (exwm--buffer->id (get-buffer bufname))))
    (when id
      ;; EXWM buffer: move to current workspace.
      (exwm-workspace-move-window exwm-workspace-current-index id))
    ;; Whether EXWM buffer or not, bring it up.
    (switch-to-buffer bufname)))

(defun elmord-exwm-ido-jump-to-exwm-buffer ()
  (interactive)
  (elmord-exwm-ido-jump-to-buffer t))

(defun elmord-exwm-ido-bring-exwm-buffer ()
  (interactive)
  (elmord-exwm-ido-bring-buffer t))

(exwm-input-set-key (kbd "s-j") 'elmord-exwm-ido-jump-to-buffer)
(exwm-input-set-key (kbd "s-J") 'elmord-exwm-ido-jump-to-exwm-buffer)
(exwm-input-set-key (kbd "s-b") 'elmord-exwm-ido-bring-buffer)
(exwm-input-set-key (kbd "s-B") 'elmord-exwm-ido-bring-exwm-buffer)

(display-time-mode 1)

;; Shortcuts.

(defun elmord-exwm-shortcut (key command)
  (exwm-input-set-key key
    `(lambda ()
       (interactive)
       (start-process "exwm-shortcut" nil "sh" "-c" ,command))))

(elmord-exwm-shortcut (kbd "s--") "amixer set Master 3%-")
(elmord-exwm-shortcut (kbd "s-=") "amixer set Master 3%+")
(elmord-exwm-shortcut (kbd "s-\\") "musicctl play")
(elmord-exwm-shortcut (kbd "s-(") "musicctl prev")
(elmord-exwm-shortcut (kbd "s-)") "musicctl next")

(elmord-exwm-shortcut (kbd "<XF86AudioLowerVolume>") "amixer set Master 3%-")
(elmord-exwm-shortcut (kbd "<XF86AudioRaiseVolume>") "amixer set Master 3%+")
(elmord-exwm-shortcut (kbd "<XF86AudioPlay>") "musicctl play")

(elmord-exwm-shortcut (kbd "s-x") "sleep 0.2; xset dpms force suspend")
(elmord-exwm-shortcut (kbd "s-<prior>") "backlight-brightness +")
(elmord-exwm-shortcut (kbd "s-<next>") "backlight-brightness -")

;;(elmord-exwm-shortcut (kbd "s-t") "x-terminal-emulator")
(elmord-exwm-shortcut (kbd "s-L") "gnome-screensaver-command -l")


(defun elmord-scrot ()
  (interactive)
  (let* ((filename (expand-file-name (format-time-string "~/%Y%m%d_%H%M%S_scrot.png")))
         (process (start-process "exwm-scrot" "*exwm-scrot*"
                                 "bash" "-c" (concat "sleep 0.5; scrot -s " filename))))
    (message "Select rectangle for screenshot...")
    (set-process-sentinel process
      (lambda (proc status)
        (cond ((equal status "finished\n")
               (gui-backend-set-selection 'CLIPBOARD filename)
               (message "Screenshot saved in %s" filename))
              (t
               (message "scrot: %s" (string-trim status))))))))

(exwm-input-set-key (kbd "<print>") 'elmord-scrot)


;;;; Start pidgin in background

(fset '~exwm-floating--unset-floating (symbol-function 'exwm-floating--unset-floating))
(fset '~exwm-layout--iconic-state-p (symbol-function 'exwm-layout--iconic-state-p))

;; This is a terrible kludge to make EXWM think the window is
;; iconified when it is created.
(defadvice exwm-manage--manage-window (around elmord-start-in-background activate)
  (cl-letf (((symbol-function 'exwm-floating--unset-floating)
             (lambda (id)
               (cl-letf (((symbol-function 'exwm-layout--iconic-state-p)
                          (lambda (&optional id)
                            (or (~exwm-layout--iconic-state-p id)
                                (elmord-exwm-start-in-background-p id)))))
                 (~exwm-floating--unset-floating id)))))
    ad-do-it
    (exwm-layout--refresh)))

(defun elmord-exwm-start-in-background-p (id)
  ;; TODO: Do whatever other window managers do to figure out that a
  ;; window should be started in background, rather than tying it to Pidgin.
  (with-current-buffer (if id (exwm--id->buffer id) (current-buffer))
    (equal exwm-class-name "Pidgin")))


;;;; Urgency handling.


;; Telegram notifications hack.
;; (defun elmord-exwm-telegram-title-hook ()
;;     ;;    (when (equal exwm-class-name "TelegramDesktop")
;;     (run-hooks 'elmord-exwm-update-hints-hook)))


(defun elmord-exwm-telegram-urgent-p (buffer)
  (and (equal (buffer-local-value 'exwm-class-name buffer) "TelegramDesktop")
       (string-match "([0-9]+)" (or (buffer-local-value 'exwm-title buffer) ""))))

(defun elmord-exwm-evolution-urgent-p (buffer)
  (and (equal (buffer-local-value 'exwm-class-name buffer) "Evolution")
       (string-match "([0-9]+ unread)" (or (buffer-local-value 'exwm-title buffer) ""))))

(defun elmord-exwm-chromium-skype-urgent-p (buffer)
  (and (equal (buffer-local-value 'exwm-class-name buffer) "Chromium-browser")
       (string-match "([0-9]+) Skype" (or (buffer-local-value 'exwm-title buffer) ""))))

(defun elmord-exwm-buffer-urgent-p (buffer)
  (or
   (buffer-local-value 'exwm--hints-urgency buffer)
   ;;(elmord-exwm-telegram-urgent-p buffer)
   (elmord-exwm-evolution-urgent-p buffer)
   ;;(elmord-exwm-chromium-skype-urgent-p buffer)
  ))

(defun elmord-exwm-urgent-buffer-list ()
  (elmord-exwm-buffer-list 'elmord-exwm-buffer-urgent-p))

(defun elmord-exwm-jump-to-urgent-buffer ()
  "Jump to the first urgent window."
  (interactive)
  (let ((urgent-buffers (elmord-exwm-urgent-buffer-list)))
    (if (not urgent-buffers)
        (message "No urgent buffers")
      (let ((buffer (car urgent-buffers)))
        (exwm-workspace-switch-to-buffer buffer)
        (elmord-update-mode-line-exwm-urgency)))))

;; When visiting a window, remove its urgency.
(add-hook 'buffer-list-update-hook
  (defun elmord-exwm-remove-urgency ()
    (when (and (eq major-mode 'exwm-mode)
               (eq (current-buffer) (window-buffer))
               exwm--hints-urgency)
      (setf exwm--hints-urgency nil)
      (run-hooks 'elmord-exwm-update-hints-hook))))

(exwm-input-set-key (kbd "s-u") 'elmord-exwm-jump-to-urgent-buffer)

;; A hook for tracking when windows become urgent.
(defvar elmord-exwm-update-hints-hook nil
  "Normal hook run when window hints are updated.")

(defadvice exwm--update-hints (after elmord-update-hints-hook activate)
  (run-hooks 'elmord-exwm-update-hints-hook))

;; Indicate urgent windows in the mode line.
(defvar elmord-mode-line-exwm-urgency "")
(put 'elmord-mode-line-exwm-urgency 'risky-local-variable t)
(setq mode-line-misc-info
      (append mode-line-misc-info (list 'elmord-mode-line-exwm-urgency)))

(defun elmord-update-mode-line-exwm-urgency ()
  (let ((text (string-join
               (mapcar 'buffer-name (elmord-exwm-urgent-buffer-list))
               ", ")))
    (setq elmord-mode-line-exwm-urgency
          `(:propertize ,text
                        face
                        (:foreground "white"
                         :background "#0000a0")))
    (force-mode-line-update t)))

(add-hook 'elmord-exwm-update-hints-hook 'elmord-update-mode-line-exwm-urgency)
(add-hook 'exwm-update-title-hook 'elmord-update-mode-line-exwm-urgency)


;; Make C-x 8 RET work with any program supporting paste from clipboard with C-v.

(defun elmord-exwm-get-paste-key ()
  (cond ((member exwm-class-name '("XTerm" "Xfce4-terminal"))
         (aref (kbd "S-C-v") 0))
        (t ?\C-v)))

(defun elmord-exwm-send-string (string)
  (interactive "MSend string: ")
  (let ((saved-clipboard (gui-backend-get-selection 'CLIPBOARD 'STRING))
        (paste-key (elmord-exwm-get-paste-key)))
    (gui-backend-set-selection 'CLIPBOARD string)
    (run-at-time 0.1 nil
      `(lambda ()
         (exwm-input--fake-key ,paste-key)
         (run-at-time 0.3 nil
           `(lambda ()
              (gui-backend-set-selection 'CLIPBOARD ,,saved-clipboard))))))
  t)

(defun elmord-exwm-send-character (char)
  (interactive
   ;; Copied from `insert-char' source.
   (list (read-char-by-name "Insert character for EXWM (Unicode name or hex): ")))
  (elmord-exwm-send-string (string char)))

(define-key exwm-mode-map (kbd "C-x 8 RET") 'elmord-exwm-send-character)

;; Much better (but with possible unknown side-effects?): allow the
;; buffer to be modified, and transfer any modification to the
;; application via clipboard.

;; (defun elmord-exwm-after-change-function (begin end length)
;;   (elmord-exwm-send-string (buffer-substring begin end))
;;   (delete-region begin end)
;;   (set-buffer-modified-p nil))


(defun elmord-remove-from-tree (item tree)
  (cond
   ((and (consp tree)
         (eq (car tree) item))
    (elmord-remove-from-tree item (cdr tree)))
   ((consp tree)
    (cons (elmord-remove-from-tree item (car tree))
     (elmord-remove-from-tree item (cdr tree))))
   (t tree)))


(add-hook 'exwm-manage-finish-hook
  (defun elmord-exwm-manage-finish-hook ()
    ;; We don't need line position in an EXWM buffer.
    (setq-local mode-line-format
                (elmord-remove-from-tree 'mode-line-position mode-line-format))
    (setq buffer-read-only nil)
    (add-hook 'after-change-functions 'elmord-exwm-after-change-function nil t)
    (add-hook 'kill-buffer-hook 'elmord-exwm-kill-buffer-hook nil t)
    (when (elmord-exwm-terminal-p)
      (add-hook 'exwm-update-title-hook 'elmord-exwm-terminal-cd))))

(defun elmord-exwm-terminal-p (&optional class-name)
  (member (or class-name exwm-class-name)
          '("Xfce4-terminal" "X-terminal-emulator")))

(defun elmord-exwm-terminal-cd ()
  (when (string-match "\\`<\\([^>]*\\)>" exwm-title)
    (cd (match-string 1 exwm-title))))

(defun elmord-exwm-kill-buffer-hook ()
  (let ((window (get-buffer-window (current-buffer) 'visible)))
    (when window
      (condition-case err
          (delete-window window)
        (error (unless (equal (cdr err)
                              '("Attempt to delete minibuffer or sole ordinary window"))
                 (signal (car err) (cdr err))))))))

(setq elmord-exwm-class-workspace-alist
      '(("Firefox" . 0)
        ("Firefox-esr" . 0)
        ("skypeforlinux" . 9)
        ("Chromium-browser" . 9)
        ("Chromium" . 9)
        ("Evolution" . 8)
        ("Icedove" . 8)
        ("Thunderbird" . 8)
        ("Telegram" . 7)
        ("TelegramDesktop" . 7)
        ("Pidgin" . 6)))

(defun elmord-exwm-auto-set-workspace ()
  (when (not exwm--floating-frame)
    (let ((workspace (cdr (assoc exwm-class-name
                                 elmord-exwm-class-workspace-alist))))
      (when workspace
        (let ((current-workspace exwm-workspace-current-index))
          (exwm-workspace-move-window workspace exwm--id)
          (message "Moved window %S to workspace %S"
                   exwm-class-name workspace))))))

(defun elmord-exwm-fixup-window-workspaces ()
  (interactive)
  (dolist (buffer (elmord-exwm-buffer-list))
    (with-current-buffer buffer
      (elmord-exwm-auto-set-workspace))))


;; (add-hook 'exwm-init-hook
;;   (defun elmord-exwm-init-hook ()
;;     ;; Fix workspace of already existing windows.
;;     ;; Fix it for windows created afterwards.
;;     (add-hook 'exwm-manage-finish-hook 'elmord-exwm-auto-set-workspace)))

;; (setq debug-on-error t)

(add-hook 'exwm-manage-finish-hook 'elmord-exwm-auto-set-workspace)


(defun elmord-exwm-window-chromium-p ()
  (member exwm-class-name '("Chromium" "Chromium-browser")))

(defun elmord-exwm-set-urgency-from-title ()
  (when (and (elmord-exwm-window-chromium-p)
             (string-match "([0-9]+) Skype" exwm-title)
             (not (eq (current-buffer) (window-buffer))))
    (setq exwm--hints-urgency t)
    (elmord-update-mode-line-exwm-urgency)))

(add-hook 'exwm-manage-finish-hook
          (defun elmord-exwm-chromium-skype-urgent-hook ()
            (when (elmord-exwm-window-chromium-p)
              (add-hook 'exwm-update-title-hook
                        'elmord-exwm-set-urgency-from-title
                        t ;; local
                        ))))

(defun elmord-exwm-switch-or-open-other-window (selector command)
  (let ((candidates (cl-remove-if-not
                     (lambda (buffer)
                              (with-current-buffer buffer
                                (and (equal major-mode 'exwm-mode)
                                     (funcall selector))))
                     (buffer-list))))
    (if candidates
        (progn
          (switch-to-buffer-other-window (car candidates))
          (exwm-workspace-move-window
           exwm-workspace-current-index
           (buffer-local-value 'exwm--id (car candidates)))
          )
      (switch-to-buffer-other-window "*scratch*")
      (start-process "exwm-shortcut" nil "sh" "-c" command))))


(exwm-input-set-key (kbd "s-t")
  (lambda ()
    (interactive)
    (elmord-exwm-switch-or-open-other-window
     'elmord-exwm-terminal-p "x-terminal-emulator")))

(exwm-input-set-key (kbd "s-v")
  (lambda ()
    (interactive)
    (elmord-exwm-switch-or-open-other-window
     (lambda () (equal exwm-class-name "vlc"))
     "vlc")))

;; (exwm-input--update-global-prefix-keys)
