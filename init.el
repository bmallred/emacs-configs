;; init.el                                     -*- lexical-binding: t; -*-

;;;; Packages.
(require 'package)
;; Make sure to use HTTPS.
(setq package-archives
      ;; '("melpa-stable" . "http://melpa-stable.org/packages/")
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Check TLS (https://glyph.twistedmatrix.com/2015/11/editor-malware.html)
(setq tls-checktrust t)
(setq gnutls-verify-error t)

;;;; Basic configs.
(require 'cl-lib)
(require 'subr-x)
(load-library "dired-x")

;; Define C-z as a new prefix key; because it usually is a standard Emacs
;; keybinding, no mode uses it, so we can have it for ourselves without fear
;; of conflicts. (Also, C-z is very convenient to type.) Suspending can still
;; be done with C-x C-z.
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)

;;;;;; Backups.

;; Save backup files in ~/backups instead of the file's directory.
(mkdir "~/.emacs.d/backups" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; Likewise for autosave files.
(mkdir "~/.emacs.d/autosave" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;;;;;; Enabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;; Interactive commands.

;;;;; Kill all unmodified buffers.

(defun elmord-kill-unmodified-buffers ()
  "Kill all unmodified buffers. Ignores EXWM buffers."
  (interactive)
  (let ((killed nil))
    (dolist (buffer (buffer-list))
      (when (not (or (buffer-modified-p buffer)
                     (equal (buffer-name buffer) "*scratch*")
                     (elmord-exwm-buffer-p buffer)))
	(push (buffer-name buffer) killed)
	(kill-buffer buffer)))
    (message "%d buffers killed: %s" (length killed) killed)))

(defun elmord-exwm-buffer-p (buffer)
  (eq (buffer-local-value 'major-mode buffer)
      'exwm-mode))

(global-set-key (kbd "C-x K") 'elmord-kill-unmodified-buffers)

;;;;; Shift+Insert pastes from primary selection, like xterm.

(defun elmord-primary-selection-yank ()
  "Yanks text from the primary selection (the text you just marked with
the mouse in a graphical environment), as opposed to the clipboard (the
place things end up in when you copy with Ctrl+C)."
  (interactive)
  (let ((x-select-enable-clipboard nil)
	(x-select-enable-primary t))
    (yank)))

(global-set-key (kbd "S-<insert>") 'elmord-primary-selection-yank)

;;;;; Insert current date.

(defun elmord-insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M %z")))

(define-key ctl-z-map (kbd "d") 'elmord-insert-current-date)

;;;;; Visit config file.

(defun elmord-visit-init-file ()
  (interactive)
  (find-file user-init-file))

(define-key ctl-z-map (kbd "e") 'elmord-visit-init-file)

;;;;; ESC to quit temporary windows.

;; Keep track of which buffers are temporary.
(add-hook 'temp-buffer-window-setup-hook
	  (defun elmord-mark-buffer-as-temporary ()
	    (setq-local elmord-buffer-temporary-p t)))

(defvar elmord-temporary-buffer-names
  '("*Messages*" "*Completions*" "*Occur*")
  "Names of buffers which are always considered temporary, regardless of
the value of their value of `buffer-temporary-p'.")

(defun elmord-window-temporary-p (window)
  "Returns whether WINDOW is associated with a temporary buffer."
  (with-current-buffer (window-buffer window)
    (or (and (boundp 'elmord-buffer-temporary-p) elmord-buffer-temporary-p)
	(member (buffer-name) elmord-temporary-buffer-names))))

(defvar elmord-quit-temp-windows-previous-configuration nil)

(defun elmord-quit-temp-windows-or-delete-last-window (undo-p)
  "Quit all temporary buffers in the current frame. If no such buffer exists,
delete the last window in the current frame (defined by the order of
`window-list'), excluding the current one. With a prefix, undo the efect of
the last call. Returns t if a window was quitted/deleted, nil otherwise."
  (interactive "P")
  (if undo-p
      (set-window-configuration elmord-quit-temp-windows-previous-configuration)
    (let ((winconf (current-window-configuration)))
      (cond ((or (elmord-quit-temp-windows)
                 (elmord-delete-last-window))
	     (setq elmord-quit-temp-windows-previous-configuration winconf)
	     t)
	    (t (message "No window to delete")
	       nil)))))

(defun elmord-quit-temp-windows ()
  "Quit all temporary buffers in the current frame.
 Returns t is any window was quitted, nil otherwise."
  (interactive)
  (let ((some-quitted-p nil))
    (dolist (window (window-list))
      (when (elmord-window-temporary-p window)
	(quit-window nil window)
	(setq some-quitted-p t)))
    some-quitted-p))

(defun elmord-delete-last-window ()
  "Delete the last window in the current frame (defined by the order of
`window-list', excluding the current window. Returns t if a window was
deleted, nil otherwise."
  (interactive)
  (let* ((windows (remove (get-buffer-window) (window-list)))
	 (victim (car (last windows))))
    (when victim
      (delete-window victim)
      t)))
(global-set-key (kbd "<escape>") 'elmord-quit-temp-windows-or-delete-last-window)

;; In minibuffer, ESC behaves the same as C-g.
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

;;;;; C-u kills line backward (like in terminal/Vim).

(defvar elmord-backward-kill-line-preserve-indentation t
  "Whether `backward-kill-line' should avoid deleting indentation.")

(defun elmord-backward-kill-line (&optional prefix)
  "Kill from point to the beginning of the line.

If `elmord-backward-kill-line-preserve-indentation' is non-nil,
only kill up to indentation, unless point is already at
indentation, in which case kill to the beginning of the line.

With a prefix N, just calls (`kill-line' -N)."
  (interactive "P")
  (if prefix
      (kill-line (- prefix))
    (if (bolp)
	;; What to do if we are at the beginning of line?
	;; Currently, delete the previous newline, like Vim.
	(backward-delete-char 1 t)
      (if elmord-backward-kill-line-preserve-indentation
	  (elmord-backward-kill-line-preserving-indentation)
	(kill-line 0)))))

(defun elmord-backward-kill-line-preserving-indentation ()
  (let ((indentation (save-excursion (back-to-indentation) (point))))
    (if (<= (point) indentation)
	(kill-line 0)
      (kill-region (point) indentation))))

(global-set-key "\C-u" 'elmord-backward-kill-line)

;;;;; Move universal argument to C-v (daring proposition!).
(global-set-key "\C-v" 'universal-argument)
(define-key universal-argument-map "\C-u" nil)
(define-key universal-argument-map "\C-v" 'universal-argument-more)

;;;;; Other edit commands.
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

(defun elmord-backward-kill-char ()
  (interactive)
  (backward-delete-char-untabify 1 t))

(global-set-key (kbd "S-<backspace>") 'elmord-backward-kill-char)


;;;; Appearance and stuff.

(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)

(setq history-length 500)
(savehist-mode 1)

;; Show clock in mode line, but only in full-screen mode.
(setq display-time-default-load-average nil)
(setq display-time-mail-file 'none)
(setq display-time-24hr-format t)

(defun toggle-frame-fullscreen* ()
  (interactive)
  (toggle-frame-fullscreen)
  (if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
      (display-time-mode 1)
    (display-time-mode 0)))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen*)

;; More mode line stuff
(column-number-mode 1)
(size-indication-mode 1)

;;;; Window management.

;; -*- lexical-binding: t; -*-

;; Changing window focus.
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)

;; Breaks arrows in terminals :(
;;(global-set-key (kbd "M-O") 'other-window)

;; C-x b should offer the last active buffer as the default option,
;; even if it is visible in another window.
(defadvice other-buffer (around allow-visible-buffers)
  (ad-set-arg 1 t)
  ad-do-it)

(ad-activate 'other-buffer)

(define-prefix-command 'ctl-2-map)
(global-set-key (kbd "C-2") 'ctl-2-map)
(global-set-key (kbd "C-2 C-2") 'split-window-below)
(global-set-key (kbd "C-2 C-f") 'find-file-other-window)
(global-set-key (kbd "C-2 f") 'find-file-other-window)
(global-set-key (kbd "C-2 C-b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-2 b") 'switch-to-buffer-other-window)


;;;; Completion behavior.

;;;;; Do not auto-size completion windows.

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window
                display-buffer-pop-up-window
                display-buffer-use-some-window)
               (inhibit-same-window . t)
               (window-height . ignore) )
         )

(defadvice minibuffer-completion-help (around elmord-do-not-fit-window activate)
  (cl-letf (((symbol-function 'fit-window-to-buffer) #'ignore))
    ad-do-it))

(defadvice completion-at-point (around elmord-do-not-fit-window activate)
  (cl-letf (((symbol-function 'fit-window-to-buffer) #'ignore))
    ad-do-it))


;;;; Programming modes.

;;;;; Tabs and spaces.

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Cleanup trailing whitespace.
(add-hook 'prog-mode-hook
  (defun elmord-prog-mode-hook ()
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(add-hook 'sgml-mode-hook
  (defun elmord-sgml-mode-hook ()
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)))


;;;;; Compilation.

(defun elmord-compile-prompt-only-once (&optional read-command-p)
  "Invoke `compile' on the current buffer. Only ask for the compilation command
the first time it is called, or if a prefix argument is used."
  (interactive "P")
  (when read-command-p
    (setq-local compilation-read-command t))
  (call-interactively 'compile)
  (setq-local compilation-read-command nil))

(global-set-key (kbd "C-z C-z") 'elmord-compile-prompt-only-once)

(setq compilation-scroll-output t)
(setq compilation-ask-about-save nil)

;;;;; Scheme.

;; Indent Scheme 'if' like Emacs Lisp (I'm a barbarian like that).
(put 'if 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)

;; Geiser.
(setq-default geiser-scheme-implementation 'guile)

(show-paren-mode 1)

;;;;; C.

;; C indentation.

(defvar elmord-c-style
  '("java"  ;; Inherit from this style
    (c-offsets-alist
     (case-label . 4)
     (statement-case-intro . 4)
     (statement-case-open . 4)
     (label . 2))))

(c-add-style "elmord" elmord-c-style)
(setq c-default-style "elmord")    

;;;; Text modes.

;; word-wrap-mode: like visual-line-mode, but without the visual line part.

(define-minor-mode word-wrap-mode
  "Wrap lines at word boundaries."
  nil nil nil
  (cond (word-wrap-mode ;; enable
	 (setq word-wrap t)
	 (set (make-local-variable 'fringe-indicator-alist)
	      (copy-alist fringe-indicator-alist))
	 (rplacd (assoc 'continuation fringe-indicator-alist) nil)
	 (message "Word-Wrap mode enabled"))
	(t ;;disable
	 (setq word-wrap nil)
	 (kill-local-variable 'fringe-indicator-alist)
	 (message "Word-Wrap mode disabled"))))

(add-hook 'text-mode-hook 'word-wrap-mode)

;;;; Argh.
(defun elmord-new-line-after ()
  (interactive)
  (end-of-line)
  (newline nil t))

(defun elmord-new-line-before ()
  (interactive)
  (previous-line)
  (elmord-new-line-after))

(global-set-key "\C-o" 'elmord-new-line-after)
(global-set-key "\M-o" 'elmord-new-line-before)


;; https://stackoverflow.com/questions/2471557/how-to-undo-fill-paragraph-in-emacs

(defun elmord-unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun elmord-unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;;; Unclever indent.

;; Another attempt at unclever indent: changing what RET does

(defun elmord-newline-and-unclever-indent ()
  (interactive)
  (let (indent-start indent-end)
    (save-excursion
      (beginning-of-line)
      (setq indent-start (point))
      (back-to-indentation)
      (setq indent-end (point)))
    (newline)
    (insert-buffer-substring (current-buffer) indent-start indent-end)
    ;; Special case: if RET is type twice in sequence, remove the
    ;; indentation automatically added by the previous RET.
    (when (eq last-command 'elmord-newline-and-unclever-indent)
      (delete-region indent-start indent-end))
    ))

(define-minor-mode unclever-indent-mode
  ""					; docstring
  nil					; default value
  " Unclever"				; mode line message
  '(("\t" . tab-to-tab-stop)
    ("\r" . elmord-newline-and-unclever-indent)
    ("\d" . backward-delete-char))
  (cond
   (unclever-indent-mode		; enable
    (setq-local electric-indent-inhibit t)
    (message "Unclever Indent mode enabled"))
   (t					; disable
    (kill-local-variable 'electric-indent-inhibit)
    (message "Unclever Indent mode disabled"))))

(global-set-key (kbd "C-z i") 'unclever-indent-mode)

;;;; Search.

;; Automatically wrapping I-search.
;; https://stackoverflow.com/questions/285660/automatically-wrapping-i-search
;; TODO: Still not perfect: does not distinguish overwrapped I-search anymore.
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    ;; Avoid recursive loop
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search) ;; ad-activate to reflect the above change
    ;; Repeat the search (in the appropriate direction)
    (isearch-repeat (if isearch-forward 'forward))
    ;; Restore advice
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; The above solution has a serious problem: it creates a spurious "input event"
;; when it implicitly repeats the search. This means you have to type DEL
;; *twice* to undo the last command. I should find a proper solution to this,
;; but the fact is that I don't generally want DEL to undo last event anyway, I
;; want it to remove the last characher from the search (what C-M-w does (yeah,
;; really). So instead I'll redefine DEL (which apparently has to be called
;; <backspace> for the graphical Emacs) to remove last character from search
;; query, and the undo key to undo the last search event. This leaves the undo
;; key with the aforementioned bug, but whatever.

(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-/") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-_") 'isearch-delete-char)


(defun elmord-isearch-all-buffers ()
  (interactive)
  (multi-isearch-buffers (buffer-list)))

(global-set-key (kbd "M-s M-s") 'elmord-isearch-all-buffers)

;;;; Shell and comint.

;;;;;; Shell/Term-mode alternation (http://emacswiki.org/emacs/ShellMode)

;; (require 'shell)
;; (require 'term)

(defun elmord-term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
	;;(local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
	)
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l"))
      )))

(add-hook 'shell-mode-hook
  (defun elmord-shell-mode-hook ()
    ;; By default, Emacs ignores files with some extensions when completing filenames
    ;; (mainly object files such as .o). This should not apply to shell completion.
    (make-local-variable 'completion-ignored-extensions)
    (setq completion-ignored-extensions nil)
    ;; Fix process window size (SIGWINCH) when it changes.
    (add-hook 'window-configuration-change-hook 'elmord-comint-fix-window-size)
    (compilation-shell-minor-mode)))

;; https://stackoverflow.com/questions/7987494/emacs-shell-mode-display-is-too-wide-after-splitting-window/11255996#11255996
;; Adapted because Emacs kept calling it after the process died.
(defun elmord-comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
	(set-process-window-size process
				 (window-height)
				 (window-width))))))

;; Don't create a new window for M-x shell, just like Emacs 24
;; (code from Emacs 25.1 NEWS file).
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" . (display-buffer-same-window)))

(eval-after-load 'comint
  (defun elmord-comint-eval-after-load ()
    (define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
    (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
    (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
    (add-hook 'comint-output-filter-functions 'force-mode-line-update)
    ))

;; Make prompt read-only.
(defun elmord-comint-backspace-no-delete-prompt (n)
  (interactive "p")
  (cond ((and (> (point) 1)
              (elmord-is-or-contains
               'comint-highlight-prompt
               (get-char-property (1- (point)) 'font-lock-face (current-buffer))))
         (message "Not stepping over prompt"))
        (t (delete-backward-char n))))

(defun elmord-is-or-contains (item list)
  (or (eq item list)
      (member item list)))

(eval-after-load 'shell
  (defun elmord-shell-eval-after-load ()
    (define-key shell-mode-map (kbd "DEL") 'elmord-comint-backspace-no-delete-prompt)
    (setq shell-dynamic-complete-functions
          (remove 'pcomplete-completions-at-point shell-dynamic-complete-functions))))

;; Hmm.

(setq frame-title-format '("%b (" default-directory ")"))

(setenv "GIT_MAN_VIEWER" "woman")
(setenv "GIT_PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(setq comint-input-ring-size 10000)

;;;; Web surfing.

;; w3m.
(eval-after-load 'w3m
  (defun elmord-w3m-eval-after-load ()
    (require 'w3m-search)
    (add-to-list 'w3m-search-engine-alist
                 '("startpage"
                   "https://startpage.com/do/search"
                   utf-8
                   "query=%s&cat=web"))
    (setq w3m-search-default-engine "startpage")))


(global-set-key (kbd "C-z C-f") 'browse-url-emacs)

(add-hook 'w3m-mode-hook
          (defun elmord-w3m-mode-hook ()
            (local-set-key (kbd "C-<prior>") 'w3m-previous-buffer)
            (local-set-key (kbd "C-<next>") 'w3m-next-buffer)
            (local-set-key (kbd "C-c RET") 'w3m-view-this-url-new-session)))

;; Don't use proportional fonts in Elfeed (which uses shr).
(setq shr-use-fonts nil)

(setq w3m-clear-display-while-reading nil)

;; Elfeed stuff
(eval-after-load 'elfeed
  (defun elmord-elfeed-eval-after-load ()
    (setq elfeed-search-filter "-meh -log +unread")
    (define-key elfeed-search-mode-map (kbd "1") (lambda () (interactive) (elfeed-search-set-filter "-meh -log +unread")))
    (define-key elfeed-search-mode-map (kbd "2") (lambda () (interactive) (elfeed-search-set-filter "-meh +log +unread")))
    (define-key elfeed-search-mode-map (kbd "3") (lambda () (interactive) (elfeed-search-set-filter "+meh -log +unread")))
    (define-key elfeed-search-mode-map (kbd "0") (lambda () (interactive) (elfeed-search-set-filter "")))))


;;;; Blog stuff.
;;; Edit Blognir posts in html-mode.
(add-to-list 'auto-mode-alist '("/\\.data/entries/" . html-mode))

(defun elmord-new-blog-post (id)
  (interactive "sPost id: ")
  (let ((filename (concat "/home/www/internal/html/public_html/blog/.data/entries/"
                          (format-time-string "%Y%m%d")
                          "-" id)))
    (with-current-buffer (find-file filename)
      (insert (concat "Title: \n"
                      "Created: " (format-time-string "%Y-%m-%d %H:%M %z") "\n"
                      "Tags: \n\n"))
      (beginning-of-buffer)
      (end-of-line))))
                      

(define-key ctl-z-map (kbd "b") 'elmord-new-blog-post)


(defun elmord-monomono (words)
  (interactive "p")
  (dotimes (i words)
    (let ((word-length (+ 1 (random 5))))
      (dotimes (syllable word-length)
        (insert (if (evenp syllable) "mo" "no"))))
    (insert " ")))
  

;;;; Miscellaneous.

;; Cache GPG passphrases (questionable! should use gpg-agent instead)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(global-set-key (kbd "M-g M-f") 'find-file-at-point)

(defun elmord-git-commit-and-push (message)
  (interactive "sCommit message: ")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (shell-command (concat "git commit -a -m "
                         (shell-quote-argument message)
                         " && git push")))

(global-set-key (kbd "C-z m") 'elmord-git-commit-and-push)

;; Remove special action of ? in minibuffer completion.
(defun elmord-delq-alist (key alist)
  (let ((binding (assq key alist)))
    (when binding
      (delq binding alist))))

(setq minibuffer-local-completion-map
      (elmord-delq-alist ?? minibuffer-local-completion-map))

;; Persistent scratch buffer (is this a good idea?)
;; Vaguely based on http://dorophone.blogspot.com.br/2011/11/how-to-make-emacs-scratch-buffer.html

(defvar elmord-scratch-file "~/.emacs.d/scratch")

(defun elmord-load-scratch-file ()
  (with-current-buffer (get-buffer "*scratch*")
    (when (file-exists-p elmord-scratch-file)
      (delete-region (point-min) (point-max))
      (insert-file elmord-scratch-file))
    ;; Associate buffer with file, but restore original buffer name.
    (set-visited-file-name elmord-scratch-file)
    (set-buffer-modified-p nil)
    (cd "~")
    (rename-buffer "*scratch*")))

(elmord-load-scratch-file) ;; load on init

;; Misc.
(ido-mode 'buffers)

(add-to-list 'Info-directory-list "/opt/guile/share/info/")

;; Outline mode.
(setq outline-minor-mode-prefix (kbd "C-2"))

;; PDF Tools and PDF stuff.

(add-to-list 'dired-guess-shell-alist-user
             '("\\.pdf\\'" "evince"))

(when (package-installed-p 'pdf-tools)
  (require 'pdf-tools)
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000")))


;;;; Input methods.
;; Work around buggy 'a' in Tibetan input method. This effectively
;; turns 'a' into a null key which just breaks consonant stacking.
(eval-after-load 'tibetan
  (defun elmord-tibetan-fixup-hook ()
    (setq tibetan-vowel-transcription-alist
          (remq (assoc "a" tibetan-vowel-transcription-alist)
                tibetan-vowel-transcription-alist))))


;;;; Org mode.
(setq org-pretty-entities t)
(setq  org-entities-user
       '(("top" "\\top" t "⊤" "\\top" "\\top" "⊤")
         ("bot" "\\bot" t "⊥" "\\bot" "\\bot" "⊥")))

(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ))


;;;; Utilities.

(defun elmord-decode-event (event)
  "Example: (elmord-decode-event (kbd \"s-a\"))"
  (if (or (vectorp event) (stringp event))
      (map 'vector 'elmord-decode-event event)
    (let ((modifiers (event-modifiers event))
          (base (event-basic-type event)))
      (cons modifiers (if (characterp base)
                          (string base)
                        base)))))

;;;; Other config files.

(load-file "~/.emacs.d/init-exwm.el")
(load-file "~/.emacs.d/init-experimental.el")

;; Custom file (where Emacs's Customize feature saves settings).
(load (setq custom-file "~/.emacs.d/custom.el"))

;; Host-specific settings.
(let ((host-init-file (format "~/.emacs.d/init.%s.el" (system-name))))
  (when (file-exists-p host-init-file)
    (load-file host-init-file)))


;; Local Variables:
;; outline-regexp: ";;;;\\(;* [^ ]\\)"
;; eval: (outline-minor-mode)
;; eval: (outline-hide-body)
;; End:

