(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dired-single smooth-scrolling geiser company-anaconda json-mode js2-mode rainbow-mode elisp-slime-nav company helm-ag helm-descbinds smex ido-completing-read+ flx-ido helm-projectile helm rainbow-delimiters anaconda-mode dired+ direx elscreen zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit projectile ov imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major dash crux browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; List of required packages
(defvar required-packages
  '(
    ;;    solarized-theme
    ;;    sr-speedbar
    elscreen
    expand-region
    ;;    hideshowvis
    ;;    gh-md  ; Markdown viewer from github realtime
    ;;    markdown-mode
    ;;    diff-hl
    ;;    magit
    smooth-scrolling
    ;;    cmake-mode
    ;;    flycheck
    ;;    projectile
    ;;    helm-projectile
    direx
    dired+
    dired-single
    ;;    ggtags
    ;;    yasnippet
    ;;    solarized-theme
    ;;    chess
    ;;    auto-complete
    ;;    jedi
    ;;    pyde
    ;;    pyimpsort
    ;;    pymacs
    ;;    pysmell
    ;;    python
    ;;    python-cell
    ;;    ac-python
    ;;    anaconda-mode
    ;;    ein
    ;;    elpy
    ;;    ipython
    ;;    transpose-frame
    ;;    python-mode
    expand-region
    ;;    projectile
    ;;    move-text
    ;;    neotree
    ) "a list of packages to ensure are installed at launch.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; Check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Propmt y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Open Emacs in Full screen mode always
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customizing the cursor
(setq-default cursor-type 'bar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set line numbers
(global-linum-mode 1)
(setq linum-format "   %d ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function and shortcut (C-d) to duplicate the current line
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; Save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; Local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; Don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; Store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; Insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; End-of-let

  ;; Put the point in the lowest line and return
  (next-line arg))
(global-set-key (kbd "C-d") 'duplicate-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set Title bar to show the name of the full path of the file which is open
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind Alt Shift Up/Down to move regions of text up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start Elscreen by default(gives functionality similar to tabs)
(elscreen-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expand Regions -- Ctrl W feature of intelliJ
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)    ;; Expand   with ctrl =
(global-set-key (kbd "C-+") 'er/contract-region)  ;; Contract with ctrl shift =
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Toggle Comment on Line/Selected Region with Alt + ;
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
;; Overriding default behaviour to insert comment at end of line.
;; Rather toggle comment on whole line/ selected region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Join Lines with M-j
(global-set-key (kbd "M-j")
		(lambda () (interactive) (join-line -1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dired settings

(require 'dired )

;; Dired show file name in human readable format
(setq dired-listing-switches "-alhk")

;; Setting up shortcut key for direx
(global-set-key (kbd "C-x C-d") 'direx-project:jump-to-project-root)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
;; Dired reuse same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Dired reuse same bufer while going to parent directory
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))

;; Switch to dired buffer
(defun switch-to-dired-buffer ()
  "..."
  (interactive)
  (let ((dbufs  (cl-remove-if-not
                 (lambda (bf)
                   (with-current-buffer bf
                     (derived-mode-p 'dired-mode)))
                 (buffer-list))))
    (switch-to-buffer (car dbufs))))
(global-set-key (kbd "C-x M-d") 'switch-to-dired-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm Config

(require 'prelude-helm-everywhere)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-S-s") 'helm-occur)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set selection background to white color
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prevent Creation of Backup files
(setq make-backup-files nil)

;; Disable auto-save
(setq auto-save-default nil)

;; Prevent Lockfiles
(setq create-lockfiles nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tab indentation
(setq tab-width 2 indent-tabs-mode nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on highlighting matching brackets when cursor is on one of them
(show-paren-mode t)

;; Auto Close brackets
(electric-pair-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set smooth scrolling
;; Mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
;; Keyboard
(require 'smooth-scrolling)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
