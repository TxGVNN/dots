;;; .emacs --- initialization file
;;; _____  _     __    _      _      _
;;;  | |  \ \_/ / /`_ \ \  / | |\ | | |\ |
;;;  |_|  /_/ \ \_\_/  \_\/  |_| \| |_| \|
;;;
;;; [ @author TxGVNN ]

;;; Code:
(when (version< emacs-version "25.1")
  (error "Requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

(setq gc-cons-threshold most-positive-fixnum) ;; enable gcmh
;; doom-emacs:docs/faq.org#unset-file-name-handler-alist-temporarily
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist doom--file-name-handler-alist)))
(defvar hidden-minor-modes '(whitespace-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("me" . "https://txgvnn.github.io/packages/"))
(when (< emacs-major-version 27)
  (package-initialize))

;;; BOOTSTRAP `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; PACKAGES
;; gcmh
(use-package gcmh
  :ensure t
  :init (gcmh-mode)
  :config (add-to-list 'hidden-minor-modes 'gcmh-mode))

;; ivy
(use-package ivy
  :ensure t :pin me
  :bind ("C-x C-r" . ivy-resume)
  :init
  (setq ivy-magic-tilde nil
        ivy-extra-directories '("./")
        ivy-on-del-error-function #'ignore
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-action)
  (add-to-list 'hidden-minor-modes 'ivy-mode)
  (ivy-mode)
  :config (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial))

;; counsel
(use-package counsel
  :ensure t :pin me
  :init (counsel-mode)
  :bind
  ("M-X" . execute-extended-command)
  ("M-Y" . yank-pop)
  ("C-c m" . counsel-imenu)
  ("M-s d" . counsel-ag)
  ("M-s r" . counsel-rg)
  ("M-s j" . counsel-file-jump)
  (:map counsel-find-file-map ("C-k" . counsel-up-directory))
  :hook
  (org-mode . (lambda() (define-key org-mode-map (kbd "C-c m") 'counsel-org-goto)))
  :config
  (add-to-list 'hidden-minor-modes 'counsel-mode)
  (setq counsel-yank-pop-separator
        (concat "\n" (apply 'concat (make-list 25 "---")) "\n")
        enable-recursive-minibuffers t
        counsel-find-file-at-point t)
  (use-package smex :ensure t))

;; swiper
(use-package swiper
  :ensure t :pin me
  :config
  (defun swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol))) (swiper sym))
  :bind ("M-s w" . swiper-at-point))

;; fuzzy
(use-package fuzzy
  :ensure t
  :config (turn-on-fuzzy-isearch))

;; avy
(use-package avy
  :ensure t
  :config
  (setq avy-all-windows nil
        avy-background t)
  :bind
  ("M-g a" . avy-goto-char)
  ("M-g l" . avy-goto-line))

;; crux
(use-package crux
  :ensure t :pin me
  :bind
  ("C-^" . crux-top-join-line)
  ("C-a" . crux-move-beginning-of-line)
  ("C-o" . crux-smart-open-line-above)
  ("M-o" . crux-smart-open-line)
  ("C-c c" . crux-create-scratch-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c r" . crux-rename-buffer-and-file)
  ("C-c t" . crux-visit-term-buffer)
  ("C-h RET" . crux-find-user-init-file)
  ("C-x / e" . crux-open-with)
  ("C-x 7" . crux-swap-windows))

;; move-text
(use-package move-text
  :ensure t
  :bind
  ("M-g <up>" . move-text-up)
  ("M-g <down>" . move-text-down))

;; switch-window
(use-package ace-window
  :ensure t :defer t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope (quote frame)))

;; xclip -- don't use xsel
(use-package xclip
  :ensure t :defer t
  :init
  (add-hook 'tty-setup-hook
            (lambda()(require 'xclip nil t)(xclip-mode))))

;; checker kbd("C-h .")
(if (version< emacs-version "26.1")
    (use-package flycheck
      :ensure t
      :config
      (defun flycheck-display-error-at-point-soon () nil)
      (setq flycheck-highlighting-mode (quote columns))
      :hook (prog-mode . flycheck-mode))
  (use-package flymake
    :config
    (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer)
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    :hook (prog-mode . flymake-mode)))

;; git-gutter
(use-package git-gutter
  :ensure t
  :init (setq git-gutter:lighter "")
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  (global-git-gutter-mode)
  :bind
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g s" . git-gutter:stage-hunk)
  ("C-x g r" . git-gutter:revert-hunk))
;; magit
(use-package magit
  :ensure t
  :bind
  ("C-x g g" . magit-status)
  ("C-x g f" . magit-find-file)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch)
  (:map magit-file-mode-map ("C-x g") nil))

;; projectile
(use-package projectile
  :ensure t :defer t
  :init
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-mode-line-prefix "")
  (setq projectile-project-compilation-cmd "make ")
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  :bind
  (:map projectile-mode-map ("C-x p" . projectile-command-map)))
;; counsel-projectile
(use-package counsel-projectile
  :ensure t :defer t
  :after (projectile)
  :init
  (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)
  (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
  (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-ag)
  (define-key projectile-mode-map [remap projectile-ripgrep] #'counsel-projectile-rg)
  (define-key projectile-mode-map [remap projectile-compile-project] #'counsel-compile))
;; ibuffer-projectile
(use-package ibuffer-projectile
  :ensure t
  :config
  (setq ibuffer-projectile-prefix "")
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; perspective
(use-package perspective
  :ensure t
  :init
  (setq persp-mode-prefix-key (kbd "C-z")
        persp-initial-frame-name "0")
  (persp-mode)
  :bind
  ("C-x x" . persp-switch-last)
  (:map perspective-map ("z" . perspective-map)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c e a" . mc/mark-all-like-this)
  ("C-c e n" . mc/mark-next-like-this)
  ("C-c e p" . mc/mark-previous-like-this)
  ("C-c e l" . mc/edit-lines)
  ("C-c e r" . mc/mark-all-in-region))

;; smartparens
(use-package smartparens
  :ensure t
  :config (require 'smartparens-config)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda()(turn-off-smartparens-mode)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda()(turn-on-smartparens-mode)))
  (add-to-list 'hidden-minor-modes 'smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . 'sp-forward-sexp)
              ("C-M-b" . 'sp-backward-sexp))
  :hook (prog-mode . smartparens-mode))
;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :init (volatile-highlights-mode)
  :config (add-to-list 'hidden-minor-modes 'volatile-highlights-mode))
;; anzu
(use-package anzu
  :ensure t
  :init
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  (global-anzu-mode))
;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :config
  (add-to-list 'hidden-minor-modes 'symbol-overlay-mode)
  :bind ("M-s H" . symbol-overlay-put)
  :hook (prog-mode . symbol-overlay-mode))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  :bind
  (:map yas-minor-mode-map
        ("C-c y i" . yas-insert-snippet)
        ("C-c y n" . yas-new-snippet)
        ("C-c y v" . yas-visit-snippet-file)
        ("C-c y TAB" . yas-maybe-expand))
  :hook
  ((prog-mode org-mode markdown-mode)
   . yas-minor-mode))
;; My yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t :defer t :pin me)

;; company
(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("M-]" . company-complete-custom)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-lighter-base "@"
        company-require-match 'never
        company-idle-delay 0.1)
  (defun company-complete-custom (&optional prefix)
    "Company and Yasnippet(PREFIX)."
    (interactive "P")
    (if (fboundp 'company--active-p)
        (if (company--active-p) (company-cancel)))
    (if prefix
        (if (not company-mode) (yas-expand)
          (call-interactively 'company-yasnippet))
      (call-interactively 'company-complete))))

;; undo-tree
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-mode-lighter ""
        undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (global-undo-tree-mode))

;; themes
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; OPTIONS
;; google-translate
(use-package google-translate
  :config
  (defun google-translate-query(&optional prefix)
    (interactive "P")
    (setq google-translate-translation-directions-alist '(("en" . "vi")))
    (if prefix
        (setq google-translate-translation-directions-alist '(("vi" . "en"))))
    (call-interactively 'google-translate-smooth-translate))
  :bind ("M-s t" . google-translate-query))
(use-package debpaste
  :defer t
  :init
  (setq debpaste-base-url "https://paste.debian.net/"
        debpaste-paste-is-hidden t))
(use-package git-link
  :defer t
  :init (setq git-link-use-commit t))

;;; HOOKS
(defun add-to-hooks (func &rest hooks)
  "Add FUNC to mutil HOOKS."
  (dolist (hook hooks) (add-hook hook func)))
;; enable whitespace-mode
(add-to-hooks 'whitespace-mode
              'prog-mode-hook 'org-mode-hook
              'markdown-mode-hook)
;; flymake on g-n & g-p bindings
(add-hook 'flymake-mode-hook
          (lambda()
            (setq next-error-function #'flymake-goto-next-error
                  previous-error-function #'flymake-goto-prev-error)))
;; Apply .dir-locals to major-mode after load .dir-local
;; https://stackoverflow.com/questions/19280851/how-to-keep-dir-local-variables-when-switching-major-modes
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

;; large-file
(defun find-file-with-large-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) 7340032) ;; (* 7 1024 1024)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'find-file-with-large-file-hook)

;; hide the minor modes
(defun purge-minor-modes ()
  "Dont show on modeline."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

;;; CUSTOMIZE
;; defun
(defun my-kill-ring-save ()
  "Better than kill-ring-save"
  (interactive)
  (if (not mark-active)
      (kill-ring-save (point) (line-end-position))
    (call-interactively 'kill-ring-save)))
(defun indent-and-delete-trailing-whitespace ()
  "Indent and delete trailing whitespace in buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(defun yank-file-path ()
  "Yank file path of buffer."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory
                    (buffer-file-name))))
    (when filename (kill-new filename)
          (message "Yanked %s (%s)" filename (what-line)))))
(defun split-window-vertically-last-buffer (prefix)
  "Split window vertically.
- PREFIX: default(1) is switch to last buffer"
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1 ) (switch-to-next-buffer)))
(defun split-window-horizontally-last-buffer (prefix)
  "Split window horizontally.
- PREFIX: default(1) is switch to last buffer"
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1 ) (switch-to-next-buffer)))
(defun mark-backword (&optional arg allow-extend)
  "Reverse of mark-word(ARG ALLOW-EXTEND)."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (> (mark) (point)) -1 1)))
         (set-mark (save-excursion
                     (goto-char (mark))
                     (backward-word arg) (point))))
        (t (push-mark
            (save-excursion
              (backward-word (prefix-numeric-value arg))
              (point)) nil t))))
(defun insert-temp-filename()
  "Insert new temp filename."
  (interactive)
  (insert
   (concat (file-name-as-directory temporary-file-directory)
           (make-temp-name "tmp"))))
(defun insert-datetime()
  "Insert YYYmmdd-HHMM."
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M" (current-time) t)))
(defun linux-stat-file()
  "Run stat command in linux in current file."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory
                    (buffer-file-name))))
    (when filename
      (shell-command (format "stat '%s'; file '%s'" filename filename)))))
(defun copy-region-to-scratch (&optional file)
  "Copy region to a new scratch."
  (interactive)
  (let* ((string
          (cond
           ((and (bound-and-true-p rectangle-mark-mode) (use-region-p))
            (mapconcat 'concat (extract-rectangle (region-beginning) (region-end)) "\n"))
           ((use-region-p) (buffer-substring-no-properties (point) (mark)))
           (t (buffer-substring (point-min) (point-max)))))
         (buffer-name (format "%s_%s" (file-name-base (buffer-name)) (format-time-string "%y%m%d_%H%M%S")))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (insert string)
      (if file (write-file file nil))
      (switch-to-buffer (current-buffer)))))
(defun save-region-to-temp ()
  "Save region to a new temp file."
  (interactive)
  (let ((filename
         (make-temp-file
          (format "%s_%s_" (file-name-base (buffer-name))
                  (format-time-string "%y%m%d_%H%M" (time-to-seconds)))
          nil (file-name-extension (buffer-name) t))))
    (copy-region-to-scratch filename)))

(defun eww-search-local-help ()
  "Search with keyword from local-help."
  (interactive)
  (let ((help (help-at-pt-kbd-string)))
    (if help (eww help) (message "Nothing!"))))
(defun share-to-transfer_sh (downloads)
  "Share buffer to transfer.sh.
- DOWNLOADS: The max-downloads"
  (interactive "p")
  (let ((temp-file
         (make-temp-file nil nil (file-name-extension (buffer-name) t)))
        (url "https://transfersh.com")
        (msg "") file-hash)
    (if (region-active-p)
        (write-region (point) (mark) temp-file)
      (write-region (point-min) (point-max) temp-file))
    (when (yes-or-no-p (format "Share to %s (%d)?" url downloads))
      (when (yes-or-no-p "Encrypt?")
        (let ((file-hash (md5 (buffer-string))))
          (shell-command (format "openssl aes-256-cbc -md md5 -k %s -in '%s' -out '%s.enc'"
                                 file-hash temp-file temp-file))
          (dired-delete-file temp-file)
          (setq temp-file (format "%s.enc" temp-file)
                msg (format "| openssl aes-256-cbc -d -md md5 -k %s -in - 2>/dev/null"
                            file-hash))))
      (let ((output (format
                     "curl -L %s 2>/dev/null %s"
                     (shell-command-to-string
                      (format "curl -q -H 'Max-Downloads: %d' --upload-file '%s' %s 2>/dev/null"
                              downloads temp-file url)) msg)))
        (kill-new output) (message output))
      (dired-delete-file temp-file))))

(defun share-to-paste.debian.net ()
  "Share buffer to paste.debian.net"
  (interactive)
  (let ((temp-file
         (make-temp-file nil nil (file-name-extension (buffer-name) t)))
        (msg "") file-hash)
    (if (region-active-p)
        (write-region (point) (mark) temp-file)
      (write-region (point-min) (point-max) temp-file))
    (when (yes-or-no-p "Share to paste.debian.net?")
      (when (yes-or-no-p "Encrypt?")
        (let ((file-hash (md5 (buffer-string))))
          (shell-command (format "openssl aes-256-cbc -md md5 -k %s -in '%s' | base64 > '%s.enc'"
                                 file-hash temp-file temp-file))
          (dired-delete-file temp-file)
          (setq temp-file (format "%s.enc" temp-file)
                msg (format "| base64 -d | openssl aes-256-cbc -d -md md5 -k %s -in - 2>/dev/null"
                            file-hash))))
      (find-file-read-only temp-file)
      (debpaste-paste-buffer (get-file-buffer temp-file))
      (message "curl %s 2>/dev/null %s" (debpaste-get-param-val 'download-url (debpaste-get-posted-info)) msg)
      (dired-delete-file temp-file))))

(defvar share-to-online-func
  'share-to-transfer_sh)
(defun share-to-online ()
  "Share buffer to online."
  (interactive)
  (call-interactively share-to-online-func))

(defvar linum-func
  (if (fboundp 'display-line-numbers-mode)
      'display-line-numbers-mode 'linum-mode))
(defun goto-line-with-feedback ()
  "Show line numbers temporarily when 'goto-line."
  (interactive)
  (unwind-protect
      (progn
        (funcall linum-func)
        (goto-line (read-number "Goto line: ")))
    (funcall linum-func 0)))
(global-set-key [remap goto-line] #'goto-line-with-feedback)

;;; MODELINE
(setq mode-line-position
      '((line-number-mode ("(%l" (column-number-mode ",%c")))
        (-4 ":%p" ) (")")))
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defsubst modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos) (current-column)))
(defun selection-info()
  "Information about the current selection."
  (when mark-active
    (cl-destructuring-bind (beg . end)
        (cons (region-beginning) (region-end))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((bound-and-true-p rectangle-mark-mode)
                        (let ((cols (abs (- (modeline-column end)
                                            (modeline-column beg)))))
                          (format "(%dx%d)" lines cols)))
                       ((> lines 0) (format "(%d,%d)" lines (- end beg)))
                       ((format "(%d,%d)" 0 (- end beg))))))
       'face 'font-lock-warning-face))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                " "
                mode-line-buffer-identification
                " "
                mode-line-position
                (:eval (selection-info))
                (vc-mode vc-mode)
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; isearch
(global-set-key (kbd "M-s s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "M-s %") 'isearch-query-replace-regexp)
;; term
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste))
;; summary-mode
(eval-after-load 'gnus-summary-mode
  (setq gnus-summary-line-format "%U%R%z %d %-23,23f (%4,4L) %{%B%}%s\n"
        gnus-sum-thread-tree-root            ""
        gnus-sum-thread-tree-false-root      "──> "
        gnus-sum-thread-tree-leaf-with-other "├─> "
        gnus-sum-thread-tree-vertical        "│ "
        gnus-sum-thread-tree-single-leaf     "└─> "))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "M-D") 'kill-whole-line)
(global-set-key (kbd "M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-x C-@") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x j") 'mode-line-other-buffer)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "M-s e") 'eww)
(global-set-key (kbd "M-s E") 'eww-search-local-help)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-#") 'mark-backword)
(global-set-key (kbd "C-M-_") 'dabbrev-completion)
(global-set-key (kbd "C-x / .") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x / ;") 'indent-and-delete-trailing-whitespace)
(global-set-key (kbd "C-x / b") 'rename-buffer)
(global-set-key (kbd "C-x / o") 'org-agenda)
(global-set-key (kbd "C-x / p") 'yank-file-path)
(global-set-key (kbd "C-x / r") 'revert-buffer)
(global-set-key (kbd "C-x / a") 'linux-stat-file)
(global-set-key (kbd "C-x / n") 'insert-temp-filename)
(global-set-key (kbd "C-x / d") 'insert-datetime)
(global-set-key (kbd "C-x / D") 'org-time-stamp)
(global-set-key (kbd "C-x / x") 'save-region-to-temp)
(global-set-key (kbd "C-x / c") 'copy-region-to-scratch)
(global-set-key (kbd "C-x / s") 'share-to-online)
(global-set-key (kbd "C-x / t") 'untabify)
(global-set-key (kbd "C-x / T") 'tabify)
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)
(global-set-key (kbd "C-x 4 C-v") 'scroll-other-window)
(global-set-key (kbd "C-x 4 M-v") 'scroll-other-window-down)
(global-set-key (kbd "C-x 4 M-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "C-x 4 M->") 'end-of-buffer-other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "ESC <up>") '(lambda () (interactive "") (previous-line 3)))
(global-set-key (kbd "ESC <down>") '(lambda () (interactive "") (next-line 3)))

(setq select-safe-coding-system-function t)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq create-lockfiles nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line nil)
 '(auto-revert-check-vc-info t)
 '(auto-revert-mode-text " ~")
 '(backup-by-copying t)
 '(browse-url-browser-function (quote eww-browse-url))
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(default-input-method "vietnamese-telex")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(eldoc-minor-mode-string " ED")
 '(electric-indent-mode nil)
 '(enable-local-variables :all)
 '(ffap-machine-p-known (quote reject))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(menu-bar-mode nil)
 '(read-quoted-char-radix 16)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(version-control t)
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab newline empty tab-mark)))
 '(x-select-request-type (quote (COMPOUND_TEXT UTF8_STRING STRING TEXT))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-remote ((t (:foreground "magenta"))))
 '(ivy-virtual ((t (:inherit unspecified :foreground unspecified))))
 '(symbol-overlay-default-face ((t (:inherit bold :underline t))))
 '(vc-state-base ((t (:inherit font-lock-string-face :weight bold)))))

;;; PATCHING
(use-package advice-patch :ensure t)

(advice-add 'base64-encode-region
            :before (lambda (&rest _args)
                      "Pass prefix arg as third arg to `base64-encode-region'."
                      (interactive "r\nP")))

(unless (version< emacs-version "26.1")
  (with-eval-after-load 'flymake
    (ignore-errors
      (setq-local byte-compile-warnings nil)
      (advice-patch 'flymake--highlight-line  '(+ 1 (flymake--diag-beg diagnostic)) '(flymake--diag-end diagnostic))
      (advice-patch 'flymake--mode-line-format '" FlyM" '" Flymake")
      (setq-local byte-compile-warnings t))))

(with-eval-after-load 'perspective
  (with-eval-after-load 'compile
    (ignore-errors
      (advice-patch 'compilation-start
                    '(format "compliation(%s)" (persp-name (persp-curr)))
                    '(if (eq mode t)
                         "compilation"
                       (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))))

  (defun ivy-switch-to-buffer ()
    "Switch to another buffer in the CURRENT PERSP."
    (interactive)
    (if (not (bound-and-true-p persp-mode))
        (ivy-switch-buffer)
      (setq this-command #'ivy-switch-buffer)
      (ivy-read "Switch to buffer: " (remove nil (mapcar 'buffer-name (persp-buffers (persp-curr))))
                :keymap ivy-switch-buffer-map
                :preselect (buffer-name (other-buffer (current-buffer)))
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher
                :caller 'ivy-switch-buffer)))
  (with-eval-after-load 'ivy
    (define-key ivy-mode-map (kbd "C-x b") 'ivy-switch-to-buffer))

  (defun counsel-projectile-M-x()
    "M-x ^project-name"
    (interactive)
    (counsel-M-x (format "^%s " (persp-name (persp-curr)))))
  (define-key projectile-mode-map [remap projectile-project-buffers-other-buffer] #'counsel-projectile-M-x)

  (defun counsel-switch-to-buffer ()
    "Switch to another buffer in the CURRENT PERSP."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (ivy-switch-to-buffer)))
  (global-set-key (kbd "C-x B") 'counsel-switch-to-buffer)

  (defun ivy-switch-buffer-with-persp (&optional _)
    "Clone from persp-switch-to-buffer."
    (interactive)
    (let ((buffer (window-normalize-buffer-to-switch-to (read-buffer-to-switch "Switch to buffer: "))))
      (if (memq buffer (persp-buffers (persp-curr)))
          (switch-to-buffer buffer)
        (let ((other-persp (persp-buffer-in-other-p buffer)))
          (when (eq (car-safe other-persp) (selected-frame))
            (persp-switch (cdr other-persp)))
          (switch-to-buffer buffer)))))
  (with-eval-after-load 'ivy
    (ivy-add-actions
     'ivy-switch-buffer
     '(("p" ivy-switch-buffer-with-persp "persp-switch-to-buffer"))))

  (defun persp-update-modestring ()
    "Override persp-update-modestring."
    (when persp-show-modestring
      (let ((open (list (nth 0 persp-modestring-dividers)))
            (close (list (nth 1 persp-modestring-dividers)))
            (sep (nth 2 persp-modestring-dividers)))
        (set-frame-parameter
         nil 'persp--modestring
         (append open
                 (cons (persp-format-name (persp-name (persp-curr)))())
                 close)))))

  ;; find file with perspective and projectile
  (with-eval-after-load 'counsel
    (defun counsel-find-file-action (x)
      "Find file X."
      (with-ivy-window
        (if (and counsel-find-file-speedup-remote
                 (file-remote-p ivy--directory))
            (let ((find-file-hook nil))
              (find-file (expand-file-name x ivy--directory)))
          (if (and (bound-and-true-p persp-mode) (bound-and-true-p projectile-mode))
              (let (project-name (project-root (projectile-project-root (expand-file-name x))))
                (when project-root
                  (setq project-name (funcall projectile-project-name-function project-root))
                  (persp-switch project-name))))
          (find-file (expand-file-name x ivy--directory)))))))

(with-eval-after-load 'counsel-projectile
  (advice-patch 'counsel-projectile-switch-project-by-name
                '(run-hook-with-args 'projectile-before-switch-project-hook project)
                '(run-hooks 'projectile-before-switch-project-hook))
  (add-hook 'projectile-before-switch-project-hook
            (lambda (project-to-switch)
              (if (and project-to-switch (bound-and-true-p persp-mode))
                  (persp-switch (funcall projectile-project-name-function project-to-switch)))))
  (defun counsel-projectile-find-file-action-file-jump (file)
    "Call `counsel-find-file' from FILE's directory."
    (let* ((f (projectile-expand-root file))
           (default-directory (file-name-directory f)))
      (counsel-file-jump)))
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("f" counsel-projectile-find-file-action-file-jump "file jump")))

  (defun counsel-projectile-M-x-action(file)
    "Call `counsel-projectile-M-x'."
    (let* ((project-root (projectile-project-root (expand-file-name file)))
           (project-name (funcall projectile-project-name-function project-root)))
      (persp-switch project-name)
      (counsel-projectile-M-x)))
  (ivy-add-actions
   'counsel-projectile-switch-project
   '(("ESC" counsel-projectile-M-x-action "M-x"))))

(with-eval-after-load 'projectile
  (defun projectile-run-term (program)
    "Override project-run-term."
    (interactive (list nil))
    (let* ((project (projectile-ensure-project (projectile-project-root)))
           (termname (concat "term " (projectile-project-name project)))
           (buffer (concat "*" termname "*")))
      (unless (get-buffer buffer)
        (require 'term)
        (projectile-with-default-dir project
          (ansi-term (or explicit-shell-file-name
                         (getenv "SHELL") "/bin/sh") termname)))
      (switch-to-buffer buffer))))

;;; LANGUAGES
;; .emacs
(defun develop-dot()
  "Update 'user-init-file - .emacs."
  (interactive)
  (let ((upstream (make-temp-file ".emacs")))
    (url-copy-file "https://raw.githubusercontent.com/TxGVNN/dots/master/.emacs" upstream t)
    (diff user-init-file upstream)
    (other-window 1 nil)
    (message "Override %s by %s to update" user-init-file upstream)))

(defun develop-erc ()
  "ERC configuration"
  (interactive)
  (package-install 'ercn))
(with-eval-after-load 'ercn
  (setq ercn-notify-rules
        '((current-nick . all)
          (keyword . all)
          (pal . ("#emacs"))
          (query-buffer . all)))
  (defun do-notify (nick msg)
    (call-process "notify-send" nil nil nil "ERC" nick))
  (add-hook 'ercn-notify-hook 'do-notify))

;; c-mode
(defun my-c-mode-common-hook ()
  "C-mode hook."
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t
        c-basic-offset 4
        c-indent-level 4))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; org-mode
(setq org-babel-load-languages (quote ((emacs-lisp . t) (shell . t)))
      org-enforce-todo-dependencies t
      org-todo-keyword-faces (quote (("BLOCKED" . error) ("WIP" . warning)))
      org-todo-keywords
      (quote
       ((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WIP(w)" "BLOCKED(b)" "|" "REJECTED(r)"))))

;; go-mode
(defun develop-go()
  "Go develoment.
Please install:
   GO111MODULE=on go get golang.org/x/tools/gopls@latest
   go get -u github.com/sourcegraph/go-langserver"
  (interactive)
  (package-install 'go-mode)
  (package-install 'lsp-mode))
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook
            (lambda ()
              (lsp-deferred)
              ;; gofmt before every save
              (add-hook 'before-save-hook 'gofmt-before-save)))
  (setq gofmt-command "goimports")
  (defun go-print-debug-at-point()
    "Print debug."
    (interactive)
    (let ((var (substring-no-properties (thing-at-point 'symbol))))
      (move-end-of-line nil)
      (newline-and-indent)
      (insert (format "fmt.Printf(\"D: %s@%s %s, %%+v\\n\", %s)"
                      (file-name-nondirectory (buffer-file-name))
                      (substring (md5 (format "%s%s" (emacs-pid) (current-time))) 0 4) var var)))))

;; python-mode
(defun develop-python()
  "Python development.
Please install:
   pip install python-language-server"
  (interactive)
  (package-install 'lsp-mode)
  (package-install 'company-lsp))
(with-eval-after-load 'python ;; built-in
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (defun python-docs (w)
    "Launch PyDOC on the Word at Point"
    (interactive
     (list (let* ((word (thing-at-point 'word))
                  (input (read-string
                          (format "pydoc entry%s: "
                                  (if (not word) "" (format " (default %s)" word))))))
             (if (string= input "")
                 (if (not word) (error "No pydoc args given") word) input))))
    (shell-command (concat "python -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
    (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))
  (defun python-print-debug-at-point()
    "Print debug."
    (interactive)
    (let ((var (substring-no-properties (thing-at-point 'symbol))))
      (move-end-of-line nil)
      (newline-and-indent)
      (insert (format "print(\"D: %s@%s %s, {}\".format(%s))"
                      (file-name-nondirectory (buffer-file-name))
                      (substring (md5 (format "%s%s" (emacs-pid) (current-time))) 0 4) var var)))))
(add-hook 'python-mode-hook #'lsp-deferred)

;; php-mode
(defun develop-php()
  "PHP development."
  (interactive)
  (package-install 'php-mode)
  (package-install 'company-php))
(use-package php-mode
  :defer t
  :hook
  (php-mode . (lambda ()
                (add-to-list 'company-backends 'company-ac-php-backend))))

;; terraform-mode
(defun develop-terraform()
  "Terraform development."
  (interactive)
  (package-install 'company-terraform)
  (package-install 'terraform-doc))
(use-package company-terraform
  :defer t
  :hook
  (terraform-mode . (lambda ()
                      (add-to-list 'company-backends 'company-terraform))))

;; ansible-mode
(defun develop-ansible ()
  "Ansible development."
  (interactive)
  (package-install 'ansible)
  (package-install 'ansible-doc)
  (package-install 'company-ansible))
(use-package ansible
  :defer t
  :init
  (add-hook 'ansible-hook
            (lambda ()
              (ansible-doc-mode)
              (add-to-list 'company-backends 'company-ansible)
              (yas-minor-mode-on))))
(use-package ansible-doc
  :defer t
  :config (define-key ansible-doc-mode-map (kbd "M-?") #'ansible-doc))

;; java-mode
(defun develop-java()
  "Java development.
Please install:
https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
tar -vxf jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/"
  (interactive)
  (package-install 'lsp-java)
  (package-install 'company-lsp))
(use-package lsp-java
  :defer t
  :init (add-hook 'java-mode-hook #'lsp-deferred))
;; html-mode
(defun develop-html()
  "HTML development."
  (interactive)
  (package-install 'indent-guide))
(use-package indent-guide
  :defer t
  :hook (html-mode . indent-guide-mode)
  :config (set-face-foreground 'indent-guide-face "dimgray"))

;; web-mode
(defun develop-web()
  "WEB development.
npm i -g javascript-typescript-langserver"
  (interactive)
  (package-install 'web-mode)
  (package-install 'eslint-fix))
(use-package web-mode
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

;; gitlab-mode
(defun develop-gitlab-ci()
  "Gitlab-CI development."
  (interactive)
  (package-install 'gitlab-ci-mode)
  (package-install 'gitlab-ci-mode-flycheck))
(defun gitlab-ci-mode-my-hook ()
  (gitlab-ci-mode-flycheck-enable)
  (if (fboundp 'flycheck-mode) (flycheck-mode)))
(add-hook 'gitlab-ci-mode-hook 'gitlab-ci-mode-my-hook)

;; other modes
(add-to-hooks 'whitespace-mode
              'yaml-mode-hook 'dockerfile-mode-hook)

;; keep personal settings not in the .emacs file
(let ((personal-settings (expand-file-name "personal.el" user-emacs-directory)))
  (when (file-exists-p personal-settings)
    (load-file personal-settings)))

(provide '.emacs)
;;; .emacs ends here
