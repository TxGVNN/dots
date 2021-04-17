;;; .emacs --- initialization file.
;;; Commentary:
;;; _____  _     __    _      _      _
;;;  | |  \ \_/ / /`_ \ \  / | |\ | | |\ |
;;;  |_|  /_/ \ \_\_/  \_\/  |_| \| |_| \|
;;;
;;; [ @author TxGVNN ]

;;; Code:
(when (version< emacs-version "25.1")
  (error "Requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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
(add-to-list 'package-archives '("me" . "https://txgvnn.github.io/giaelpa/"))
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

;; selectrum
(use-package selectrum
  :ensure t
  :init (selectrum-mode)
  :bind
  ("C-x C-r" . selectrum-repeat)
  (:map selectrum-minibuffer-map
        ("<prior>" . selectrum-previous-page)
        ("<next>" . selectrum-next-page)))

;; orderless - filtering
(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :defer t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g e" . consult-error)
  ("M-s w" . consult-line-at-point)
  ("M-s g" . consult-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s f" . consult-find)
  ("M-s m" . consult-multi-occur)
  ("M-X" . consult-mode-command)
  ("C-x B" . consult-buffer)
  (:map minibuffer-local-map ("M-r" . consult-history))
  :config
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq consult-preview-key (kbd "C-l"))
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history)
  (setq consult-ripgrep-command
        "rg -i --null --line-buffered --color=always --max-columns=500\
   --no-heading --line-number --hidden . -e ARG OPTS")
  (defun consult-thing-at-point ()
    "Return a string that corresponds to the current thing at point."
    (substring-no-properties
     (cond
      ((use-region-p)
       (let* ((beg (region-beginning))
              (end (region-end))
              (eol (save-excursion (goto-char beg) (line-end-position))))
         (buffer-substring-no-properties beg (min end eol))))
      ((thing-at-point 'url))
      ((let ((s (thing-at-point 'symbol)))
         (and (stringp s)
              (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
                  (match-string 1 s)
                s))))
      ((looking-at "(+\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
       (match-string-no-properties 1))
      (t ""))))
  (defun consult-line-at-point()
    (interactive)
    (let ((thing (consult-thing-at-point))
          (consult-preview-key 'any))
      (when (use-region-p)
        (deactivate-mark))
      (consult-line (regexp-quote thing))))
  (require 'consult))

(use-package embark
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-o" . embark-act))
  (:map embark-file-map
        ("x" . consult-file-externally))
  :config
  (defun embark-in-directory (dir &optional prefix)
    "Run CMD in directory DIR."
    (interactive "DIn directory:\nP")
    (let ((default-directory (file-name-directory dir))
          (embark-quit-after-action t)
          (action (embark--with-indicator
                   (propertize (format "Act:%s" dir) 'face 'highlight)
                   embark-prompter
                   (embark--action-keymap 'file))))
      (command-execute action)))
  (define-key embark-general-map (kbd "/") #'embark-in-directory)
  ;; term
  (defun embark-run-term(dir)
    "Create or visit a terminal buffer."
    (interactive "D")
    (let ((default-directory (file-name-directory dir)))
      (crux-visit-term-buffer t)))
  (define-key embark-file-map (kbd "t") #'embark-run-term)
  (defun embark-make-directory(dir)
    (interactive "D")
    (make-directory dir)
    (find-file dir))
  (define-key embark-file-map (kbd "+") #'embark-make-directory))

(use-package which-key
  :ensure t
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; avy
(use-package avy
  :ensure t :defer t
  :config
  (setq avy-all-windows nil
        avy-background t)
  :bind
  ("M-g a" . avy-goto-char)
  ("M-g l" . avy-goto-line))

;; crux - modified
(use-package crux
  :ensure t :defer t :pin me
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
  :ensure t :defer t
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

;; error-checker kbd("C-h .")
(use-package flycheck
  :ensure t :defer t
  :config
  (defun flycheck-maybe-display-error-at-point-soon() nil)
  (setq flycheck-highlighting-mode (quote columns))
  ;; g-n & g-p bindings
  (define-key flycheck-mode-map (kbd "M-g n") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-g p") #'flycheck-previous-error)
  :hook (prog-mode . flycheck-mode))

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
  :ensure t :defer t
  :init (setq magit-define-global-key-bindings nil)
  :bind
  ("C-x g f" . magit-find-file)
  ("C-x g g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))
;; git-link
(use-package git-link
  :ensure t :defer t
  :config (setq git-link-use-commit t))

;; ripgrep
(use-package rg :ensure t :defer t)

;; projectile
(use-package projectile
  :ensure t :defer t
  :init
  (setq projectile-dynamic-mode-line nil
        projectile-mode-line-prefix ""
        projectile-project-compilation-cmd "make ")
  (defvar savehist-additional-variables)
  (add-hook 'savehist-mode-hook
            (lambda nil
              (add-to-list 'savehist-additional-variables 'projectile-project-command-history)))
  (run-with-idle-timer 0.1 nil (lambda()(projectile-mode)))
  :bind (:map projectile-mode-map ("C-x p" . projectile-command-map))
  :config
  (defun projectile-run-compilation (cmd)
    "Override projectile-run-compilation. Run external or Elisp CMD."
    (if (functionp cmd)
        (funcall cmd)
      (let ((project (projectile-project-name
                      (projectile-ensure-project (projectile-project-root)))))
        (cl-letf ((compilation-buffer-name-function
                   #'(lambda (&rest _)(format "*compilation(%s)*" (persp-name (persp-curr))))))
          (compile cmd)))))
  (defun projectile-run-term (arg)
    "Override projectile-run-term."
    (interactive (list nil))
    (let* ((project (projectile-ensure-project (projectile-project-root)))
           (termname (concat "term " (projectile-project-name project)))
           (buffer (concat "*" termname "*")))
      (unless (get-buffer buffer)
        (require 'term)
        (projectile-with-default-dir project
          (ansi-term (or explicit-shell-file-name (getenv "SHELL") "/bin/sh") termname)))
      (switch-to-buffer buffer)))
  ;; integrate with consult
  (defun consult-projectile-grep (&optional initial)
    "Using consult-grep(INITIAL) in projectile."
    (interactive)
    (consult-grep (projectile-project-root) initial))
  (define-key projectile-command-map (kbd "s g") #'consult-projectile-grep)
  (define-key projectile-command-map (kbd "s G") #'projectile-grep)

  (defun consult-projectile-ripgrep (&optional initial)
    "Using consult-ripgrep(INITIAL) in projectile."
    (interactive)
    (consult-ripgrep (projectile-project-root) initial))
  (define-key projectile-command-map (kbd "s r") #'consult-projectile-ripgrep)
  (define-key projectile-command-map (kbd "s R") #'projectile-ripgrep)
  ;; integrate with embark
  (with-eval-after-load 'embark
    (defun embark-become-project(&optional full)
      (interactive "P")
      (embark--quit-and-run
       (lambda ()
         (let ((use-dialog-box nil)
               (this-command 'projectile-switch-project))
           (command-execute this-command)))))
    (define-key embark-general-map (kbd "p") #'embark-become-project)
    (defun embark-switch-project-with-persp (dir)
      (if-let ((fboundp 'persp-switch)
               (default-directory (projectile-project-root (expand-file-name dir))))
          (persp-switch default-directory)))
    (add-to-list 'marginalia-prompt-categories '("project" . project))
    (add-to-list 'embark-keymap-alist '(project . embark-project-map))
    (defun embark-project-rg(dir &optional initial)
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (consult-ripgrep dir initial))
    (defun embark-project-grep(dir &optional initial)
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (consult-grep dir initial))
    (defun embark-project-term(dir &optional initial)
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (let ((default-directory (file-name-directory dir)))
        (call-interactively 'projectile-run-term)))
    (defun magit-project (dir)
      "Run CMD in directory DIR."
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (magit-status dir nil))
    (defun embark-project-switch-buffer (dir)
      "Run CMD in directory DIR."
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (let ((default-directory (file-name-directory dir)))
        (call-interactively 'persp-switch-to-buffer*)))
    (defun embark-project-compile (dir)
      "Run CMD in directory DIR."
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (let ((default-directory (file-name-directory dir)))
        (call-interactively 'projectile-compile-project)))
    (defun embark-project-find-file (dir)
      "Run CMD in directory DIR."
      (interactive "D")
      (embark-switch-project-with-persp dir)
      (let ((default-directory (file-name-directory dir)))
        (call-interactively 'projectile-find-file)))
    (embark-define-keymap embark-project-map
      "Keymap for Embark project actions."
      ("sr" embark-project-rg)
      ("sg" embark-project-grep)
      ("xt" embark-project-term)
      ("b" embark-project-switch-buffer)
      ("c" embark-project-compile)
      ("f" embark-project-find-file)
      ("v" magit-project))))

;; ibuffer-projectile
(use-package ibuffer-projectile
  :ensure t :defer 2
  :config
  (setq ibuffer-projectile-prefix "")
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; org-projectile
(use-package org-projectile
  :ensure t :defer t
  :after (projectile)
  :bind
  ("C-x p O c" . org-projectile-capture-for-current-project)
  ("C-x p O f" . org-projectile-open-file-current-project)
  :config
  (defun org-projectile-open-file-current-project()
    (interactive)
    (let* ((project-root (projectile-acquire-root))
           (file (org-projectile-current-file project-root)))
      (find-file (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook)))
  (defun org-projectile-current-file (&optional project)
    "org file in each project."
    (concat (projectile-project-name project) ".org"))
  (setq org-projectile-per-project-filepath #'org-projectile-current-file)
  (org-projectile-per-project))

;; perspective
(use-package perspective
  :ensure t
  :init
  (setq persp-mode-prefix-key (kbd "C-z")
        persp-initial-frame-name "0")
  (persp-mode)
  :bind
  ("C-x x" . persp-switch-last)
  ("<f5>" . persp-switch-last)
  (:map perspective-map ("z" . perspective-map))
  :config
  (add-hook 'persp-switch-hook #'hack-dir-local-variables-non-file-buffer)
  ;; Show project folder of persp-curr
  (defun persp-update-modestring ()
    "Override persp-update-modestring."
    (when (and persp-show-modestring (persp-name (persp-curr)))
      (let ((open (list (nth 0 persp-modestring-dividers)))
            (close (list (nth 1 persp-modestring-dividers))))
        (set-frame-parameter
         nil 'persp--modestring
         (append open
                 (cons (propertize
                        (file-name-base (directory-file-name (persp-name (persp-curr))))
                        'face 'persp-selected-face)()) close)))))
  ;; xref ring
  (defvar persp-xref--marker-ring (make-hash-table :test 'equal))
  (defun persp-set-xref--marker-ring ()
    "Sets xref--marker-ring per persp"
    (let ((persp-curr-name (persp-name (persp-curr))))
      (unless (gethash persp-curr-name persp-xref--marker-ring)
        (puthash persp-curr-name (make-ring xref-marker-ring-length)
                 persp-xref--marker-ring))
      (setq xref--marker-ring (gethash persp-curr-name persp-xref--marker-ring))))
  (add-hook 'persp-switch-hook #'persp-set-xref--marker-ring)
  ;; persp-ibuffer
  (with-eval-after-load 'ibuffer
    (defun ibuffer-visit-buffer (&optional single)
      "Override 'ibuffer-visit-buffer with support perspective."
      (interactive "P")
      (let ((buf (ibuffer-current-buffer t)))
        (persp-switch-to-buffer buf)
        (when single (delete-other-windows)))))
  ;; find-file
  (defun find-file (filename &optional wildcards)
    "Override 'find-file(FILENAME WILDCARDS)."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (if (and (bound-and-true-p persp-mode) (bound-and-true-p projectile-mode))
        (if-let ((default-directory (projectile-project-root (expand-file-name filename))))
            (persp-switch default-directory)))
    (let ((value (find-file-noselect filename nil nil wildcards)))
      (if (listp value)
          (mapcar 'pop-to-buffer-same-window (nreverse value))
        (pop-to-buffer-same-window value))))
  ;; projectile
  (add-hook 'projectile-before-switch-project-hook
            (lambda (&optional project-to-switch)
              (if (and project-to-switch (bound-and-true-p persp-mode))
                  (persp-switch (funcall projectile-project-name-function project-to-switch))))))


;; multiple-cursors
(use-package multiple-cursors
  :ensure t :defer t
  :bind
  ("C-c e a" . mc/mark-all-like-this)
  ("C-c e n" . mc/mark-next-like-this)
  ("C-c e p" . mc/mark-previous-like-this)
  ("C-c e l" . mc/edit-lines)
  ("C-c e r" . mc/mark-all-in-region))

;; smartparens
(use-package smartparens
  :ensure t :defer t
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
  :ensure t :defer t
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
  (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  (global-anzu-mode))
;; symbol-overlay
(use-package symbol-overlay
  :ensure t :defer t
  :bind ("M-s H" . symbol-overlay-put)
  :hook (prog-mode . symbol-overlay-mode)
  :config (add-to-list 'hidden-minor-modes 'symbol-overlay-mode))
;; hl-todo
(use-package hl-todo
  :ensure t :defer t
  :hook (prog-mode . hl-todo-mode))
;; themes
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-gruvbox t)
  :config (doom-themes-org-config))

;; yasnippet
(use-package yasnippet
  :ensure t :defer t
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  :hook ((prog-mode org-mode markdown-mode) . yas-minor-mode))
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

;;; BUILTIN
(use-package savehist
  :ensure t
  :config (savehist-mode))
(use-package autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil)
  (defun doom-visible-buffers (&optional buffer-list)
    "Return a list of visible buffers (i.e. not buried)."
    (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
      (if buffer-list (cl-delete-if (lambda (b) (memq b buffer-list)) buffers)
        (delete-dups buffers))))
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t)) (auto-revert-handler))))
  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf (doom-auto-revert-buffer-h)))))
(use-package isearch :defer t
  :init
  (global-set-key (kbd "M-s s") 'isearch-forward-regexp)
  (define-key isearch-mode-map (kbd "M-s %") 'isearch-query-replace-regexp))
(use-package dired :defer t
  :config (setq dired-listing-switches "-alh"))
(use-package compile :defer t
  :init (global-set-key (kbd "C-x m") 'compile)
  :config
  (defun doom-apply-ansi-color-to-compilation-buffer-h ()
    "Applies ansi codes to the compilation buffers."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output t)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))

(use-package gnus :defer t
  :config
  (setq gnus-summary-line-format "%U%R%z %d %-23,23f (%4,4L) %{%B%}%s\n"
        gnus-sum-thread-tree-root            ""
        gnus-sum-thread-tree-false-root      "──> "
        gnus-sum-thread-tree-leaf-with-other "├─> "
        gnus-sum-thread-tree-vertical        "│ "
        gnus-sum-thread-tree-single-leaf     "└─> "))

;; term/shell
(defun interactive-cd (dir)
  "Prompt for a DIR and cd to it."
  (interactive "Dcd ")
  (let ((inhibit-read-only t))
    (insert (concat "cd " dir)))
  (pcase major-mode
    ('shell-mode (comint-send-input))
    ('eshell-mode (eshell-send-input))
    ('term-mode (term-send-input))))

(use-package shell
  :bind (:map shell-mode-map ("C-c d" . interactive-cd)))
(use-package term
  :hook
  (term-mode . (lambda()
                 (let (term-escape-char) (term-set-escape-char ?\C-x))))
  :bind
  (:map term-mode-map
        ("C-c d" . interactive-cd))
  (:map term-raw-map
        ("M-x"  . execute-extended-command)
        ("C-c C-y" . term-paste)
        ("C-c d" . interactive-cd)))
;; project-temp-root
(defvar project-temp-root "~/projects/")
(defun project-temp-M-x (&optional prefix)
  "With PREFIX we will set project-temp-root."
  (interactive "P")
  (if prefix (setq project-temp-root (read-directory-name "Select dir: ")))
  (unless (fboundp 'embark-in-directory) (require 'embark))
  (embark-in-directory project-temp-root))
(global-set-key (kbd "C-x P") #'project-temp-M-x)

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
              '("%e" mode-line-front-space mode-line-mule-info
                mode-line-client mode-line-modified mode-line-remote
                mode-line-frame-identification " "
                mode-line-buffer-identification " "
                mode-line-position (:eval (selection-info))
                (vc-mode vc-mode) " "
                mode-line-modes mode-line-misc-info
                mode-line-end-spaces))

;;; CUSTOMIZE
(defun add-to-hooks (func &rest hooks)
  "Add FUNC to mutil HOOKS."
  (dolist (hook hooks) (add-hook hook func)))
;; enable whitespace-mode
(add-to-hooks 'whitespace-mode
              'prog-mode-hook 'org-mode-hook
              'markdown-mode-hook 'yaml-mode-hook
              'dockerfile-mode-hook)

;; large-file
(defun find-file-with-large-file ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) 7340032) ;; (* 7 1024 1024)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook #'find-file-with-large-file)

;; hide the minor modes
(defun purge-minor-modes ()
  "Dont show on modeline."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))
(add-hook 'after-change-major-mode-hook #'purge-minor-modes)

(defun my-kill-ring-save ()
  "Better than 'kill-ring-save."
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
  (split-window-vertically) (other-window 1 nil)
  (if (= prefix 1 ) (switch-to-next-buffer)))
(defun split-window-horizontally-last-buffer (prefix)
  "Split window horizontally.
- PREFIX: default(1) is switch to last buffer"
  (interactive "p")
  (split-window-horizontally) (other-window 1 nil)
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
              (backward-word (prefix-numeric-value arg)) (point)) nil t))))
(defun insert-temp-filename()
  "Insert new temp filename."
  (interactive)
  (insert
   (concat (file-name-as-directory temporary-file-directory)
           (make-temp-name
            (format "%s_%s_" user-login-name
                    (format-time-string "%y%m%d-%H%M" (time-to-seconds)))))))
(defun insert-datetime()
  "Insert YYYYmmdd-HHMM."
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
  "Copy region to a new scratch or FILE."
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
          (concat (file-name-base (buffer-name)) "_"
                  (unless (string-prefix-p "*scratch-" (buffer-name))
                    (format-time-string "%y%m%d-%H%M%S_" (time-to-seconds))))
          nil (file-name-extension (buffer-name) t))))
    (copy-region-to-scratch filename)))

(defun eww-search-local-help ()
  "Search with keyword from local-help."
  (interactive)
  (let ((help (help-at-pt-kbd-string)))
    (if help (eww (read-string "Search: " help)) (message "Nothing!"))))

(defvar share-to-online-func
  'crux-share-to-transfersh)
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
      (progn (funcall linum-func)
             (goto-line (read-number "Goto line: ")))
    (funcall linum-func 0)))
(global-set-key [remap goto-line] #'goto-line-with-feedback)
(defun package-installs (&rest packages)
  "Install PACKAGES."
  (dolist (package packages) (package-install package)))

(global-set-key (kbd "M-D") 'kill-whole-line)
(global-set-key (kbd "M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-x C-@") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x j") 'mode-line-other-buffer)
(global-set-key (kbd "M-s e") 'eww)
(global-set-key (kbd "M-s E") 'eww-search-local-help)
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
(global-set-key (kbd "C-x / l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x / f") 'flush-lines)
(global-set-key (kbd "C-x / k") 'keep-lines)
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)
(global-set-key (kbd "C-x 4 C-v") 'scroll-other-window)
(global-set-key (kbd "C-x 4 M-v") 'scroll-other-window-down)
(global-set-key (kbd "C-x 4 M-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "C-x 4 M->") 'end-of-buffer-other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "ESC <up>") '(lambda () (interactive) (previous-line 3)))
(global-set-key (kbd "ESC <down>") '(lambda () (interactive) (next-line 3)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (previous-line 3)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (next-line 3)))

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
 '(auto-revert-mode-text " ~")
 '(backup-by-copying t)
 '(browse-url-browser-function 'eww-browse-url)
 '(column-number-mode t)
 '(default-input-method "vietnamese-telex")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(eldoc-minor-mode-string " ED")
 '(electric-indent-mode nil)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(ffap-machine-p-known 'reject)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-default-init nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(kill-do-not-save-duplicates t)
 '(menu-bar-mode nil)
 '(proced-tree-flag t)
 '(read-quoted-char-radix 16)
 '(ring-bell-function #'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(version-control t)
 '(whitespace-style
   '(face tabs trailing space-before-tab newline empty tab-mark))
 '(x-select-request-type '(COMPOUND_TEXT UTF8_STRING STRING TEXT)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(symbol-overlay-default-face ((t (:inherit bold :underline t))))
 '(vc-state-base ((t (:inherit font-lock-string-face :weight bold)))))

;;; PATCHING
(advice-add #'yes-or-no-p :override #'y-or-n-p)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
(advice-add 'base64-encode-region
            :before (lambda (&rest _args)
                      "Pass prefix arg as third arg to `base64-encode-region'."
                      (interactive "r\nP")))


;;; LANGUAGES
;; .emacs
(defun develop-dot()
  "Diff 'user-init-file - .emacs."
  (interactive)
  (let ((upstream (make-temp-file ".emacs")))
    (url-copy-file "https://raw.githubusercontent.com/TxGVNN/dots/master/.emacs" upstream t)
    (diff user-init-file upstream)
    (other-window 1 nil)
    (message "Override %s by %s to update" user-init-file upstream)))

(defun develop-erc ()
  "ERC configuration."
  (interactive)
  (package-install 'ercn))
(with-eval-after-load 'ercn
  (setq ercn-notify-rules
        '((current-nick . all)
          (keyword . all)
          (pal . ("#emacs" "#guix"))
          (query-buffer . all)))
  (defun do-notify (nick msg)
    (call-process "notify-send" nil nil nil "ERC" nick))
  (add-hook 'ercn-notify-hook #'do-notify))

;; c-mode
(defun my-c-mode-common-hook ()
  "C-mode hook."
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t
        c-basic-offset 4
        c-indent-level 4))
(add-hook 'c-mode-common-hook #'my-c-mode-common-hook)

;; org-mode
(setq org-babel-load-languages (quote ((emacs-lisp . t) (shell . t)))
      org-enforce-todo-dependencies t
      org-todo-keyword-faces (quote (("BLOCKED" . error) ("WIP" . warning)))
      org-todo-keywords
      (quote
       ((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "WIP(w)" "BLOCKED(b)" "|" "REJECTED(r)"))))

;; lsp-mode
(use-package lsp-mode
  :defer t
  :init (setq lsp-keymap-prefix "C-x l")
  :config (setq lsp-headerline-breadcrumb-enable nil))

;; go-mode
(defun develop-go()
  "Go develoment.
Please install:
   GO111MODULE=on go get golang.org/x/tools/gopls@latest"
  (interactive)
  (package-installs 'go-mode 'lsp-mode))
(use-package go-mode
  :defer t
  :config
  (defun lsp-go-install-save-hooks ()
    (if (fboundp 'lsp-deferred)(lsp-deferred))
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
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
  (package-installs 'pyvenv 'lsp-mode))
(defun python-pyvenv-activate (&rest args)
  "Python-pyvenv-activate(ARGS)."
  (if (and (equal major-mode 'python-mode)
           (fboundp 'pyvenv-activate))
      (dolist (env '(".venv" ".env" "venv" "env"))
        (if-let ((dir (locate-dominating-file (or (projectile-project-root) default-directory) env)))
            (if (file-directory-p (concat dir env))
                (pyvenv-activate (concat dir env)))))))
(defun python-install-hooks ()
  "Hooks for python."
  (python-pyvenv-activate)
  (lsp-deferred)
  (advice-add 'switch-to-buffer :after 'python-pyvenv-activate '((name . "python-pyvenv")))
  (add-hook 'find-file-hook #'python-pyvenv-activate t t))
(add-hook 'python-mode-hook #'python-install-hooks)
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
      (insert (format "print(\"D: %s@%s %s: {} {}\".format(type(%s), %s))"
                      (file-name-nondirectory (buffer-file-name))
                      (substring (md5 (format "%s%s" (emacs-pid) (current-time))) 0 4)
                      var var var)))))

;; php-mode
(defun develop-php()
  "PHP development."
  (interactive)
  (package-install 'php-mode)
  (package-install 'company-php))
(add-hook 'php-mode-hook
          (lambda() (add-to-list 'company-backends 'company-ac-php-backend)))

;; terraform-mode
(defun develop-terraform()
  "Terraform development."
  (interactive)
  (package-install 'company-terraform)
  (package-install 'terraform-doc))
(add-hook 'terraform-mode-hook
          (lambda() (add-to-list 'company-backends 'company-terraform)))

;; ansible-mode
(defun develop-ansible ()
  "Ansible development."
  (interactive)
  (package-installs 'ansible 'ansible-doc 'company-ansible))
(add-hook 'ansible-hook
          (lambda()
            (ansible-doc-mode) (yas-minor-mode-on)
            (add-to-list 'company-backends 'company-ansible)))
(use-package ansible-doc
  :defer t
  :config (define-key ansible-doc-mode-map (kbd "M-?") #'ansible-doc))

;; java-mode
(defun develop-java()
  "Java development.
https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  (interactive)
  (package-install 'lsp-java))
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

;; web-mode; js, ts mode
(defun develop-web()
  "WEB development.
npm i -g typescript-language-server; npm i -g typescript"
  (interactive)
  (package-installs 'web-mode 'eslint-fix 'typescript-mode))
(use-package web-mode
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))
(add-hook 'typescript-mode-hook #'lsp-deferred)


;; erlang
(add-hook 'erlang-mode-hook #'lsp-deferred)

;; gitlab-mode
(defun develop-gitlab-ci()
  "Gitlab-CI development."
  (interactive)
  (package-installs 'gitlab-ci-mode 'gitlab-pipeline
                    'gitlab-ci-mode-flycheck))
(defun gitlab-ci-mode-my-hook ()
  "Gitlab ci my hook."
  (gitlab-ci-mode-flycheck-enable)
  (if (fboundp 'flycheck-mode) (flycheck-mode)))
(add-hook 'gitlab-ci-mode-hook #'gitlab-ci-mode-my-hook)

(defun develop-vagrant()
  "Vagrant tools."
  (interactive)
  (package-installs 'vagrant 'vagrant-tramp))

(defun develop-docker()
  "Docker tools."
  (interactive)
  (package-installs 'dockerfile-mode 'docker))

(defun develop-kubernetes()
  "Kubernetes tools."
  (interactive)
  (package-installs 'kubel 'k8s-mode))
(add-hook 'k8s-mode-hook (lambda () (yas-minor-mode-on)))

;; keep personal settings not in the .emacs file
(let ((personal-settings (locate-user-emacs-file "personal.el")))
  (when (file-exists-p personal-settings)
    (load-file personal-settings)))

(run-with-idle-timer
 0.1 nil
 (lambda()
   (message (format "init-time %.03fs"
                    (float-time (time-subtract after-init-time before-init-time))))))

(provide '.emacs)
;;; .emacs ends here
