;;; .emacs --- initialization file.
;;; Commentary:
;;; _____  _     __    _      _      _
;;;  | |  \ \_/ / /`_ \ \  / | |\ | | |\ |
;;;  |_|  /_/ \ \_\_/  \_\/  |_| \| |_| \|
;;;
;;; [ @author TxGVNN ]

;;; Code:
(when (version< emacs-version "27.1")
  (error "Requires GNU Emacs 27.1 or newer, but you're running %s" emacs-version))

(setq gc-cons-threshold most-positive-fixnum) ;; enable gcmh
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; doom-emacs:docs/faq.org#unset-file-name-handler-alist-temporarily
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist doom--file-name-handler-alist)))
(defvar emacs-config-version "20221204.0238")
(defvar hidden-minor-modes '(whitespace-mode))

(require 'package)
(setq package-archives
      '(("me" . "https://txgvnn.github.io/giaelpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; BOOTSTRAP `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gcmh
  :ensure t
  :init (gcmh-mode)
  :config (add-to-list 'hidden-minor-modes 'gcmh-mode))

;;; COMPLETION SYSTEM: vertico, orderless, marginalia, consult, embark
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (delete ".git/" completion-ignored-extensions)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :bind ("C-x C-r" . vertico-repeat)
  (:map vertico-map
        ("<prior>" . vertico-scroll-down)
        ("<next>" . vertico-scroll-up)))
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package marginalia
  :ensure t :defer t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t :defer t
  :custom
  (orderless-matching-styles
   '(orderless-regexp orderless-literal orderless-initialism))
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (minibuffer (initials)))))

(use-package consult
  :ensure t :defer t :pin me
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g e" . consult-error)
  ("M-s w" . consult-line-at-point)
  ("M-s g" . consult-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s F" . consult-find)
  ("M-s u" . consult-focus-lines)
  ("C-x / k" . consult-keep-lines)
  ("M-X" . consult-mode-command)
  ("C-x B" . consult-buffer)
  (:map minibuffer-local-map ("M-r" . consult-history))
  :init
  ;; @FIXME: Disable `consult-completion-in-region' buggy in (e)shell-mode (tramp), minibuffer (compile command)
  ;; (setq completion-in-region-function
  ;;       (lambda (&rest args)
  ;;         (apply (if (and (fboundp 'vertico-mode) vertico-mode)
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region) args)))
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  :config
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        consult-preview-key (kbd "C-l"))
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history)
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
      (consult-line (regexp-quote thing)))))
(use-package embark
  :ensure t :defer t
  :bind ("C-c /" . embark-act)
  (:map minibuffer-local-map ("M-o" . embark-act))
  (:map embark-general-map ("/" . embark-chroot))
  (:map embark-region-map ("M-&" . async-shell-from-region))
  (:map embark-file-map
        ("s" . embark-run-shell)
        ("t" . embark-run-term)
        ("T" . embark-run-vterm)
        ("v" .  magit-status-setup-buffer)
        ("+" . embark-make-directory)
        ("x" . consult-file-externally))
  :config
  (setq embark-indicators '(embark-minimal-indicator))
  ;; as chroot
  (defun embark-chroot (dir &optional prefix)
    "Run CMD in directory DIR."
    (interactive "DIn directory:\nP")
    (let ((default-directory (file-name-directory dir))
          (embark-quit-after-action t)
          (action (embark--prompt
                   (mapcar #'funcall embark-indicators)
                   (embark--action-keymap 'file nil) `(,(list :type 'file :target `,dir)))))
      (command-execute action)))
  ;; project become
  (defun embark-become-project (&optional _target)
    (interactive "s") ; prompt for _target and ignore it
    (embark--quit-and-run
     (lambda ()
       (let ((use-dialog-box nil)
             (this-command 'project-switch-project))
         (command-execute this-command)))))
  (define-key embark-general-map (kbd "p") #'embark-become-project)
  ;; perspective
  (defun embark-persp-to-buffer (&optional _target)
    (interactive "s") ; prompt for _target and ignore it
    (embark--quit-and-run
     (lambda () (command-execute #'persp-switch-to-buffer))))
  (define-key embark-general-map (kbd "P") #'embark-persp-to-buffer)
  ;; region
  (add-to-list 'embark-target-injection-hooks
               '(async-shell-from-region embark--allow-edit))
  ;; term
  (defun embark-run-term(dir)
    "Create or visit a ansi-term buffer."
    (interactive "D")
    (let ((default-directory (file-name-directory dir)))
      (crux-visit-term-buffer t)))
  (defun embark-run-shell(dir)
    "Create or visit a shell buffer."
    (interactive "D")
    (let ((default-directory (file-name-directory dir)))
      (crux-visit-shell-buffer t)))
  (defun embark-run-vterm(dir)
    "Create or visit a vterm buffer."
    (interactive "D")
    (let ((default-directory (file-name-directory dir)))
      (crux-visit-vterm-buffer t)))
  (defun embark-make-directory(dir)
    (interactive "D")
    (make-directory dir)
    (find-file dir)))
(use-package embark-consult
  :ensure t :defer t
  :init
  (with-eval-after-load 'consult
    (with-eval-after-load 'embark
      (require 'embark-consult))))

;;; VERSION CONTROL: git-gutter, magit, git-link
(use-package git-gutter
  :ensure t :defer t
  :init
  (setq git-gutter:lighter ""
        git-gutter:disabled-modes '("fundamental-mode"))
  (defun git-gutter-mode-turn-on-custom ()
    "Enable git-gutter except TRAMP."
    (when (not (file-remote-p default-directory))
      (git-gutter-mode +1)))
  :hook
  (prog-mode . git-gutter-mode-turn-on-custom)
  (magit-post-refresh . git-gutter:update-all-windows)
  :bind
  ("C-x v p" . git-gutter:previous-hunk)
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v s" . git-gutter:stage-hunk)
  ("C-x v r" . git-gutter:revert-hunk))
(use-package magit
  :ensure t :defer t
  :config
  (defun magit-find-file-on-project (project rev path)
    (let ((default-directory project))
      (magit-find-file rev path)))
  :bind
  ("C-x v f" . magit-find-file))
(use-package git-link
  :ensure t :defer t
  :config (setq git-link-use-commit t))

;;; SEARCHING: ripgrep, anzu, engine-mode
(use-package isearch :defer t
  :init
  (global-set-key (kbd "M-s s") 'isearch-forward-regexp)
  (global-set-key (kbd "M-s %") 'query-replace-regexp)
  (define-key isearch-mode-map (kbd "M-s %") 'isearch-query-replace-regexp))
(use-package anzu
  :ensure t :defer t
  :hook (after-init . global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
(use-package isearch-mb
  :ensure t :after anzu
  :init (isearch-mb-mode)
  :config
  (add-to-list 'isearch-mb--after-exit #'anzu-isearch-query-replace)
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "M-%") 'anzu-isearch-query-replace)
  (define-key isearch-mb-minibuffer-map (kbd "M-s %") 'isearch-query-replace-regexp))
(use-package rg :ensure t :defer t)
(use-package engine-mode
  :ensure t :defer 1
  :config
  (setq engine/browser-function 'eww-browse-url)
  (defengine vagrant-box
    "https://app.vagrantup.com/boxes/search?provider=libvirt&q=%s&utf8=%%E2%%9C%%93")
  (defengine debian-package
    "https://packages.debian.org/search?searchon=names&keywords=%s")
  (defengine alpine-apk-file
    "https://pkgs.alpinelinux.org/contents?file=%s&path=&name=&branch=edge&arch=x86_64")
  (defengine ubuntu-package
    "https://packages.ubuntu.com/search?keywords=%s&searchon=names&suite=all&section=all"))

;;; WORKSPACE: project, perspective, envrc
(use-package pkg-info :ensure t :defer t)
(use-package project :defer t
  :bind
  (:map project-prefix-map
        ("t" . project-term)
        ("j" . project-jump-persp)
        ("T" . project-vterm)
        ("M-x" . project-execute-extended-command)
        ("o" . project-org-capture)
        ("O" . project-org-go)
        ("v" . magit-project-status))
  :config
  (unless (version= (package-version-join (pkg-info-package-version 'project)) "0.9.2")
    (user-error "Require `project-0.9.2', please install from ELPA"))
  (advice-add #'project-find-file :override #'project-find-file-cd)
  (defun project-find-file-cd (&optional include-all)
    "Project-find-file set default-directory is project-root"
    (interactive)
    (let* ((pr (project-current t))
           (default-directory (project-root pr))
           (dirs (list default-directory)))
      (project-find-file-in (thing-at-point 'filename) dirs pr include-all)))
  (setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (defun project-shell ()
    "Override `project-shell'."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "shell"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if current-prefix-arg
          (shell (generate-new-buffer-name default-project-shell-name))
        (if (get-buffer-process shell-buffer)
            (pop-to-buffer-same-window shell-buffer)
          (shell default-project-shell-name)))))
  (defun project-consult-grep (&optional initial)
    "Using consult-grep(INITIAL) in project."
    (interactive)
    (consult-grep (project-root (project-current t)) initial))
  (define-key project-prefix-map (kbd "g") #'project-consult-grep)
  (define-key project-prefix-map (kbd "G") #'project-find-regexp)
  (defun project-consult-ripgrep (&optional initial)
    "Using consult-ripgrep(INITIAL) in project."
    (interactive)
    (consult-ripgrep (project-root (project-current t)) initial))
  (define-key project-prefix-map (kbd "r") #'project-consult-ripgrep)
  (define-key project-prefix-map (kbd "R") #'project-query-replace-regexp)
  ;; term
  (defun project-term ()
    "project-term."
    (interactive)
    (let* ((default-directory (cdr (project-current t)))
           (termname (format "%s-term" (file-name-nondirectory
                                        (directory-file-name default-directory))))
           (buffer (format "*%s*" termname)))
      (unless (get-buffer buffer)
        (require 'term)
        (ansi-term (or explicit-shell-file-name (getenv "SHELL") "/bin/sh") termname))
      (switch-to-buffer buffer)))
  (defun project-vterm ()
    "project-vterm."
    (interactive)
    (let* ((default-directory (cdr (project-current t)))
           (buffer (format "*%s-vterm*" (file-name-nondirectory
                                         (directory-file-name default-directory)))))
      (unless (get-buffer buffer)
        (require 'vterm)
        (vterm buffer))
      (switch-to-buffer buffer)))
  ;; embark
  (defun embark-on-project()
    (interactive)
    (require 'embark nil t)
    (embark-chroot (project-root (project-current t))))
  (define-key project-prefix-map (kbd "/") #'embark-on-project)
  ;; org-capture
  (defun project-org-capture ()
    "Capture to project dir."
    (interactive)
    (unless (bound-and-true-p org-default-notes-file) (require 'org-capture))
    (let* ((project (project-root (project-current t)))
           (org-default-notes-file (concat project "tasks.org")))
      (call-interactively 'org-capture)))
  (defun project-org-go ()
    "Jump to project org file."
    (interactive)
    (let* ((project (project-root (project-current t)))
           (org-default-notes-file (concat project "tasks.org")))
      (find-file org-default-notes-file)))
  (defun project-jump-persp ()
    "Just jump to persp of project."
    (interactive)
    (let ((dir (project-root (project-current t))))
      (persp-switch dir)))
  ;; switch commands
  (setq project-switch-commands
        '((project-find-file "file")
          (project-consult-ripgrep "rg")
          (project-consult-grep "grep")
          (project-compile "compile")
          (project-switch-to-buffer "buf")
          (project-term "term")
          (project-shell "shell")
          (magit-project-status "git")
          (project-jump-persp "jump")
          (embark-on-project "embark"))))
(use-package envrc ;; direnv > 2.7
  :ensure t :defer t
  :config
  (setq envrc-none-lighter nil
        envrc-on-lighter '(:propertize " env" face envrc-mode-line-on-face)
        envrc-error-lighter '(:propertize " env" face envrc-mode-line-error-face))
  :hook (after-init . envrc-global-mode))
(use-package perspective
  :ensure t :pin me
  :init
  (setq persp-mode-prefix-key (kbd "C-z")
        persp-initial-frame-name "0")
  (persp-mode)
  :bind
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x x" . persp-switch-last)
  ("<f5>" . persp-switch-last)
  (:map perspective-map ("z" . perspective-map))
  :config
  ;; buffer
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-command-categories '(persp-switch-to-buffer* . buffer)))
  ;; hack local var when switch
  (add-hook 'persp-switch-hook #'hack-dir-local-variables-non-file-buffer)
  ;; persp-ibuffer
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (with-eval-after-load 'ibuffer
    (require 'ibuf-ext)
    (advice-add #'ibuffer-visit-buffer :override #'ibuffer-visit-buffer-persp)
    (defun ibuffer-visit-buffer-persp (&optional single)
      "Override 'ibuffer-visit-buffer with support perspective."
      (interactive "P")
      (let ((buffer (ibuffer-current-buffer t)))
        (if (bound-and-true-p persp-mode)
            (unless (persp-is-current-buffer buffer)
              (let ((other-persp (persp-buffer-in-other-p buffer)))
                (persp-switch (cdr other-persp)))))
        (switch-to-buffer buffer)
        (when single (delete-other-windows)))))
  (with-eval-after-load 'project
    (defun project-switch-project (dir)
      "Override 'project-switch-project with support perspective."
      (interactive (list (project-prompt-project-dir)))
      (let ((command (if (symbolp project-switch-commands)
                         project-switch-commands
                       (project--switch-project-command))))
        (persp-switch dir)
        (let ((project-current-directory-override dir))
          (call-interactively command)))))
  ;; find-file
  (advice-add #'find-file :override #'find-file-persp)
  (defun find-file-persp (filename &optional wildcards)
    "Override 'find-file(FILENAME WILDCARDS)."
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (if-let* ((bound-and-true-p persp-mode)
              (pr (project-current nil (file-name-directory filename)))
              (dir (project-root pr)))
        (persp-switch dir))
    (let ((value (find-file-noselect filename nil nil wildcards)))
      (if (listp value)
          (mapcar 'pop-to-buffer-same-window (nreverse value))
        (pop-to-buffer-same-window value))))
  ;; compile
  (with-eval-after-load 'compile
    (defvar persp-compile-history (make-hash-table :test 'equal))
    (defun persp--get-command-history (persp)
      (or (gethash persp persp-compile-history)
          (puthash persp (make-ring 16) persp-compile-history)))
    (advice-add #'compilation-read-command :override #'compilation-read-command-persp)
    (defun compilation-read-command-persp (command)
      "Override compilation-read-command (COMMAND)."
      (let* ((persp-name (if (bound-and-true-p persp-mode)
                             (persp-name (persp-curr)) "0"))
             (compile-history
              (ring-elements (persp--get-command-history persp-name))))
        (ring-insert (persp--get-command-history persp-name)
                     (read-shell-command (format "Compile [%s]: " default-directory)
                                         (or (car compile-history) command)
                                         (if (equal (car compile-history) command)
                                             '(compile-history . 1)
                                           'compile-history))))))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'persp-compile-history)))
;; project-temp-root
(defvar project-temp-root "~/projects/")
(defun project-temp-M-x (&optional prefix)
  "With PREFIX we will set `project-temp-root'."
  (interactive "P")
  (if prefix (setq project-temp-root (read-directory-name "Select dir: ")))
  (unless (fboundp 'embark-chroot) (require 'embark))
  (embark-chroot project-temp-root))
(global-set-key (kbd "C-x P") #'project-temp-M-x)

;;; DISPLAY ENHANCE
(use-package smartparens
  :ensure t :defer t
  :config (require 'smartparens-config)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda()(turn-off-smartparens-mode)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda()(turn-on-smartparens-mode)))
  (add-to-list 'hidden-minor-modes 'smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . 'sp-forward-sexp)
              ("C-M-b" . 'sp-backward-sexp))
  :hook
  (markdown-mode . smartparens-mode)
  (prog-mode . smartparens-mode))
(use-package rainbow-mode
  :ensure t :defer t
  :hook (prog-mode . rainbow-mode)
  :config (add-to-list 'hidden-minor-modes 'rainbow-mode))
(use-package rainbow-delimiters
  :ensure t :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode)
  :config (add-to-list 'hidden-minor-modes 'volatile-highlights-mode))
(use-package symbol-overlay
  :ensure t :defer t
  :bind ("M-s H" . symbol-overlay-put)
  :hook (prog-mode . symbol-overlay-mode)
  :config (add-to-list 'hidden-minor-modes 'symbol-overlay-mode))
(use-package hl-todo
  :ensure t :defer t
  :hook (prog-mode . hl-todo-mode))
(use-package beacon
  :ensure t :defer t
  :hook (after-init . beacon-mode)
  :config (add-to-list 'hidden-minor-modes 'beacon-mode))


;;; COMPLETION CODE: corfu, yasnippet, eglot, dumb-jump
(use-package corfu
  :ensure t :defer t
  :init (global-corfu-mode)
  :bind
  (:map corfu-map
        ("M-m" . corfu-move-to-minibuffer)
        ("TAB" . corfu-complete-common-or-next) ;; Use TAB for cycling, default is `corfu-complete'.
        ([tab] . corfu-complete-common-or-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (setq completion-cycle-threshold 3
        corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-preselect-first nil
        corfu-history-mode t)
  (defvar-local corfu-common-old nil)
  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate (@minad/corfu#170)."
    (interactive)
    (if (= corfu--total 1)
        (if (not (thing-at-point 'filename))
            (progn
              (corfu--goto 1)
              (corfu-insert))))
    (let* ((input (car corfu--input))
           (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
           (pt (length str))
           (common (try-completion str corfu--candidates)))
      (if (and (> pt 0)
               (stringp common)
               (not (string= str common)))
          (insert (substring common pt))
        (if (equal common corfu-common-old)
            (corfu-next)))
      (setq-local corfu-common-old common)))
  (put 'corfu-complete-common-or-next 'completion-predicate #'ignore)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (defun corfu-move-to-minibuffer ()
    "Move completion to minibuffer instead of corfu."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))
(use-package cape
  :ensure t :defer t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))
(use-package yasnippet
  :ensure t :defer t :pin me
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-lighter " υ")
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
(use-package yasnippet-snippets
  :ensure t :defer t :pin me)
(use-package consult-yasnippet
  :ensure t :defer t
  :init (global-set-key (kbd "M-]") #'completion-customize)
  (defun completion-customize(&optional prefix)
    "Complete and Yasnippet(PREFIX)."
    (interactive "P")
    (if prefix
        (consult-yasnippet nil)
      (call-interactively 'completion-at-point))))
(use-package dumb-jump
  :ensure t :defer t
  :init
  (with-eval-after-load 'xref
    (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)))
(use-package eglot
  :ensure t
  :commands eglot-ensure
  :after (project flymake))

;;; TOOLS: avy, crux, expand-region, move-text, ace-window, vundo|undo-tree,...
(use-package avy
  :ensure t :defer t
  :config
  (setq avy-all-windows nil
        avy-background t)
  :bind
  ("M-g a" . avy-goto-char)
  ("M-g l" . avy-goto-line))
(use-package crux
  :ensure t :defer t :pin me
  :bind
  ("C-^" . crux-top-join-line)
  ("C-a" . crux-move-beginning-of-line)
  ("C-o" . crux-smart-open-line-above)
  ("C-c c" . crux-create-scratch-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c r" . crux-rename-buffer-and-file)
  ("C-c s" . crux-visit-shell-buffer)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c T" . crux-visit-vterm-buffer)
  ("C-h RET" . crux-find-user-init-file)
  ("C-x / e" . crux-open-with)
  ("C-x 7" . crux-swap-windows))
(use-package expand-region
  :ensure t :defer t
  :bind ("M-#" . er/expand-region))
(use-package move-text
  :ensure t :defer t
  :bind
  ("M-g <up>" . move-text-up)
  ("M-g <down>" . move-text-down))
(use-package ace-window
  :ensure t :defer t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope (quote frame)))
(if (version< emacs-version "28.1")
    (use-package undo-tree
      :ensure t
      :init (add-hook 'after-init-hook #'global-undo-tree-mode)
      :config
      (setq undo-tree-mode-lighter ""
            undo-limit 800000           ; 800kb
            undo-strong-limit 12000000  ; 12mb
            undo-outer-limit 128000000 ; 128mb
            undo-tree-history-directory-alist
            `((".*" . ,temporary-file-directory))))
  (use-package vundo
    :ensure t :defer t
    :init (global-set-key (kbd "C-x u") #'vundo)
    :config (define-key vundo-mode-map (kbd "q") #'vundo-confirm)))
(use-package pinentry
  :ensure t :defer t
  :hook (after-init . pinentry-start))
(use-package multiple-cursors
  :ensure t :defer t
  :bind
  ("C-c e a" . mc/mark-all-like-this)
  ("C-c e n" . mc/mark-next-like-this)
  ("C-c e p" . mc/mark-previous-like-this)
  ("C-c e l" . mc/edit-lines)
  ("C-c e r" . mc/mark-all-in-region))
(use-package helpful
  :ensure t :defer t
  :init
  (global-set-key [remap describe-command] 'helpful-command)
  (global-set-key [remap describe-function] 'helpful-function)
  (global-set-key [remap describe-key] 'helpful-key)
  (global-set-key [remap describe-macro] 'helpful-macro)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-symbol] 'helpful-symbol))
(use-package shell-command+
  :ensure t :defer t
  :init (global-set-key (kbd "M-!") #'shell-command+))
(use-package eev
  :ensure t :defer 1
  :config (require 'eev-load)
  (defun eepitch-this-line-or-setup (&optional prefix)
    "Setup eepitch-buffer-name if PREFIX or eval this line."
    (interactive "P")
    (if prefix
        (setq-local eepitch-buffer-name (read-buffer-to-switch "Buffer: "))
      (eepitch-this-line)))
  (global-set-key (kbd "<f8>") #'eepitch-this-line-or-setup))
(use-package so-long
  :ensure t :defer t
  :hook (after-init . global-so-long-mode))

;;; CHECKER: flymake(C-h .)
(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :hook (prog-mode . flymake-mode))

;;; DIRED
(use-package dired :defer t
  :config
  (define-key dired-mode-map (kbd "E") #'dired-ediff-files)
  (defun dired-auto-update-name (&optional suffix)
    "Auto update name with SUFFIX.ext."
    (interactive "p")
    (let* ((filename (file-name-nondirectory (dired-get-file-for-visit)))
           (suffix (replace-regexp-in-string
                    "\n" "" (shell-command-to-string (format "stat %s|grep Change|awk '{print $2\"_\"$3}'" filename)))))
      (rename-file filename (file-name-with-extension filename (format "%s.%s" suffix (file-name-extension filename))) t)
      (revert-buffer)))
  (setq dired-listing-switches "-alh"))
(use-package diredfl
  :ensure t :defer t
  :init (add-hook 'dired-mode-hook 'diredfl-mode))

;;; TERM: shell, term, xclip
(setenv "PAGER" "cat")
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
(use-package xclip ;; -- don't use xsel
  :ensure t :defer t
  :init
  (add-hook 'tty-setup-hook
            (lambda()(require 'xclip nil t)
              (ignore-errors (xclip-mode)))))

;; BUILTIN
(use-package tramp :defer t
  :config (setq tramp-allow-unsafe-temporary-files t))
(use-package ediff
  :ensure nil :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))
(use-package savehist
  :ensure t
  :config (savehist-mode)
  (add-hook 'savehist-save-hook
            (lambda () (setq savehist-minibuffer-history-variables
                             (delete 'eww-prompt-history savehist-minibuffer-history-variables)))))
(use-package autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
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
(use-package compile :defer t
  :init (global-set-key (kbd "C-x m") 'compile)
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output t)
  (defun compile-with-nohistory (command &optional comint)
    "Override compile(COMMAND &optional COMINT)."
    (interactive
     (list
      (let ((command (eval compile-command)))
        (if (or compilation-read-command current-prefix-arg)
            (compilation-read-command command) command))
      (consp current-prefix-arg)))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start command comint))
  (advice-add #'compile :override #'compile-with-nohistory)
  (defun doom-apply-ansi-color-to-compilation-buffer-h ()
    "Applies ansi codes to the compilation buffers."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))
(use-package epg
  :defer t
  :config (setq epg-pinentry-mode 'loopback))
(use-package epa
  :defer t
  :config (setq epa-armor t))

;; MAIL
(use-package gnus :defer t
  :config
  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-summary-line-format "%U%R%z %d %-23,23f (%4,4L) %{%B%}%s\n"
        gnus-sum-thread-tree-root            ""
        gnus-sum-thread-tree-false-root      "──> "
        gnus-sum-thread-tree-leaf-with-other "├─> "
        gnus-sum-thread-tree-vertical        "│ "
        gnus-sum-thread-tree-single-leaf     "└─> "))

;;; THEMES
(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula t))

;; MODELINE
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
  (let ((filename (if (buffer-file-name) (buffer-file-name)
                    default-directory)))
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
(defun insert-temp-filename()
  "Insert new temp filename."
  (interactive)
  (insert
   (concat (file-name-as-directory temporary-file-directory)
           (make-temp-name
            (format "%s_%s_" user-login-name
                    (format-time-string "%Y%m%d-%H%M"))))))
(defun insert-datetime(&optional prefix)
  "Insert YYYYmmdd-HHMM or YYYY-mm-dd_HH-MM if PREFIX set."
  (interactive "p")
  (let ((msg
         (cond
          ((= prefix 1)
           (format-time-string "%Y%m%d-%H%M" (current-time) t))
          ((= prefix 2)
           (string-trim (shell-command-to-string "date --utc")))
          ((= prefix 4)
           (format-time-string "%Y-%m-%d_%H-%M" (current-time) t)))))
    (insert msg)))

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
         (buffer-name (format "%s_%s" (file-name-base (buffer-name))
                              (format-time-string "%Y%m%d_%H%M%S")))
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
                    (format-time-string "%Y%m%d-%H%M%S_")))
          nil (file-name-extension (buffer-name) t))))
    (copy-region-to-scratch filename)))
(defun find-file-rec ()
  "Find a file in the current working directory recursively."
  (interactive)
  (let ((find-files-program
         (cond
          ((executable-find "rg") '("rg" "--color=never" "--files"))
          ((executable-find "find") '("find" "-type" "f")))))
    (find-file
     (completing-read
      "Find file: " (apply #'process-lines find-files-program)))))
(defun eww-search-local-help ()
  "Search with keyword from local-help."
  (interactive)
  (let ((help (help-at-pt-kbd-string)))
    (if help (eww (read-string "Search: " help)) (message "Nothing!"))))

(defun async-shell-from-region (start end &optional command)
  "Run async shell from region(START END &optional COMMAND)."
  (interactive
   (let (string)
     (unless (mark)
       (user-error "The mark is not set now, so there is no region"))
     (setq string (read-shell-command "async-shell: "
                                      (buffer-substring-no-properties (region-beginning) (region-end))))
     (list (region-beginning) (region-end) string)))
  (let ((bufname (car (split-string (substring command 0 (if (< (length command) 9) (length command) 9))))))
    (async-shell-command command (format "*shell:%s:%s*" bufname (format-time-string "%Y%m%d_%H%M%S")))))

(defvar share-to-online-func
  'crux-share-to-transfersh)
(defun share-to-online ()
  "Share buffer to online."
  (interactive)
  (call-interactively share-to-online-func))

(global-set-key (kbd "M-D") 'kill-whole-line)
(global-set-key (kbd "M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-x C-@") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "M-s e") 'eww)
(global-set-key (kbd "M-s E") 'eww-search-local-help)
(global-set-key (kbd "M-s f") 'find-file-rec)
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
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)
(global-set-key (kbd "C-x 4 C-v") 'scroll-other-window)
(global-set-key (kbd "C-x 4 M-v") 'scroll-other-window-down)
(global-set-key (kbd "C-x 4 M-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "C-x 4 M->") 'end-of-buffer-other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "ESC <up>") #'(lambda () (interactive) (previous-line 3)))
(global-set-key (kbd "ESC <down>") #'(lambda () (interactive) (next-line 3)))
(global-set-key (kbd "M-<up>") #'(lambda () (interactive) (previous-line 3)))
(global-set-key (kbd "M-<down>") #'(lambda () (interactive) (next-line 3)))

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq select-safe-coding-system-function t
      create-lockfiles nil
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
 '(eldoc-minor-mode-string " σ")
 '(electric-indent-mode nil)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(ffap-machine-p-known 'reject t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-default-init nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(kill-do-not-save-duplicates t)
 '(menu-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(proced-tree-flag t)
 '(read-quoted-char-radix 16)
 '(ring-bell-function #'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(vc-follow-symlinks nil)
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
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
(advice-add #'base64-encode-region
            :before (lambda (&rest _args)
                      "Pass prefix arg as third arg to `base64-encode-region'."
                      (interactive "r\nP")))
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; DEVELOPMENT ENV
(defun package-installs (&rest packages)
  "Install PACKAGES."
  (dolist (package packages) (package-install package)))

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
(use-package erc :defer t
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-default-server "irc.libera.chat")
  (setq erc-prompt-for-password nil))
(use-package ercn  :defer t
  :hook (ercn-notify . do-notify)
  :config
  (setq ercn-notify-rules
        '((current-nick . all)
          (keyword . all)
          (pal . ("#emacs" "#guix"))
          (query-buffer . all)))
  (defun do-notify (nick msg)
    (call-process "notify-send" nil nil nil "ERC" nick)))

;; org-mode
(use-package org :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . flyspell-mode)
  :config
  (add-to-list 'hidden-minor-modes 'org-indent-mode)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
  (setq org-babel-load-languages (quote ((emacs-lisp . t) (shell . t)))
        org-enforce-todo-dependencies t
        org-adapt-indentation nil
        org-odd-levels-only nil
        org-hide-leading-stars t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-log-done 'time
        org-todo-keyword-faces (quote (("BLOCKED" . error) ("WIP" . warning)
                                       ("WONTFIX" . (:foreground "gray" :weight bold))))
        org-todo-keywords
        (quote
         ((sequence "TODO(t)" "|" "WONTFIX(W)" "DONE(d)")
          (sequence "WIP(w)" "BLOCKED(b)" "|" "REJECTED(r)")))))
(use-package org-bullets
  :ensure t :defer t
  :init (add-hook 'org-mode-hook #'org-bullets-mode))
(use-package ob-compile :ensure t :defer t
  :config (add-hook 'compilation-finish-functions #'ob-compile-save-file))

;; yaml
(use-package yaml-mode
  :ensure t
  :init (add-hook 'yaml-mode-hook #'eglot-ensure))

;; go-mode
(defun develop-go()
  "Go develoment.
Please install:
   go install golang.org/x/tools/gopls"
  (interactive)
  (package-installs 'go-mode))
(use-package go-mode :defer t
  :config
  (defun lsp-go-install-save-hooks ()
    (if (fboundp 'eglot-ensure)(eglot-ensure))
    (add-hook 'before-save-hook #'eglot-format-buffer t t))
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
   pip install python-lsp-server[all]"
  (interactive))
(add-hook 'python-mode-hook #'eglot-ensure)

(with-eval-after-load 'python ;; built-in
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (defun python-pip-install-requirements()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (async-shell-command "pwd; which pip; pip install -r requirements.txt")))
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
  (package-install 'php-mode))

;; erlang
(add-hook 'erlang-mode-hook #'eglot-ensure)

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
            (ansible-doc-mode)
            (add-to-list 'company-backends 'company-ansible)))
(use-package ansible-doc :defer t
  :config (define-key ansible-doc-mode-map (kbd "M-?") #'ansible-doc))

;; java-mode
(defun develop-java()
  "Java development.
https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  (interactive))
(add-hook 'java-mode-hook #'eglot-ensure)

(defun develop-html()
  "HTML development."
  (interactive)
  (package-install 'indent-guide))
(use-package indent-guide :defer t
  :hook (html-mode . indent-guide-mode)
  :config (set-face-foreground 'indent-guide-face "dimgray"))
(use-package sgml-mode :defer t
  :config
  (define-key html-mode-map (kbd "M-o") #'mode-line-other-buffer))

;; js, ts mode
(defun develop-ts()
  "TS development.
npm i -g typescript-language-server; npm i -g typescript"
  (interactive)
  (package-installs 'eslint-fix 'typescript-mode))
(use-package typescript-mode :defer t
  :hook
  (typescript-mode . eglot-ensure)
  (js-mode . eglot-ensure))

(defun js-print-debug-at-point()
  "Print debug."
  (interactive)
  (let ((var (substring-no-properties (thing-at-point 'symbol))))
    (move-end-of-line nil)
    (newline-and-indent)
    (insert (format "console.log(\"D: %s@%s %s: \", %s);"
                    (file-name-nondirectory (buffer-file-name))
                    (substring (md5 (format "%s%s" (emacs-pid) (current-time))) 0 4)
                    var var))))

(defun develop-gitlab-ci()
  "Gitlab-CI development."
  (interactive)
  (package-installs 'gitlab-ci-mode 'gitlab-pipeline))

(defun develop-vagrant()
  "Vagrant tools."
  (interactive)
  (package-installs 'vagrant 'vagrant-tramp))

(defun develop-docker()
  "Docker tools."
  (interactive)
  (package-installs 'dockerfile-mode 'docker 'docker-tramp 'docker-compose-mode))
(use-package docker :defer t
  :config (setq docker-run-async-with-buffer-function #'docker-run-async-with-buffer-shell))

(defun develop-restclient ()
  "Install restclient."
  (interactive)
  (package-installs 'restclient 'restclient-jq))
(use-package restclient :defer t
  :config (require 'restclient-jq))
(defun develop-kubernetes()
  "Kubernetes tools."
  (interactive)
  (package-installs 'kubel 'kubedoc 'k8s-mode))
(defun develop-keylog ()
  "Keycast and log."
  (interactive)
  (package-installs 'keycast 'interaction-log))
(use-package keycast :defer t
  :config (setq keycast-mode-line-insert-after 'mode-line-misc-info))

;; keep personal settings not in the .emacs file
(let ((personal-settings (locate-user-emacs-file "personal.el")))
  (when (file-exists-p personal-settings)
    (load-file personal-settings)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "init-time %.03fs"
                     (float-time (time-subtract after-init-time before-init-time)))))

(provide '.emacs)
;;; .emacs ends here
