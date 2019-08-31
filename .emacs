;;; .emacs --- initialization file
;;; Commentary: by TxGVNN

;;; Code:
(when (version< emacs-version "25.1")
  (error "Requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))
(setq gc-cons-threshold (* 64 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("txgvnn" . "https://txgvnn.github.io/packages/"))
(package-initialize)

;;; BOOTSTRAP `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; PACKAGES
;; ivy
(use-package ivy
  :ensure t
  :init (ivy-mode)
  :bind
  ("C-x C-r" . ivy-resume)
  :config
  (setq ivy-extra-directories '("./"))
  (setq ivy-on-del-error-function #'ignore)
  (setq ivy-magic-tilde nil)
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-action)
  ;; Can not exit minibuffer - https://github.com/abo-abo/swiper/issues/1953
  (defvar ivy-recursive-restore-in-progress nil)
  (defun ivy-note-when-inside-recursive-restore (orig-fun &rest args)
    (let ((ivy-recursive-restore-in-progress t))
      (apply orig-fun args)))
  (defun ivy-no-read-while-exiting-recursion (orig-fun &rest args)
    (if ivy-recursive-restore-in-progress
        (error "Cannot use `ivy-read' while restoring recursive state")
      (apply orig-fun args)))
  (advice-add 'ivy-recursive-restore :around
              #'ivy-note-when-inside-recursive-restore)
  (advice-add 'ivy-read :around #'ivy-no-read-while-exiting-recursion))

;; counsel
(use-package counsel :pin txgvnn
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("M-X" . execute-extended-command)
  ("C-x C-f" . counsel-find-file)
  ("C-x B" . counsel-switch-buffer)
  ("C-c m" . counsel-imenu)
  ("M-y" . counsel-yank-pop)
  ("M-Y" . yank-pop)
  ("M-s d" . counsel-ag)
  ("C-h b" . counsel-descbinds)
  (:map counsel-find-file-map ("C-k" . counsel-up-directory))
  :hook
  (org-mode . (lambda() (define-key org-mode-map (kbd "C-c m") 'counsel-org-goto)))
  :config
  (setq counsel-yank-pop-separator
        (concat "\n" (apply 'concat (make-list 25 "---")) "\n"))
  (setq counsel-find-file-at-point t)
  (use-package smex :ensure t))

;; swiper
(use-package swiper
  :ensure t
  :config
  (defun swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol))) (swiper sym))
  :bind ("M-s w" . swiper-at-point))

;; avy
(use-package avy
  :ensure t
  :bind ("M-g a" . avy-goto-char)
  :bind ("M-g l" . avy-goto-line))

;; crux
(use-package crux
  :pin txgvnn
  :ensure t
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
  ("C-x x e" . crux-open-with)
  ("C-x 7" . crux-swap-windows))

;; vlf - view large files
(use-package vlf
  :ensure t
  :config (require 'vlf-setup))

;; move-text
(use-package move-text
  :ensure t
  :bind
  ("M-g <up>" . move-text-up)
  ("M-g <down>" . move-text-down))

;; switch-window
(use-package ace-window
  :ensure t
  :init (global-set-key (kbd "C-x o") 'ace-window)
  :config (setq aw-scope (quote frame)))

;; checker kbd("C-h .")
(if (version< emacs-version "26.1")
    (use-package flycheck
      :ensure t
      :hook (prog-mode . flycheck-mode))
  (use-package flymake
    :config
    (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer)
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    :hook (prog-mode . flymake-mode)))

;; git-gutter
(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  :config (setq git-gutter:lighter "")
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  :bind
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g s" . git-gutter:stage-hunk)
  ("C-x g r" . git-gutter:revert-hunk))
;; magit
(use-package magit
  :ensure t
  :config (define-key magit-file-mode-map (kbd "C-x g") nil)
  :bind
  ("C-x g g" . magit-status)
  ("C-x g f" . magit-find-file)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-mode-line-prefix "")
  (projectile-mode)
  :config
  (setq projectile-project-compilation-cmd "make ")
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))
;; counsel-projectile
(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; perspective
(use-package perspective
  :ensure t
  :init
  (setq persp-mode-prefix-key (kbd "C-z"))
  (setq persp-initial-frame-name "0")
  (persp-mode)
  :config
  (global-set-key (kbd "<f5>") 'persp-switch-last)
  (define-key perspective-map (kbd "z") 'perspective-map))

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
  :init (volatile-highlights-mode))
;; anzu
(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :config
  (define-key symbol-overlay-map (kbd "N") 'symbol-overlay-switch-forward)
  (define-key symbol-overlay-map (kbd "P") 'symbol-overlay-switch-backward)
  (define-key symbol-overlay-map (kbd "c") 'symbol-overlay-remove-all)
  :bind ("M-s H" . symbol-overlay-put)
  :hook (prog-mode . symbol-overlay-mode))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c y v") 'yas-visit-snippet-file)
  (define-key yas-minor-mode-map (kbd "C-c y TAB") yas-maybe-expand)
  :hook
  ((prog-mode org-mode markdown-mode)
   . yas-minor-mode))
;; My yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t :pin txgvnn)

;; company
(use-package company
  :ensure t
  :init (global-company-mode)
  :config (setq company-lighter-base "@")
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (defun company-complete-custom (&optional prefix)
    "Company and Yasnippet(PREFIX)."
    (interactive "P")
    (if (company--active-p) (company-cancel))
    (if prefix
        (if (not company-mode) (yas-expand)
          (call-interactively 'company-yasnippet))
      (call-interactively 'company-complete)))
  :bind ("M-]" . company-complete-custom))

;; undo-tree
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; themes
(use-package doom-themes :pin txgvnn
  :ensure t
  :init (load-theme 'doom-one t)
  :config (doom-themes-org-config))

;;; OPTIONS
;; which-key
(cond ((package-installed-p 'which-key)
       (setq which-key-lighter "")
       (which-key-mode)))
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

(defun develop-utils()
  "Utility packages."
  (interactive)
  (package-install 'json-mode)
  (package-install 'google-translate))

;;; HOOKS
(defun add-to-hooks (func &rest hooks)
  "Add FUNC to mutil HOOKS."
  (dolist (hook hooks) (add-hook hook func)))
;; enable whitespace-mode
(add-to-hooks 'whitespace-mode
              'prog-mode-hook 'org-mode-hook)
;; flymake on g-n & g-p bindings
(add-hook 'flymake-mode-hook
          (lambda()
            (setq next-error-function #'flymake-goto-next-error)
            (setq previous-error-function #'flymake-goto-prev-error)))
;; Apply .dir-locals to major-mode after load .dir-local
;; https://stackoverflow.com/questions/19280851/how-to-keep-dir-local-variables-when-switching-major-modes
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

;; hide the minor modes
(defvar hidden-minor-modes
  '(whitespace-mode ivy-mode smartparens-mode volatile-highlights-mode symbol-overlay-mode))
(defun purge-minor-modes ()
  "Dont show on modeline."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

;;; CUSTOMIZE
;; defun
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
          (message (format "Yanked %s (%s)" filename (what-line))))))
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
(defun copy-to-clipboard ()
  "Copy to clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn (message "Yanked region to x-clipboard!")
             (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!"))))
(defun paste-from-clipboard ()
  "Paste from clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn (clipboard-yank))
    (insert (shell-command-to-string "xsel -o -b"))))
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
(defun linux-stat-file()
  "Run stat command in linux in current file."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory
                    (buffer-file-name))))
    (when filename
      (shell-command (format "stat %s; file %s" filename filename)))))
(defun save-region-to-temp ()
  "Save region to a new temp file."
  (interactive)
  (let ((filename
         (make-temp-file
          (format "%s_%s_" (buffer-name)
                  (format-time-string "%m-%d_%H-%M" (time-to-seconds)))
          nil (file-name-extension (buffer-name) t))))
    (if (region-active-p)
        (write-region (point) (mark) filename)
      (write-region (point-min) (point-max) filename))
    (switch-to-buffer (find-file-noselect filename))))
(defun eww-search-local-help ()
  "Search with keyword from local-help."
  (interactive)
  (let ((help (help-at-pt-kbd-string)))
    (if help (eww help) (message "Nothing!"))))
(defun share-to-online (downloads)
  "Share buffer to online.
- DOWNLOADS: The max-downloads"
  (interactive "p")
  (let ((temp-file
         (make-temp-file ".sharing." nil (file-name-extension (buffer-name) t)))
        (msg "") file-hash)
    (if (region-active-p)
        (write-region (point) (mark) temp-file)
      (write-region (point-min) (point-max) temp-file))
    (when (yes-or-no-p "Encrypt?")
      (let (( file-hash (md5 (buffer-string))))
        (shell-command (format "openssl aes-128-cbc -md md5 -k %s -in '%s' -out '%s.enc'"
                               file-hash temp-file temp-file))
        (dired-delete-file temp-file)
        (setq temp-file (format "%s.enc" temp-file))
        (setq msg (format "# openssl aes-128-cbc -d -md md5 -k %s -in %s 2>/dev/null"
                          file-hash (file-name-nondirectory temp-file)))))
    (when (yes-or-no-p (format "Share online (%d)?" downloads))
      (message "%s %s"
               (shell-command-to-string
                (format "curl -q -H 'Max-Downloads: %d' --upload-file '%s' https://transfer.sh 2>/dev/null"
                        downloads temp-file))
               msg)
      (dired-delete-file temp-file))))

(defvar linum-func
  (if (fboundp 'display-line-numbers-mode)
      'display-line-numbers-mode 'linum-mode))
(defun goto-line-with-feedback ()
  "Show line numbers temporarily when goto-line."
  (interactive)
  (unwind-protect
      (progn
        (funcall linum-func)
        (goto-line (read-number "Goto line: ")))
    (funcall linum-func 0)))
(global-set-key [remap goto-line] #'goto-line-with-feedback)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x j") 'mode-line-other-buffer)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "M-s e") 'eww)
(global-set-key (kbd "M-s E") 'eww-search-local-help)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s r") 'isearch-backward-regexp)
(global-set-key (kbd "M-#") 'mark-backword)
(global-set-key (kbd "C-M-_") 'dabbrev-completion)
(global-set-key (kbd "C-x x .") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x x ;") 'indent-and-delete-trailing-whitespace)
(global-set-key (kbd "C-x x b") 'rename-buffer)
(global-set-key (kbd "C-x x o") 'org-agenda)
(global-set-key (kbd "C-x x p") 'yank-file-path)
(global-set-key (kbd "C-x x r") 'revert-buffer)
(global-set-key (kbd "C-x x a") 'linux-stat-file)
(global-set-key (kbd "C-x x n") 'insert-temp-filename)
(global-set-key (kbd "C-x x x") 'save-region-to-temp)
(global-set-key (kbd "C-x x s") 'share-to-online)
(global-set-key (kbd "C-x x t") 'untabify)
(global-set-key (kbd "C-x x T") 'tabify)
(global-set-key (kbd "C-x x M-w") 'copy-to-clipboard)
(global-set-key (kbd "C-x x C-y") 'paste-from-clipboard)
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)
(global-set-key (kbd "C-x 4 C-v") 'scroll-other-window)
(global-set-key (kbd "C-x 4 M-v") 'scroll-other-window-down)
(global-set-key (kbd "C-x 4 M-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "C-x 4 M->") 'end-of-buffer-other-window)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(prefer-coding-system 'utf-8)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      tramp-auto-save-directory `,(concat user-emacs-directory "backups"))
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
 '(enable-local-variables :all)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/.gxt/org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-enforce-todo-dependencies t)
 '(org-todo-keyword-faces (quote (("BLOCKED" . error) ("WIP" . warning))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WIP(w)" "BLOCKED(b)" "|" "REJECTED(r)"))))
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
    (face tabs trailing space-before-tab newline empty tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-virtual ((t (:inherit unspecified :foreground unspecified))))
 '(symbol-overlay-default-face ((t (:inherit bold :underline t))))
 '(vc-state-base ((t (:inherit font-lock-string-face :weight bold)))))

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
                ;; mode-line-frame-identification -- this is for text-mode emacs only
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

;;; LANGUAGES
;; .emacs
(defun develop-dot()
  "Update 'user-init-file - .emacs."
  (interactive)
  (let (upstream)
    (setq upstream (make-temp-file ".emacs"))
    (url-copy-file "https://raw.githubusercontent.com/TxGVNN/dots/master/.emacs" upstream t)
    (diff user-init-file upstream)
    (other-window 1 nil)
    (message "Override %s by %s to update" user-init-file upstream)))

;; summary-mode
(eval-after-load 'gnus-summary-mode
  (setq gnus-summary-line-format "%U%R%z %d %-23,23f (%4,4L) %{%B%}%s\n"
        gnus-sum-thread-tree-root            ""
        gnus-sum-thread-tree-false-root      "──> "
        gnus-sum-thread-tree-leaf-with-other "├─> "
        gnus-sum-thread-tree-vertical        "│ "
        gnus-sum-thread-tree-single-leaf     "└─> "))

;; c-mode
(defun my-c-mode-common-hook ()
  "C-mode hook."
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; go-mode
(defun develop-go()
  "Go development.
Please install:
   go get -u golang.org/x/tools/cmd/...
   go get -u golang.org/x/tools/cmd/goimports
   go get -u golang.org/x/tools/cmd/guru
   go get -u github.com/rogpeppe/godef/...
   go get -u github.com/nsf/gocode
   go get -u github.com/dougm/goflymake"
  (interactive)
  (package-install 'go-projectile)
  (package-install 'company-go))
(use-package go-projectile
  :defer t
  :init
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode)                    ; highlight identifiers
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

;; python-mode
(defun develop-python()
  "Python development.
Please install:
   pip install python-language-server"
  (interactive)
  (package-install 'lsp-mode)
  (package-install 'company-lsp))
(with-eval-after-load 'python ;; built-in
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
    (let (var)
      (setq var (substring-no-properties (thing-at-point 'symbol)))
      (move-end-of-line nil)
      (newline-and-indent)
      (insert (format "print(\"%d:%s: {}\".format(%s))" (line-number-at-pos) var var)))))
(add-hook 'python-mode-hook 'lsp)

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
  :config
  (define-key ansible-doc-mode-map (kbd "M-?") #'ansible-doc))

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
  :init
  (add-hook 'java-mode-hook
            (lambda () (require 'lsp-java) (lsp))))

;; html-mode
(defun develop-html()
  "HTML development."
  (interactive)
  (package-install 'indent-guide))
(use-package indent-guide
  :defer t
  :hook (html-mode . indent-guide-mode)
  :config (set-face-foreground 'indent-guide-face "dimgray"))

;; js-mode
(defun develop-js()
  "JS development.
npm i -g javascript-typescript-langserver"
  (interactive)
  (package-install 'lsp-mode)
  (package-install 'company-lsp))
(use-package lsp-mode
  :defer t
  :config
  (require 'lsp)
  (require 'lsp-clients)
  :hook
  (js-mode . (lambda() (lsp)
               (define-key js-mode-map (kbd "M-.") 'xref-find-definitions))))

;; gitlab-mode
(defun develop-gitlab-ci()
  "Gitlab-CI development."
  (interactive)
  (package-install 'gitlab-ci-mode)
  (package-install 'gitlab-ci-mode-flycheck))
(defun gitlab-ci-mode-my-hook ()
  (gitlab-ci-mode-flycheck-enable)
  (if (fboundp 'flycheck-mode)
      (flycheck-mode)))
(add-hook 'gitlab-ci-mode-hook 'gitlab-ci-mode-my-hook)

;;; PATCHING
(with-eval-after-load 'flycheck
  (defun flycheck-display-error-at-point-soon () nil)
  (setq flycheck-mode-line-prefix "FC"
        flycheck-highlighting-mode (quote columns)))

(unless (version< emacs-version "26.1")
  (use-package advice-patch
    :ensure t
    :config
    ;; flymake--highlight-line only a char
    (with-eval-after-load 'flymake
      (setq byte-compile-warnings nil)
      (advice-patch 'flymake--highlight-line  '(+ 1 (flymake--diag-beg diagnostic)) '(flymake--diag-end diagnostic))
      (advice-patch 'flymake--mode-line-format '" FlyM" '" Flymake")
      (setq byte-compile-warnings t))))

(with-eval-after-load 'perspective
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

  (defun ivy-switch-buffer-with-persp (&optional _)
    "Clone from persp-switch-to-buffer."
    (interactive)
    (let (buffer)
      (setq buffer (window-normalize-buffer-to-switch-to (read-buffer-to-switch "Switch to buffer: ")))
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

  ;; find file with perspective and projectile
  (defun counsel-find-file-action (x)
    "Find file X."
    (with-ivy-window
      (if (and counsel-find-file-speedup-remote
               (file-remote-p ivy--directory))
          (let ((find-file-hook nil))
            (find-file (expand-file-name x ivy--directory)))
        (if (and (bound-and-true-p persp-mode) (bound-and-true-p projectile-mode))
            (let (project-name (project-name-root (projectile-project-root (expand-file-name x))))
              (when project-name-root
                (setq project-name (funcall projectile-project-name-function project-name-root))
                (persp-switch project-name))))
        (find-file (expand-file-name x ivy--directory))))))

(with-eval-after-load 'counsel-projectile
  (advice-patch 'counsel-projectile-switch-project-by-name
                '(run-hook-with-args 'projectile-before-switch-project-hook
                                     (funcall projectile-project-name-function project))
                '(run-hooks 'projectile-before-switch-project-hook))
  (add-hook 'projectile-before-switch-project-hook
            (lambda (project-to-switch)
              (if (and project-to-switch (bound-and-true-p persp-mode))
                  (persp-switch project-to-switch))))
  (defun counsel-projectile-find-file-action-find-file-jump (file)
    "Call `counsel-find-file' from FILE's directory."
    (let* ((f (projectile-expand-root file))
           (default-directory (file-name-directory f)))
      (counsel-file-jump)))
  (ivy-add-actions
   'counsel-projectile
   '(("f" counsel-projectile-find-file-action-find-file-jump
      "counsel-file-jump")))
  (ivy-add-actions
   'counsel-projectile-find-file
   '(("f" counsel-projectile-find-file-action-find-file-jump
      "counsel-file-jump"))))

;; keep personal settings not in the .emacs file
(let ((personal-settings "~/.emacs.d/personal.el"))
  (when (file-exists-p personal-settings)
    (load-file personal-settings)))

(provide '.emacs)
;;; .emacs ends here
