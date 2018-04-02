;;; .emacs --- initialization file
;;; Commentary: by TxGVNN

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; helm
(use-package helm
  :ensure t
  :init
  (require 'helm)
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-mode-line-string "")
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (define-key global-map (kbd "C-c m") 'helm-imenu)
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list))

;;; helm-project
(use-package helm-projectile
  :ensure t
  :init
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;;; helm-swoop
(use-package helm-swoop
  :ensure t
  :bind
  (("M-s s" . helm-swoop))
  )

;;; company
(use-package company
  :ensure t
  :init
  (global-company-mode t)
  )

;;; which-key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; helm-gtags
(use-package helm-gtags
  :ensure t
  :init
  ;;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'java-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'php-mode-hook 'helm-gtags-mode)
  (add-hook 'python-mode-hook 'helm-gtags-mode)
  (eval-after-load "helm-gtags"
    '(progn
       (define-key helm-gtags-mode-map (kbd "C-c t f") 'helm-gtags-find-tag)
       (define-key helm-gtags-mode-map (kbd "C-c t r") 'helm-gtags-find-rtag)
       (define-key helm-gtags-mode-map (kbd "C-c t s") 'helm-gtags-find-symbol)
       (define-key helm-gtags-mode-map (kbd "C-c t g") 'helm-gtags-parse-file)
       (define-key helm-gtags-mode-map (kbd "C-c t p") 'helm-gtags-previous-history)
       (define-key helm-gtags-mode-map (kbd "C-c t n") 'helm-gtags-next-history)
       (define-key helm-gtags-mode-map (kbd "C-c t t") 'helm-gtags-pop-stack) )
    )
  )

;;; git-gutter
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode 1)
  :bind
  (("C-x g p" . git-gutter:previous-hunk)
   ("C-x g p" . git-gutter:previous-hunk)
   ("C-x g n" . git-gutter:next-hunk)
   ("C-x g s" . git-gutter:stage-hunk)
   ("C-x g r" . git-gutter:revert-hunk)
   ))

;;; magit
(use-package magit
  :ensure t
  :bind
  (("C-x g v" . magit-status)
   ("C-x g d" . magit-diff-buffer-file)
   ("C-x g l" . magit-log-buffer-file)
   ("C-x g b" . magit-blame)
   ("C-x g a" . magit-log-all)
   ))

;;; switch-window
(use-package switch-window
  :ensure t
  :init
  (global-set-key (kbd "C-x o") 'switch-window)
  )
(use-package windmove
  :bind
  (("C-c <right>" . windmove-right)
   ("C-c <left>" . windmove-left)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)
   ))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c e a" . mc/mark-all-like-this)
   ("C-c e n" . mc/mark-next-like-this)
   ("C-c e p" . mc/mark-previous-like-this)
   ("C-c e l" . mc/edit-lines)
   )
  )

;; smartparens
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode t)
  )

;;; themes
(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t)
  )

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (add-hook 'after-init-hook #'sml/setup)
  )

;; dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-startup-banner nil)
  (setq dashboard-items
        '((recents . 5) (bookmarks . 5) (projects . 15)))
  (dashboard-setup-startup-hook)
  )

;; hide the minor modes
(defvar hidden-minor-modes
  '(flycheck-mode which-key-mode projectile-mode git-gutter-mode helm-mode undo-tree-mode company-mode helm-gtags-mode))
(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

;;; customize
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-c s") 'rgrep)

(defun indent-buffer ()
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace)
  )
(global-set-key (kbd "C-c ;") 'indent-buffer)
(global-set-key (kbd "C-c r")
                (lambda ()
                  (interactive)
                  (revert-buffer t t t)
                  (message "buffer is reverted"))
                )
;; (add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda()(delete-trailing-whitespace)))

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line nil)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(default-input-method "vietnamese-telex")
 '(delete-old-versions 6)
 '(delete-selection-mode t)
 '(helm-gtags-auto-update t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(keep-new-versions 2)
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/backup")
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; go-mode
(use-package go-guru
  :ensure t
  )
(use-package go-autocomplete
  :ensure t
  )
(use-package go-projectile
  :ensure t
  :init
  (defun my-switch-project-hook ()
    (go-set-project))
  (add-hook 'projectile-after-switch-project-hook 'my-switch-project-hook)
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode)                    ; highlight identifiers
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (auto-complete-mode 1))                         ; Enable auto-complete mode
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete))
  )
;;; python-mode
(use-package jedi
  :ensure t
  :init
  (defvar jedi-config:use-system-python nil)
  (defvar jedi-config:with-virtualenv nil)
  (defvar jedi-config:vcs-root-sentinel ".git")
  (defvar jedi-config:python-module-sentinel "__init__.py")

  (defun get-project-root-with-file (buf repo-file &optional init-file)
    "Guesses that the python root is the less 'deep' of either:
         -- the root directory of the repository, or
         -- the directory before the first directory after the root
            having the init-file file (e.g., '__init__.py'."

    ;; make list of directories from root, removing empty
    (defun make-dir-list (path)
      (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
                        (split-string path "/"))))
    ;; convert a list of directories to a path starting at "/"
    (defun dir-list-to-path (dirs)
      (mapconcat 'identity (cons "" dirs) "/"))
    ;; a little something to try to find the "best" root directory
    (defun try-find-best-root (base-dir buffer-dir current)
      (cond
       (base-dir ;; traverse until we reach the base
        (try-find-best-root (cdr base-dir) (cdr buffer-dir)
                            (append current (list (car buffer-dir)))))

       (buffer-dir ;; try until we hit the current directory
        (let* ((next-dir (append current (list (car buffer-dir))))
               (file-file (concat (dir-list-to-path next-dir) "/" init-file)))
          (if (file-exists-p file-file)
              (dir-list-to-path current)
            (try-find-best-root nil (cdr buffer-dir) next-dir))))

       (t nil)))

    (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
           (vc-root-dir (vc-find-root buffer-dir repo-file)))
      (if (and init-file vc-root-dir)
          (try-find-best-root
           (make-dir-list (expand-file-name vc-root-dir))
           (make-dir-list buffer-dir)
           '())
        vc-root-dir))
    ) ;; default to vc root if init file not given

  ;; Set this variable to find project root
  (defvar jedi-config:find-root-function 'get-project-root-with-file)

  (defun current-buffer-project-root ()
    (funcall jedi-config:find-root-function
             (current-buffer)
             jedi-config:vcs-root-sentinel
             jedi-config:python-module-sentinel))

  (defun jedi-config:setup-server-args ()
    ;; little helper macro for building the arglist
    (defmacro add-args (arg-list arg-name arg-value)
      `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
    ;; and now define the args
    (let ((project-root (current-buffer-project-root)))

      (make-local-variable 'jedi:server-args)

      (when project-root
        (message (format "Adding system path: %s" project-root))
        (add-args jedi:server-args "--sys-path" project-root))

      (when jedi-config:with-virtualenv
        (message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
        (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

  ;; Use system python
  (defun jedi-config:set-python-executable ()
    (set-exec-path-from-shell-PATH)
    (make-local-variable 'jedi:server-command)
    (set 'jedi:server-command
         (list (executable-find "python"))))

  ;; Now hook everything up
  ;; Hook up to autocomplete
  (add-to-list 'ac-sources 'ac-source-jedi-direct)

  ;; Enable Jedi setup on mode start
  (add-hook 'python-mode-hook 'jedi:setup)

  ;; Buffer-specific server options
  (add-hook 'python-mode-hook
            'jedi-config:setup-server-args)
  (when jedi-config:use-system-python
    (add-hook 'python-mode-hook
              'jedi-config:set-python-executable))

  ;; And custom keybindings
  (defun jedi-config:setup-keys ()
    (local-set-key (kbd "M-.") 'jedi:goto-definition)
    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
    (local-set-key (kbd "M-?") 'jedi:show-doc)
    (local-set-key (kbd "M-/") 'jedi:get-in-function-call))

  ;; Don't let tooltip show up automatically
  (setq jedi:get-in-function-call-delay 10000000)
  ;; Start completion at method dot
  (setq jedi:complete-on-dot t)
  ;; Use custom keybinds
  (add-hook 'python-mode-hook 'jedi-config:setup-keys)
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  (add-hook 'python-mode-hook 'jedi:setup))

;; php-mode
(use-package company-php
  :ensure t
  )
