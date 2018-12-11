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

;;; Packages
;; helm
(use-package helm
  :ensure t
  :init
  (require 'helm)
  (require 'helm-config)
  (setq helm-mode-line-string "")
  (setq helm-split-window-inside-p t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 25)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-autoresize-mode t)
  (helm-mode t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-buffers-list)
   ("C-c m" . helm-imenu)
   ("M-y" . helm-show-kill-ring)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))
;; helm-projectile
(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (helm-projectile-on))
;; helm-swoop
(use-package helm-swoop
  :ensure t
  :bind ("M-s w" . helm-swoop))
;; helm-gtags
(use-package helm-gtags
  :ensure t
  :init
  (setq helm-gtags-auto-update t)
  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'java-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'php-mode-hook 'helm-gtags-mode)
  (eval-after-load "helm-gtags"
    '(progn
       (define-key helm-gtags-mode-map (kbd "C-x t f") 'helm-gtags-find-tag)
       (define-key helm-gtags-mode-map (kbd "C-x t r") 'helm-gtags-find-rtag)
       (define-key helm-gtags-mode-map (kbd "C-x t s") 'helm-gtags-find-symbol)
       (define-key helm-gtags-mode-map (kbd "C-x t g") 'helm-gtags-parse-file)
       (define-key helm-gtags-mode-map (kbd "C-x t p") 'helm-gtags-previous-history)
       (define-key helm-gtags-mode-map (kbd "C-x t n") 'helm-gtags-next-history)
       (define-key helm-gtags-mode-map (kbd "C-x t t") 'helm-gtags-pop-stack))
    ))

;; crux
(use-package crux
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
  ("C-c f" . crux-recentf-find-file)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c r" . crux-rename-buffer-and-file)
  ("C-c t" . crux-visit-term-buffer)
  ("C-h RET" . crux-find-user-init-file)
  ("C-x 7" . crux-swap-windows))

;; vlf - view large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;; move-text
(use-package move-text
  :ensure t
  :bind
  ("M-g <up>" . move-text-up)
  ("M-g <down>" . move-text-down))

;; which-key
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; magit
(use-package magit
  :ensure t
  :config
  (define-key magit-file-mode-map (kbd "C-x g") nil)
  :bind
  ("C-x g v" . magit-status)
  ("C-x g d" . magit-diff-buffer-file-popup)
  ("C-x g l" . magit-log-buffer-file-popup)
  ("C-x g a" . magit-log-all)
  ("C-x g b" . magit-blame-popup)
  ("C-x g c" . magit-commit-popup))

;; git-gutter
(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode t)
  :bind
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g s" . git-gutter:stage-hunk)
  ("C-x g r" . git-gutter:revert-hunk))

;; switch-window
(use-package ace-window
  :ensure t
  :init (global-set-key (kbd "C-x o") 'ace-window))
;; layout
(use-package perspective
  :ensure t
  :init
  (setq persp-mode-prefix-key (kbd "C-z"))
  (setq persp-initial-frame-name "0")
  :config
  (persp-mode t))

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
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))
;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :init (global-highlight-parentheses-mode t))
;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :init (volatile-highlights-mode t))

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :hook
  ((sh-mode emacs-lisp-mode python-mode perl-mode php-mode
            makefile-mode c-mode go-mode java-mode c++-mode
            org-mode markdown-mode terraform-mode)
   . yas-minor-mode))
;; company
(use-package company
  :ensure t
  :init (global-company-mode t)
  :config
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend)) backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :bind ("M-]" . company-complete))

;; undo-tree
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode t))

;; themes
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t))
;; modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-major-mode-icon nil)
  :hook (after-init . doom-modeline-init))

;;; Options
;; helm-ag
(use-package helm-ag
  :bind ("M-s d" . helm-ag))
;; ace-jump-mode
(use-package ace-jump-mode
  :bind ("M-s a" . ace-jump-mode))
;; which keybindings in my major?
(use-package discover-my-major
  :bind ("C-h M" . discover-my-major))
;; google-translate
(use-package google-translate
  :init
  (setq google-translate-translation-directions-alist '(("en" . "vi")))
  :bind ("M-s t" . google-translate-smooth-translate))
(defun develop-utils()
  "Utility packages ."
  (interactive)
  (package-install 'helm-ag)
  (package-install 'ace-jump-mode)
  (package-install 'discover-my-major)
  (package-install 'markdown-mode)
  (package-install 'interaction-log))

;;: Hook
;; hide the minor modes
(defvar hidden-minor-modes
  '(global-whitespace-mode flycheck-mode which-key-mode projectile-mode git-gutter-mode helm-mode undo-tree-mode company-mode highlight-parentheses-mode smartparens-mode volatile-highlights-mode))
(defun purge-minor-modes ()
  "Dont show on modeline."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

;;; Customize
;; defun
(defun indent-buffer ()
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
          (message (format "Yanked %s" filename)))))
(defun untabify-buffer ()
  "Convert all tabs in buffer to multiple spaces."
  (interactive)
  (save-excursion (untabify (point-min) (point-max) nil)))
(defun tabify-buffer ()
  "Convert 4 spaces in buffer to tab."
  (interactive)
  (save-excursion (tabify (point-min) (point-max) nil)))
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
(defun share-buffer-online (downloads)
  "Share buffer to online.
- DOWNLOADS: The max-downloads"
  (interactive "p")
  (let ((filename (if (equal major-mode 'dired-mode) default-directory
                    (buffer-file-name))))
    (when filename (async-shell-command
                    (format "curl --progress-bar -H 'Max-Downloads: %d' --upload-file %s https://transfer.sh"
                            downloads filename)))))
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
(defun show-lossage ()
  "Show logssage."
  (interactive)
  (if (locate-library "interaction-log")
      (progn
        (load-library "interaction-log")
        (call-interactively 'interaction-log-mode))
    (view-lossage)))
(defun sudo-save ()
  "Save buffer with sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo::" (helm-read-file-name "sudo-save to:")))
    (write-file (concat "/sudo::" buffer-file-name))))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-x j") 'mode-line-other-buffer)
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "M-s e") 'eww)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-_") 'dabbrev-completion)
(global-set-key (kbd "C-x x .") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x x ;") 'indent-buffer)
(global-set-key (kbd "C-x x b") 'rename-buffer)
(global-set-key (kbd "C-x x g") 'org-agenda)
(global-set-key (kbd "C-x x p") 'yank-file-path)
(global-set-key (kbd "C-x x r") 'revert-buffer)
(global-set-key (kbd "C-x x s") 'share-buffer-online)
(global-set-key (kbd "C-x x t") 'untabify-buffer)
(global-set-key (kbd "C-x x T") 'tabify-buffer)
(global-set-key (kbd "C-x x M-w") 'copy-to-clipboard)
(global-set-key (kbd "C-x x C-y") 'paste-from-clipboard)
(global-set-key (kbd "C-x x C-s") 'sudo-save)
(global-set-key (kbd "C-x 2") 'split-window-vertically-last-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-last-buffer)
(global-set-key (kbd "C-x 4 C-v") 'scroll-other-window)
(global-set-key (kbd "C-x 4 M-v") 'scroll-other-window-down)
(global-set-key (kbd "C-h l") 'show-lossage)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line nil)
 '(auto-revert-check-vc-info t)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(browse-url-browser-function (quote eww-browse-url))
 '(column-number-mode t)
 '(default-input-method "vietnamese-telex")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(enable-local-variables :all)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/.gxt/org")))
 '(org-enforce-todo-dependencies t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WAITING(w)" "|" "CANCELED(c)"))))
 '(read-quoted-char-radix 16)
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/backup" nil (tramp))
 '(version-control t)
 '(whitespace-style (quote (tabs empty indentation big-indent tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; .emacs
(defun develop-dot()
  "Update 'user-init-file - .emacs."
  (interactive)
  (let (upstream)
    (setq upstream (make-temp-file ".emacs"))
    (message upstream)
    (url-copy-file "https://raw.githubusercontent.com/TxGVNN/dots/master/.emacs" upstream t)
    (diff user-init-file upstream)
    (other-window 1 nil)
    (message "Override %s by %s to update" user-init-file upstream)))

;; c-mode
(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;; go-mode
(defun develop-go()
  "Go develoment.
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
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (add-to-list 'company-backends '(company-go :with company-yasnippet)))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

;; python-mode
(defun develop-python()
  "Python development."
  (interactive)
  (package-install 'python-mode)
  (package-install 'company-jedi))
(use-package python-mode
  :defer t
  :init
  (setq jedi:complete-on-dot t)
  ;; Buffer-specific server options
  (defun jedi-config:setup-server-args ()
    (defmacro add-args (arg-list arg-name arg-value)
      `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
    (let ((project-root (projectile-project-root)))
      (make-local-variable 'jedi:server-args)
      (when project-root
        (message (format "Adding system path: %s" project-root))
        (add-args jedi:server-args "--sys-path" project-root))))
  ;; And custom keybindings
  (defun jedi-config:setup-keys ()
    (local-set-key (kbd "M-.") 'jedi:goto-definition)
    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
    (local-set-key (kbd "M-?") 'jedi:show-doc))
  ;; Update python environment
  (defun py-venv-update()
    (defvar venv-executables-dir "bin")
    (setq venv-current-dir (file-name-as-directory (python-environment-root-path)))
    ;; setup the python shell
    (setq python-shell-virtualenv-path venv-current-dir)
    ;; setup emacs exec-path
    (add-to-list 'exec-path (concat venv-current-dir venv-executables-dir))
    ;; setup the environment for subprocesses
    (let ((path (concat venv-current-dir
                        venv-executables-dir
                        path-separator
                        (getenv "PATH"))))
      (setenv "PATH" path)
      ;; keep eshell path in sync
      (setq eshell-path-env path))
    (setenv "VIRTUAL_ENV" venv-current-dir))
  ;; Hooks
  (if (package-installed-p 'company-jedi)
      (progn
        (add-hook 'python-mode-hook 'jedi-config:setup-server-args)
        (add-hook 'python-mode-hook 'py-venv-update)
        (add-hook 'python-mode-hook 'jedi:setup)
        (add-hook 'python-mode-hook 'jedi-config:setup-keys)
        (add-hook 'python-mode-hook '(lambda ()
                                       (add-to-list 'company-backends
                                                    '(company-jedi :with company-yasnippet))))))
  )

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
                (add-to-list 'company-backends '(company-ac-php-backend :with company-yasnippet)))))

;; terraform-mode
(defun develop-terraform()
  "Terraform development."
  (interactive)
  (package-install 'company-terraform))
(use-package company-terraform
  :defer t
  :init
  (add-hook 'terraform-mode-hook 'company-terraform-init))

;; ansible-mode
(defun develop-ansible ()
  "Ansible development."
  (interactive)
  (package-install 'ansible)
  (package-install 'company-ansible))
(use-package ansible
  :defer t
  :init
  (add-hook 'ansible-hook '
            (lambda ()
              (add-to-list 'company-backends '(company-ansible :with company-yasnippet))
              (yas-minor-mode-on))))
;; java-mode
(defun develop-java()
  "Java develoment.
Please install:
https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
tar -vxf jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/"
  (interactive)
  (package-install 'lsp-java)
  (package-install 'company-lsp))
(use-package lsp-java
  :defer t
  :init
  (add-hook 'java-mode-hook '
            (lambda () (require 'lsp-java) (lsp)
              (add-to-list 'company-backends '(company-lsp :with company-yasnippet)))))

(provide '.emacs)
;;; .emacs ends here
