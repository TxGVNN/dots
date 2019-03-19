;;; GNUS settings
;; (STATELESS BEGIN)
(use-package smtpmail-multi
  :ensure t
  :config
  (setq send-mail-function (quote smtpmail-multi-send-it))
  (setq message-send-mail-function (quote smtpmail-multi-send-it)))
(use-package ebdb
  :ensure t
  :config
  (setq ebdb-mua-pop-up nil)
  (require 'ebdb-gnus))
(use-package company-ebdb
  :ensure t
  :config (add-to-list 'company-backends 'company-ebdb))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-large-newsgroup 50)
(setq gnus-select-method '(nnml ""))
(setq gnus-gcc-mark-as-read t)

(defvar topic-topology nil)
(defvar topic-alist nil)
(defvar email-addresses nil)
;; (STATELESS END)

;;; Default setting
(setq user-mail-address "txgvnn@gmail.com"
      user-full-name "Giap Tran")

;;; EMAIL 1
(add-to-list 'email-addresses "Giap Tran <txgvnn@gmail.com>")
;; Incomming
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "Gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo")))
(add-to-list 'gnus-parameters
             '("nnimap.Gmail:.*"
               (gcc-self . "nnimap+Gmail:Sent")
               (display . all)
               (posting-style
                (name "Giap Tran")
                (address "txgvnn@gmail.com")
                (signature-file "~/.mutt/signature.gmail"))
               (expiry-target . delete)))
;; Outcomming
(add-to-list 'smtpmail-multi-accounts
             '(gmail . ("txgvnn@gmail.com"
                        "smtp.gmail.com"
                        587
                        "txgvnn@gmail.com"
                        starttls nil nil nil)))
(add-to-list 'smtpmail-multi-associations '("txgvnn@gmail.com" gmail))

;; Topic
(add-to-list 'topic-topology '(("Gmail" visible nil nil)))
(add-to-list 'topic-alist '("Gmail" ; the key of topic
                            "nnimap+Gmail:INBOX"
                            "nnimap+Gmail:Drafts"
                            "nnimap+Gmail:Sent"))

;;; EMAIL 2
(add-to-list 'email-addresses "Giap TRAN <user@example.com>")
;; Incomming
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "Example"
                      (nnimap-address "imap.example.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo")))
(add-to-list 'gnus-parameters
             '("nnimap.Example:.*"
               (gcc-self . "nnimap+Example:Sent")
               (display . all)
               (posting-style
                (name "Giap TRAN")
                (address "user@example.com")
                (organization "EXAMPLE VIETNAM")
                (signature-file "~/.mutt/signature.example"))
               (expiry-target . delete)))
;; Outcomming
(add-to-list 'smtpmail-multi-accounts
             '(example . ("user@example.com"
                          "smtp.example.com"
                          465
                          "user@example.com"
                          ssl nil nil nil)))
(add-to-list 'smtpmail-multi-associations '("user@example.com" example))

;; Topic
(add-to-list 'topic-topology '(("Example" visible nil nil)))
(add-to-list 'topic-alist '("Example" ; the key of topic
                            "nnimap+Example:INBOX"
                            "nnimap+Example:Drafts"
                            "nnimap+Example:Sent"))

;;; Footer
;; (STATELESS BEGIN)
(add-to-list 'topic-topology '("Gnus" visible))
(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-topic-topology topic-topology)
     (setq gnus-topic-alist topic-alist)))

(setq message-alternative-emails
      (regexp-opt email-addresses))
;;; setup message-dont-reply-to-names
(mapc
 (lambda(arg)
   (when (string-match "\\(.+?\\)\<\\(.+?\\)\>" arg)
     (add-to-list 'message-dont-reply-to-names (match-string 2 arg)))
   ) email-addresses)

;; Gnus from manipulation
(setq gnus-from-selected-index 0)
(defun gnus-loop-from ()
  (interactive)
  (setq gnus-article-current-point (point))
  (goto-char (point-min))
  (if (eq gnus-from-selected-index (length email-addresses))
      (setq gnus-from-selected-index 0) nil)
  (while (re-search-forward "^From:.*$" nil t)
    (replace-match (concat "From: " (nth gnus-from-selected-index email-addresses))))
  (goto-char gnus-article-current-point)
  (setq gnus-from-selected-index (+ gnus-from-selected-index 1)))

(define-key message-mode-map (kbd "C-c f") 'gnus-loop-from)
;; (STATELESS END)
