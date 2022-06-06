;;; Initialize emacs for declarative configuration
(when (< emacs-major-version 27)
  (setq gc-cons-threshold 100000000))

;;; Code:
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;;;; no littering
      (use-package no-littering
        :demand t
        :commands (no-littering-expand-etc-file-name)
        :init
        (require 'recentf)

        (setq no-littering-etc-directory (expand-file-name "etc" user-emacs-directory))
        (setq no-littering-var-directory (expand-file-name "var" user-emacs-directory))
        ;; exclude from recentf
        (add-to-list 'recentf-exclude no-littering-var-directory)
        (add-to-list 'recentf-exclude no-littering-etc-directory)
        ;; store auto save files in the var directory.
        :config
        (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

      (customize-set-variable 'load-prefer-newer t) ;; load newer bytecode
;;;; auto compile
      (use-package auto-compile
        :demand t
        :init
        (auto-compile-on-load-mode))

;;; Org
;;;;; Org configuration
      (use-package org
;;;;;; customizations
        :custom
;;;;;;; Files
        (org-directory (file-truename "~/.org/"))
        ;; setup archive directory in current folder
        (org-archive-location "archive/%s_archive::")
;;;;;;; Org source
	      (org-confirm-babel-evaluate nil)
        (org-src-fontify-natively t)
        (org-src-preserve-indentation t)
        (org-src-persistent-message nil)
        (org-src-window-setup 'current-window)
        (org-ctrl-k-protect-subtree 'error)
        (org-startup-indented t)
        (org-catch-invisible-edits 'smart)
;;;;;;; Structure and Appearance
        (org-display-remote-inline-images 'cache)
        (org-insert-heading-respect-content t)
        (org-ellipsis "")
        (org-list-allow-alphabetical t)
        (org-hide-emphasis-markers t)
        (org-hidden-keywords '(author title date))
        (org-pretty-entities t)
        (org-use-sub-superscripts '{})
        (org-use-speed-commands t)
        (org-yank-folded-subtrees t)
        (org-yank-adjusted-subtrees t)
        (org-blank-before-new-entry
         '((heading . auto)
           (plain-list-item . auto)))
;;;;;;; org libraries
        (org-modules '(ol-bookmark
                       ol-man
                       ol-elisp-symbol
                       org-bbdb
                       org-bibtex
                       org-crypt
                       org-eww
                       org-habit
                       org-id
                       org-info
                       org-inlinetask
                       org-protocol
                       org-tempo
                       org-eshell
                       org-annotate-file
                       org-checklist
                       org-collector
                       org-mac-iCal
                       org-mac-link
                       org-velocity
                       org-latex
                       org-beamer))
;;;;;; org keybindings
        :bind
        (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         (:map org-mode-map
               ("C-c C-x h" . org-toggle-link-display)
               ("C-c C-s" . org-schedule))))
 (use-package diminish
  :defer t)

 (require 'bind-key)

(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))
;; (org-babel-load-file
;;  (expand-file-name "email.org"
;;                    user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
