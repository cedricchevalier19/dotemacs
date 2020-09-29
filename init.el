(when (< emacs-major-version 27)
  (setq gc-cons-threshold 100000000))

;;; Code:
    (let ((straight-treat-as-init t))
      (when (locate-library "gnutls")
        (require 'gnutls)	
;;; straight
;;;; Variables
      (setq straight-repository-branch "develop"
            straight-profiles '((dotemacs . "versions.el")
                                (nil . "default.el"))
            straight-current-profile 'dotemacs)
      ;; Enable `straight-live-modifications-mode' if its dependencies are
      ;; found.
;;;; straight live modifications:
      (if (and (executable-find "watchexec")
               (executable-find "python3"))
          (setq straight-check-for-modifications
                '(watch-files find-when-checking))
        (setq straight-check-for-modifications
              '(check-on-save find-when-checking)))
;;;; Bootstrap straight.el:
      (let ((bootstrap-file
             (expand-file-name
              "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5)
            (domain "https://raw.githubusercontent.com")
            (repo "raxod502/straight.el")
            (branch straight-repository-branch)
            (remote-file "install.el"))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               (mapconcat #'identity (list domain repo branch remote-file) "/")
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))
;;;; use package
      ;; Enable the `:bind-key' keyword
      (straight-use-package 'bind-key)
      ;; Now clone the `use-package' library
      (straight-use-package 'use-package)
      ;; Enable the `:ensure-system-package' keyword
      (straight-use-package 'use-package-ensure-system-package)
      ;; Use `blackout' to clean mode lighters, essentially a drop in
      ;; replacement for ':diminish'
      (straight-use-package
       '(blackout :host github :repo "raxod502/blackout"))
      (require 'blackout)
      ;; lazy load by default
      (setq use-package-always-defer t)
      ;; Enable the newer version of `use-package'.
      (setq straight-use-package-version 'straight
            straight-use-package-by-default t)
      ;; reduce the clutter in `user-emacs-directory'
;;;; no littering
      (use-package no-littering
        :demand t
        :commands (no-littering-expand-etc-file-name)
        :custom
        (no-littering-etc-directory (expand-file-name "etc" user-emacs-directory))
        (no-littering-var-directory (expand-file-name "var" user-emacs-directory))
        :init
        (require 'recentf)
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
      ;; `el-patch' is like advice, but with state awareness and validation.
      (straight-use-package 'el-patch)
      (require 'subr-x)
      (straight-use-package 'git)

;;; Org
;;;;; install org mode:
      (straight-use-package 'org-plus-contrib)
;;;;; Org configuration
      (use-package org
        :straight org-plus-contrib
;;;;;; customizations
        :custom
;;;;;;; Files
        (org-directory (file-truename "~/Dropbox/org"))
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
                       org-velocity))
;;;;;; org keybindings
        :bind
        (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         (:map org-mode-map
               ("C-c C-x h" . org-toggle-link-display)
               ("C-c C-s" . org-schedule))))))

 (use-package diminish
  :defer t)

 (require 'bind-key)

(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
