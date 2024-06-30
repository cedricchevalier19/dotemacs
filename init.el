;;;; Initialize emacs for declarative configuration
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
  (require 'use-package)
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


(use-package diminish
  :defer t)

(require 'bind-key)

(defvar cc/roam-dir "~/org/roam")
(defvar cc/bibfiles '("~/org/roam/zotero.bib"))


;; Don't attempt to find/apply special file handlers to files loaded during
;; startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "readme.elc" user-emacs-directory))
      (load-file (expand-file-name "readme.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "readme.org" user-emacs-directory))))

(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
