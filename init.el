(when (< emacs-major-version 27)
  (setq gc-cons-threshold 100000000))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package diminish
  :straight t
  :defer t)
(require 'bind-key)

(use-package org
  :straight (:type built-in))

(use-package use-package-ensure-system-package
  :straight t)

(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)
