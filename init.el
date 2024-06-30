;;; package -- Summary
;;; Commentary:
;;; init Emacs --  Initialize Emacs for declarative configuration

;;; Code:

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
