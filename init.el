;;; package -- Summary
;;; Commentary:
;;; init Emacs --  Initialize Emacs for declarative configuration

;;; Code:

;; Don't attempt to find/apply special file handlers to files loaded during
;; startup.


(defconst config-org (locate-user-emacs-file "readme.org"))
(defconst config-el (locate-user-emacs-file "config.el"))
(defconst config-email-el (locate-user-emacs-file "local/email.el"))

(unless (file-exists-p config-el)
  (require 'org)
  (org-babel-tangle-file config-org config-el "emacs-lisp"))

(load-file config-el)

(if (file-exists-p config-email-el)
    (load-file config-email-el))

(provide 'init)
;;; init.el ends here
