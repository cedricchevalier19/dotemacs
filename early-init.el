(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(provide 'early-init)
;;; early-init.el ends here
