(require 'multi-mode-util-common)
(require 'viper)

(defcustom multi-mode-util-preserved-viper-states
  '(vi-state insert-state emacs-state)
  "States of viper-mode preserved among multiple major modes."
  :type '(symbol)
  :group 'multi-mode-util)

(defun multi-viper-state-name (state)
  (let ((state (if (symbolp state) (symbol-name state) state)))
    (car (split-string state "-"))))

(defun multi-viper-state-fun-name (state)
  (concat "viper-change-state-to-" (multi-viper-state-name state)))

(defun multi-viper-state-fun (state)
  (intern (multi-viper-state-fun-name state)))

(defun multi-viper-state-hook (state)
  (intern (concat "viper-" (multi-viper-state-name state) "-state-hook")))

(defun multi-viper-state-hook-fun (state)
  (intern (concat "multi-viper-" (multi-viper-state-name state) "-state")))

(dolist (state multi-mode-util-preserved-viper-states)
  (let ((state-fun (multi-viper-state-fun state))
        (hook (multi-viper-state-hook state))
        (hook-fun (multi-viper-state-hook-fun state)))
    (eval `(defun ,hook-fun ()
             (when multi-mode-alist
               (multi-with-every-buffer
                (remove-hook ',hook ',hook-fun t)
                (,state-fun)
                (add-hook ',hook ',hook-fun nil t)))))))

(defun multi-viper-install-hook ()
  (when (multi-indirect-buffer-p)
    (let ((state (multi-with-base-buffer viper-current-state)))
      (funcall (multi-viper-state-fun state))))
  (dolist (state multi-mode-util-preserved-viper-states)
    (let ((hook (multi-viper-state-hook state))
          (fun  (multi-viper-state-hook-fun state)))
    (add-hook hook fun nil t))))

(defadvice multi-mode-util-init (after ad-multi-mode-util-viper-init activate)
  (when viper-mode
    (add-hook 'multi-indirect-buffer-hook 'multi-viper-install-hook nil t)))

(provide 'multi-mode+viper)
;;; multi-mode+viper.el ends here
