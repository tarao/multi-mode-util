(require 'multi-mode-util)
(require 'evil)

(defcustom multi-mode-util-preserved-evil-states
  '(normal insert emacs)
  "States of evil-mode preserved among multiple major modes."
  :type '(symbol)
  :group 'multi-mode-util)

(defun multi-evil-state-name (state)
  (if (symbolp state) (symbol-name state) state))

(defun multi-evil-state-fun-name (state)
  (concat "evil-" (multi-evil-state-name state) "-state"))

(defun multi-evil-state-fun (state)
  (intern (multi-evil-state-fun-name state)))

(defun multi-evil-state-hook (state)
  (intern (concat (multi-evil-state-fun-name state) "-entry-hook")))

(defun multi-evil-state-hook-fun (state)
  (intern (concat "multi-evil-" (multi-evil-state-name state) "-state")))

(dolist (state multi-mode-util-preserved-evil-states)
  (let ((state-fun (multi-evil-state-fun state))
        (hook (multi-evil-state-hook state))
        (hook-fun (multi-evil-state-hook-fun state)))
    (eval `(defun ,hook-fun ()
             (when multi-mode-alist
               (multi-with-every-buffer
                (remove-hook ',hook ',hook-fun t)
                (,state-fun 1)
                (add-hook ',hook ',hook-fun nil t)))))))

(defun multi-evil-install-hook ()
  (when (multi-indirect-buffer-p)
    (let ((state (multi-with-base-buffer evil-state))
          (evil-move-cursor-back nil))
      (evil-initialize-state)
      (funcall (multi-evil-state-fun state) 1)))
  (dolist (state multi-mode-util-preserved-evil-states)
    (let ((hook (multi-evil-state-hook state))
          (fun  (multi-evil-state-hook-fun state)))
    (add-hook hook fun nil t))))

(defadvice multi-mode-util-init (after ad-multi-mode-util-evil-init activate)
  (when evil-mode
    (add-hook 'multi-indirect-buffer-hook 'multi-evil-install-hook nil t)))

(provide 'multi-mode+evil)
;;; multi-mode+evil.el ends here
