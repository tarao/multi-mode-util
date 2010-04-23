(require 'multi-mode)

(defun multi-make-chunk-finder (start-pat end-pat mode)
  `(lambda (pos)
     (let ((start (point-min)) (end (point-max)))
       (save-excursion
         (save-restriction
           (widen)
           (goto-char pos)
           (when (re-search-backward ,end-pat nil t) (setq start (point)))
           (let (s e)
             (goto-char start)
             (when (re-search-forward ,start-pat pos t) (setq s (1+ (point))))
             (goto-char (or s start))
             (when (re-search-forward ,end-pat nil t)
               (re-search-backward ,end-pat nil t)
               (setq e (1- (point))))
             (if (and s e (<= s e) (<= s pos) (<= pos e))
                 (multi-make-list ',mode s e)
               nil)))))))
(defun multi-mode-init (&optional base-mode)
 (interactive)
  (unless (and multi-mode-alist (local-variable-p 'multi-mode-alist))
    (set (make-local-variable 'multi-mode-alist)
         `((,(or base-mode major-mode) . nil))))
  (set (make-variable-buffer-local 'inhibit-eval-during-redisplay) t)
  (add-hook
   'multi-indirect-buffer-hook
   '(lambda ()
      (set (make-variable-buffer-local 'inhibit-eval-during-redisplay) t)))
  (multi-mode-install-modes)
  (multi-viper-init))
(defun multi-install-chunk-finder (start end mode)
  (unless (assoc mode multi-mode-alist)
    (let ((finder (multi-make-chunk-finder start end mode)))
      (setq multi-mode-alist (append multi-mode-alist `((,mode . ,finder))))
      (multi-install-mode mode finder))))

;; Workaround for fontification
(defun multi-fontify-current-chunk ()
  (interactive)
  (when (buffer-base-buffer)
    (let ((val (multi-find-mode-at)))
      (funcall font-lock-fontify-region-function
               (nth 1 val) (nth 2 val) nil))))
(add-hook 'multi-select-mode-hook 'multi-fontify-current-chunk)

;; Workaround to prevent inconsistency in viper states
(defgroup multi-mode-util nil "Customization for multi-mode-util."
  :prefix "multi-mode-util-")
(defcustom multi-mode-util-preserved-viper-states
  '(vi-state insert-state emacs-state)
  "States of viper-mode preserved among multiple major modes."
  :type '(symbol)
  :group 'multi-mode-util)
(defvar multi-preserving-viper-states nil)
(defun multi-viper-change-base-buffer-state (state)
  (with-current-buffer (multi-base-buffer) (viper-change-state state)))
(defun multi-viper-change-indirect-buffers-state (state)
  (dolist (elt multi-indirect-buffers-alist)
    (with-current-buffer (cdr elt) (viper-change-state state))))
(defun multi-viper-hook-name (state)
  (when (symbolp state) (setq state (symbol-name state)))
  (intern (concat "viper-" state "-hook")))
(defun multi-viper-init ()
  (when (and (boundp 'viper-mode) viper-mode)
    (dolist (st multi-mode-util-preserved-viper-states)
      (let ((h (multi-viper-hook-name st)))
        (add-hook
         h
         `(lambda ()
            (when multi-mode-alist
              (multi-viper-change-indirect-buffers-state ',st)
              (unless (buffer-base-buffer)
                (multi-viper-change-base-buffer-state ',st)))))))))

(provide 'multi-mode-util)
;;; multi-mode-util.el ends here
