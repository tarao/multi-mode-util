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

;; Workaround for transient-mark-mode
(defadvice multi-select-buffer
  (around multi-disable-select-buffer-when-mark-active activate)
  (unless (and (boundp 'mark-active) mark-active) ad-do-it))

;; Workaround for undo/redo
;; This is not multi-mode specific problem;
;; undo/redo in indirect buffers seem to have the same problem
(defun multi-defadvice-in-base-buffer (func)
  (eval `(defadvice ,func
           (around ,(intern (concat (symbol-name func) "-in-base-buffer"))
                   activate)
           (let (pos)
             (with-current-buffer (or (buffer-base-buffer) (current-buffer))
               ad-do-it
               (setq pos (point)))
             (goto-char pos)))))
(multi-defadvice-in-base-buffer 'undo)
(multi-defadvice-in-base-buffer 'redo)
(multi-defadvice-in-base-buffer 'undo-tree-undo)
(multi-defadvice-in-base-buffer 'undo-tree-redo)
(defadvice undo-tree-visualize
  (around undo-tree-visualize-in-base-buffer activate)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer)) ad-do-it))

;; Workaround to prevent inconsistency in viper states
(defgroup multi-mode-util nil "Customization for multi-mode-util."
  :prefix "multi-mode-util-")
(defcustom multi-mode-util-preserved-viper-states
  '(vi-state insert-state emacs-state)
  "States of viper-mode preserved among multiple major modes."
  :type '(symbol)
  :group 'multi-mode-util)
(defvar multi-preserving-viper-states nil)
(defvar multi-viper-hook-alist nil)
(defun multi-viper-hook-name (state)
  (when (symbolp state) (setq state (symbol-name state)))
  (concat "viper-" state "-hook"))
(defun multi-viper-state-name (state)
  (when (symbolp state) (setq state (symbol-name state)))
  (car (split-string state "-")))
(defun multi-viper-change-state (state)
  (let* ((hook (intern (multi-viper-hook-name state)))
         (func (cdr (assoc hook multi-viper-hook-alist)))
         (st (multi-viper-state-name state)))
    (remove-hook hook func)
    (funcall (intern (concat "viper-change-state-to-" st)))
    (add-hook hook func)))
(defun multi-viper-change-base-buffer-state (state)
  (with-current-buffer (multi-base-buffer) (multi-viper-change-state state)))
(defun multi-viper-change-indirect-buffers-state (state)
  (dolist (elt multi-indirect-buffers-alist)
    (with-current-buffer (cdr elt) (multi-viper-change-state state))))
(defun multi-viper-init ()
  (when (and (boundp 'viper-mode) viper-mode)
    (dolist (st multi-mode-util-preserved-viper-states)
      (let ((hook (intern (multi-viper-hook-name st)))
            (func
             `(lambda ()
                (when multi-mode-alist
                  (multi-viper-change-indirect-buffers-state ',st)
                  (unless (buffer-base-buffer)
                    (multi-viper-change-base-buffer-state ',st))))))
        (unless (assoc hook multi-viper-hook-alist)
          (push (cons hook func) multi-viper-hook-alist))
        (add-hook hook func)))))

(provide 'multi-mode-util)
;;; multi-mode-util.el ends here
