(require 'multi-mode)

(defgroup multi-mode-util nil "Customization for multi-mode-util."
  :group 'convenience
  :prefix "multi-mode-util-")

(defcustom multi-mode-util-inhibit-eval-during-redisplay nil
  "Whether to set `inhibit-eval-during-redisplay' to t in the buffer.
This suppresses `Error during redisplay: (args-out-of-rage ...)'
message but `jit-lock-mode' won't work properly."
  :type 'boolean
  :group 'multi-mode-util)

(defsubst multi-base-buffer-p () (null (buffer-base-buffer)))
(defsubst multi-indirect-buffer-p () (not (null (buffer-base-buffer))))
(defsubst multi-initialized-p ()
  (and (boundp 'multi-indirect-buffers-alist)
       (consp multi-indirect-buffers-alist)))

(defmacro multi-with-base-buffer (&rest body)
  "Evaluate BODY in the base buffer."
  (declare (indent 0))
  `(with-current-buffer (or (buffer-base-buffer) (current-buffer)) ,@body))

(defmacro multi-with-indirect-buffers (&rest body)
  "Evaluate BODY in the indirect buffers."
  (declare (indent 0))
  `(dolist (elt multi-indirect-buffers-alist)
     (with-current-buffer (cdr elt) (when (multi-indirect-buffer-p) ,@body))))

(defmacro multi-with-every-buffer (&rest body)
  "Evaluate BODY in all buffers."
  (declare (indent 0))
  `(progn
     (multi-with-base-buffer ,@body)
     (multi-with-indirect-buffers ,@body)))

(defun multi-mode-util-inhibit-eval-during-redisplay ()
  (set (make-local-variable 'inhibit-eval-during-redisplay) t))

(defun multi-mode-init (&optional base-mode)
  "Initialize multi-mode with BASE-MODE activated in the base buffer."
  (interactive)
  (multi-install-mode base-mode nil t))

(defun multi-mode-util-init (base-mode)
  (set (make-local-variable 'multi-mode-alist) (list (cons base-mode nil)))
  (when multi-mode-util-inhibit-eval-during-redisplay
    (multi-mode-util-inhibit-eval-during-redisplay)
    (add-hook 'multi-indirect-buffer-hook
              'multi-mode-util-inhibit-eval-during-redisplay)))

(defadvice multi-install-mode
  (around ad-multi-mode-util-install-mode
          (mode &optional chunk-fn base) activate)
  (unless (multi-initialized-p)
    (if base
        (multi-mode-util-init mode)
      (multi-install-mode major-mode nil t)))
  ad-do-it)

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
             (when (re-search-forward ,start-pat pos t) (setq s (point)))
             (goto-char (or s start))
             (when (re-search-forward ,end-pat nil t)
               (re-search-backward ,end-pat nil t)
               (setq e (1- (point))))
             (if (and s e (<= s e) (<= s pos) (<= pos e))
                 (multi-make-list ',mode s e)
               nil)))))))

(defun multi-install-chunk-finder (start end mode)
  (unless (assoc mode multi-mode-alist)
    (let ((finder (multi-make-chunk-finder start end mode)))
      (setq multi-mode-alist (append multi-mode-alist `((,mode . ,finder))))
      (multi-install-mode mode finder))))

(defun multi-fontify-current-chunk ()
  "Workaround for fontification: fontify current chunk.
This is used to ensure fontifying on a newly selected indirect
buffer."
  (interactive)
  (when (multi-indirect-buffer-p)
    (let* ((val (multi-find-mode-at))
           (beg (nth 1 val)) (end (nth 2 val)))
        (font-lock-fontify-region beg end))))
(add-hook 'multi-select-mode-hook 'multi-fontify-current-chunk)

(defadvice multi-map-over-chunks
  (around ad-multi-no-recursive-fontification activate)
  "Workaround for fontification: `multi-map-over-chunks'
internally select indirect buffers."
  (remove-hook 'multi-select-mode-hook 'multi-fontify-current-chunk)
  ad-do-it
  (add-hook 'multi-select-mode-hook 'multi-fontify-current-chunk))

(defadvice font-lock-unfontify-region
  (around ad-multi-narrowing-font-lock-unfontify-region (beg end) activate)
  "Workaround for fontification: `font-lock-unfontify-region' can
be called in an indirect buffer with the range larger than its
chunk."
  (if (multi-initialized-p)
    (let* ((val (multi-find-mode-at))
           (buf (cdr (assoc (car val) multi-indirect-buffers-alist))))
      (setq beg (max beg (nth 1 val)))
      (setq end (min end (nth 2 val)))
      (when (and (eq buf (current-buffer)) (< beg end)) ad-do-it))
    ad-do-it))

(defadvice multi-select-buffer
  (around ad-multi-disable-select-buffer-when-mark-active activate)
  "Workaround for transient-mark-mode."
  (unless (and (boundp 'mark-active) mark-active) ad-do-it))

(defun multi-run-in-base-buffer (func &optional track-position)
  "Make FUNC to be run in the base buffer."
  (let ((ad-sym (intern (concat "ad-" (symbol-name func) "-in-base-buffer")))
        (body (if track-position
                  '(let (pos)
                     (multi-with-base-buffer
                       ad-do-it
                       (setq pos (point)))
                     (goto-char pos))
                '(multi-with-base-buffer ad-do-it))))
    (eval `(defadvice ,func (around ,ad-sym activate)
             ,body))))

;; Workaround for Emacs bug: `restore-buffer-modified-p' function does
;; not respect indirect buffers.
(multi-run-in-base-buffer 'restore-buffer-modified-p)

;; Workaround for undo/redo. This is not multi-mode specific problem;
;; undo/redo in indirect buffers seem to have the same problem.
(multi-run-in-base-buffer 'undo t)
(multi-run-in-base-buffer 'redo t)
(multi-run-in-base-buffer 'undo-tree-undo t)
(multi-run-in-base-buffer 'undo-tree-redo t)
(multi-run-in-base-buffer 'undo-tree-visualize)

(defun multi-mode-quit ()
  "Quit multi-mode."
  (interactive)
  (multi-with-base-buffer
    (multi-with-indirect-buffers (let ((kill-buffer-hook nil)) (kill-buffer)))
    (setq multi-mode-alist (list (pop multi-mode-alist)))
    (setq multi-indirect-buffers-alist
          (list (cons major-mode (current-buffer))))))

;; Workaround for viper
(eval-after-load 'viper '(progn (require 'multi-mode+viper)))

;; Workaround for evil
(eval-after-load 'evil '(progn (require 'multi-mode+evil)))

(provide 'multi-mode-util)
;;; multi-mode-util.el ends here
