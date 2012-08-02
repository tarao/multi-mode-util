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
     (with-current-buffer (cdr elt) ,@body)))

(defmacro multi-with-every-buffer (&rest body)
  "Evaluate BODY in all buffers."
  (declare (indent 0))
  `(progn
     (multi-with-base-buffer ,@body)
     (multi-with-indirect-buffers ,@body)))

(provide 'multi-mode-util-common)
;;; multi-mode-util-common.el ends here
