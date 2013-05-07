;;; multi-mode+evil.el --- Evil patch for multi-mode

;; Copyright (C) 2010-2013 Lintaro INA

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; Version: 0.0.2

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

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
    (eval
     `(defun ,hook-fun ()
        (let ((buffer (current-buffer)))
          (when multi-mode-alist
            (multi-with-every-buffer
              (unless (eq buffer (current-buffer))
                (remove-hook ',hook ',hook-fun t)
                (,state-fun 1)
                (add-hook ',hook ',hook-fun nil t)))))))))

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
