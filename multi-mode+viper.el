;;; multi-mode+viper.el --- Viper patch for multi-mode

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
