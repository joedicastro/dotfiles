;;; evil-leader.el --- let there be <leader>

;; Copyright (C) 2011-2013 by Michael Markert
;; Author: Michael Markert <markert.michael@googlemail.com>
;; URL: http://github.com/cofi/evil-leader
;; Git-Repository: git://github.com/cofi/evil-leader.git
;; Created: 2011-09-13
;; Version: 20130609.1840
;; X-Original-Version: 0.3.3
;; Keywords: evil vim-emulation leader
;; Package-Requires: ((evil "0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Known Bugs:
;; See http://github.com/cofi/evil-leader/issues

;; Install:
;; (require 'evil-leader)

;; Usage:
;;
;; (global-evil-leader-mode)
;;
;;    to enable `evil-leader' in every buffer where `evil' is enabled.
;;
;;    Note: You should enable `global-evil-leader-mode' before you enable
;;          `evil-mode', otherwise `evil-leader' won't be enabled in initial
;;          buffers (*scratch*, *Messages*, ...).
;;
;;    Use `evil-leader/set-key' to bind keys in the leader map.
;;    For example:
;;
;; (evil-leader/set-key "e" 'find-file)
;;
;;    You can also bind several keys at once:
;;
;; (evil-leader/set-key
;;   "e" 'find-file
;;   "b" 'switch-to-buffer
;;   "k" 'kill-buffer)
;;
;;    The key map can of course be filled in several places.
;;
;;    After you set up the key map you can access the bindings by pressing =<leader>=
;;    (default: \) and the key(s). E.g. \ e would call `find-file' to open a file.
;;
;;    If you wish to change so you can customize =evil-leader/leader= or call
;;    `evil-leader/set-leader', e.g. (evil-leader/set-leader ",") to change it to
;;    ",".
;;    The leader has to be readable by `read-kbd-macro', so using Space as a
;;    prefix key would be (evil-leader/set-leader "<Space>").
;;
;;    Beginning with version 0.3 evil-leader has support for mode-local bindings:
;;
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
;;
;;    Again, you can bind several keys at once.
;;
;;    A mode-local binding shadows a normal mode-independent binding.

;;; Code:

(require 'evil)

(defvar evil-leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(defvar evil-leader--mode-maps nil
  "Alist of mode-local leader bindings, shadows mode-independent bindings.")

;;; customization
(defgroup evil-leader nil
  "<leader> support for evil."
  :group 'evil
  :prefix 'evil-leader/)

(defcustom evil-leader/leader "\\"
  "The <leader> key, used to access keys defined by `evil-leader/set-key' in normal and visual state.
Must be readable by `read-kbd-macro'. For example: \",\"."
  :type "string"
  :group 'evil-leader)

(defcustom evil-leader/non-normal-prefix "C-"
  "Prefix for leader-map in insert- and emacs-state.
`evil-leader/in-all-states' has to be non-nil for this to be set.
The combination has to be readable by `read-kbd-macro'."
  :type 'string
  :group 'evil-leader)

(defcustom evil-leader/in-all-states nil
  "If is non-nil leader-map is accessible by <prefixed-leader> in emacs/insert state.

<prefixed-leader> is `evil-leader/non-normal-prefix' + `evil-leader/leader'"
  :type 'boolean
  :group 'evil-leader)

;;;###autoload
(define-minor-mode global-evil-leader-mode
  "Global minor mode for <leader> support."
  nil nil nil
  (if global-evil-leader-mode
      (add-hook 'evil-local-mode-hook #'evil-leader-mode)
    (remove-hook 'evil-local-mode-hook #'evil-leader-mode)))

;;;###autoload
(define-minor-mode evil-leader-mode
  "Minor mode to enable <leader> support."
  :init-value nil
  :keymap nil
  (let* ((prefixed (read-kbd-macro (concat evil-leader/non-normal-prefix evil-leader/leader)))
         (mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
         (map (or mode-map evil-leader--default-map)))
    (if evil-leader-mode
        (progn
          (define-key evil-motion-state-local-map (read-kbd-macro evil-leader/leader) map)
          (when evil-leader/in-all-states
            (define-key evil-emacs-state-local-map prefixed map)
            (define-key evil-insert-state-local-map prefixed map)))
      (define-key evil-motion-state-local-map (read-kbd-macro evil-leader/leader) nil)
      (when evil-leader/in-all-states
        (define-key evil-emacs-state-local-map prefixed nil)
        (define-key evil-insert-state-local-map prefixed nil)))))

(defun evil-leader/set-leader (key &optional prefix)
  "Set leader key to `key' and non-normal-prefix to `prefix' and remove old bindings.

Passing `nil' as `prefix' leaves prefix unchanged."
  (let ((global-on global-evil-leader-mode)
        (local-on evil-leader-mode))
    (when local-on
      (evil-leader-mode -1))
    (when global-on
      (global-evil-leader-mode -1))
    (setq evil-leader/leader key)
    (when prefix
      (setq evil-leader/non-normal-prefix prefix))
    (if global-on
        (global-evil-leader-mode 1)
      (when local-on
        (evil-leader-mode 1)))))

;;;###autoload
(defun evil-leader/set-key (key def &rest bindings)
  "Bind `key' to command `def' in `evil-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs."
  (interactive "kKey: \naCommand: ")
  (evil-leader--def-keys evil-leader--default-map key def bindings))
(put 'evil-leader/set-key 'lisp-indent-function 'defun)

;;;###autoload
(defun evil-leader/set-key-for-mode (mode key def &rest bindings)
  "Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-leader/set-key'."
  (interactive "SMode: \nkKey: \naCommand: ")
  (let ((mode-map (cdr (assoc mode evil-leader--mode-maps))))
    (unless mode-map
      (setq mode-map (make-sparse-keymap))
      (set-keymap-parent mode-map evil-leader--default-map)
      (push (cons mode mode-map) evil-leader--mode-maps))
    (evil-leader--def-keys mode-map key def bindings)))
(put 'evil-leader/set-key-for-mode 'lisp-indent-function 'defun)

(defun evil-leader--def-keys (map key def bindings)
  (while key
    (define-key map (read-kbd-macro key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(provide 'evil-leader)
;;; evil-leader.el ends here
