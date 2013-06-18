;;; evil-leader-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (evil-leader/set-key-for-mode evil-leader/set-key
;;;;;;  evil-leader-mode global-evil-leader-mode) "evil-leader" "../../../../.emacs.d/elpa/evil-leader-20130609.1840/evil-leader.el"
;;;;;;  "7e5190e911888d90e508c76ae5337b61")
;;; Generated autoloads from ../../../../.emacs.d/elpa/evil-leader-20130609.1840/evil-leader.el

(autoload 'global-evil-leader-mode "evil-leader" "\
Global minor mode for <leader> support.

\(fn &optional ARG)" t nil)

(autoload 'evil-leader-mode "evil-leader" "\
Minor mode to enable <leader> support.

\(fn &optional ARG)" t nil)

(autoload 'evil-leader/set-key "evil-leader" "\
Bind `key' to command `def' in `evil-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs.

\(fn KEY DEF &rest BINDINGS)" t nil)

(autoload 'evil-leader/set-key-for-mode "evil-leader" "\
Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-leader/set-key'.

\(fn MODE KEY DEF &rest BINDINGS)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/evil-leader-20130609.1840/evil-leader-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/evil-leader-20130609.1840/evil-leader.el")
;;;;;;  (20928 19546 27813 93000))

;;;***

(provide 'evil-leader-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-leader-autoloads.el ends here
