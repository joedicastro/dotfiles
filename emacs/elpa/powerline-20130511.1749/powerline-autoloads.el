;;; powerline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (powerline-nano-theme powerline-center-theme powerline-vim-theme
;;;;;;  powerline-default-theme powerline-fill powerline-raw defpowerline
;;;;;;  powerline-concat powerline-mouse powerline-hud) "powerline"
;;;;;;  "powerline.el" (20920 27367 343359 620000))
;;; Generated autoloads from powerline.el

(autoload 'powerline-hud "powerline" "\
Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline" "\
Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline" "\
Create function NAME by wrapping BODY with powerline padding an propetization.

\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline" "\
Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).

\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline" "\
Return empty space using FACE and leaving RESERVE space on the right.

\(fn FACE RESERVE)" nil nil)

(defpowerline powerline-major-mode (propertize (format-mode-line mode-name) 'mouse-face 'mode-line-highlight 'help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] `(menu-item ,(purecopy "Menu Bar") ignore :filter (lambda (_) (mouse-menu-major-mode-map)))) (define-key map [mode-line mouse-2] 'describe-mode) (define-key map [mode-line down-mouse-3] mode-line-mode-menu) map)))

(defpowerline powerline-minor-modes (mapconcat (lambda (mm) (propertize mm 'mouse-face 'mode-line-highlight 'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] (powerline-mouse 'minor 'menu mm)) (define-key map [mode-line mouse-2] (powerline-mouse 'minor 'help mm)) (define-key map [mode-line down-mouse-3] (powerline-mouse 'minor 'menu mm)) (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm)) map))) (split-string (format-mode-line minor-mode-alist)) (propertize " " 'face face)))

(defpowerline powerline-narrow (let (real-point-min real-point-max) (save-excursion (save-restriction (widen) (setq real-point-min (point-min) real-point-max (point-max)))) (when (or (/= real-point-min (point-min)) (/= real-point-max (point-max))) (propertize "Narrow" 'mouse-face 'mode-line-highlight 'help-echo "mouse-1: Remove narrowing from the current buffer" 'local-map (make-mode-line-mouse-map 'mouse-1 'mode-line-widen)))))

(defpowerline powerline-vc (when (and (buffer-file-name (current-buffer)) vc-mode) (format-mode-line '(vc-mode vc-mode))))

(defpowerline powerline-buffer-size (propertize (if powerline-buffer-size-suffix "%I" "%i") 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 (lambda nil (interactive) (setq powerline-buffer-size-suffix (not powerline-buffer-size-suffix)) (redraw-modeline)))))

(defpowerline powerline-buffer-id (format-mode-line mode-line-buffer-identification))

(defpowerline powerline-process (cond ((listp mode-line-process) (format-mode-line mode-line-process)) (t mode-line-process)))

(autoload 'powerline-default-theme "powerline" "\
Setup a default mode-line.

\(fn)" t nil)

(autoload 'powerline-vim-theme "powerline" "\
Setup a default mode-line.

\(fn)" t nil)

(autoload 'powerline-center-theme "powerline" "\
Setup a default mode-line with major and minor modes centered.

\(fn)" t nil)

(autoload 'powerline-nano-theme "powerline" "\
Setup a nano-like mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("powerline-pkg.el" "powerline-separators.el")
;;;;;;  (20920 27367 397618 529000))

;;;***

(provide 'powerline-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; powerline-autoloads.el ends here
