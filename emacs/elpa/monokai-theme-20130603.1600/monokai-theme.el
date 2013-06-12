;;; monokai-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2013

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai
;; Version: 20130603.1600
;; X-Original-Version: 1.2

;; This program is free software; you can redistribute it and/or modify
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
;; A port of the popular Textmate theme Monokai for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme for vim on which this port
;; is based.
;; Bozhidar Batsov created zenburn-theme.el on which this file is based.
;; http://colorschemedesigner.com/ for complementary colours.

;;; Code:

(deftheme monokai "The Monokai color theme")

;;; Color Palette

(defvar monokai-colors-alist
    '(("monokai-bg-1"      . "#171A0B")
      ("monokai-bg"        . "#272822")
      ("monokai-bg+1"      . "#3E3D31")
      ("monokai-bg+2"      . "#49483E")
      ("monokai-red-1"     . "#A20C41")
      ("monokai-red"       . "#F92672")
      ("monokai-red+1"     . "#FC5C94")
      ("monokai-red+2"     . "#FC87B0")
      ("monokai-green-1"   . "#67930F")
      ("monokai-green"     . "#A6E22E")
      ("monokai-green+1"   . "#C1F161")
      ("monokai-green+2"   . "#CDF187")
      ("monokai-orange-1"  . "#A45E0A")
      ("monokai-orange"    . "#FD971F")
      ("monokai-orange+1"  . "#FEB257")
      ("monokai-orange+2"  . "#FEC683")
      ("monokai-yellow-1"  . "#968B26")
      ("monokai-yellow"    . "#E6DB74")
      ("monokai-yellow+1"  . "#F3EA98")
      ("monokai-yellow+2"  . "#F3ECB0")
      ("monokai-blue-1"    . "#21889B")
      ("monokai-blue"      . "#66D9EF")
      ("monokai-blue+1"    . "#8DE6F7")
      ("monokai-blue+2"    . "#A9EBF7")
      ("monokai-purple-1"  . "#562AA6")
      ("monokai-purple"    . "#AE81FF")
      ("monokai-purple+1"  . "#C2A1FF")
      ("monokai-purple+2"  . "#D2BAFF")
      ("monokai-magenta-1" . "#A41F99")
      ("monokai-magenta"   . "#FD5FF0")
      ("monokai-magenta+1" . "#FE87F4")
      ("monokai-magenta+2" . "#FEA7F7")
      ("monokai-cyan-1"    . "#349B8D")
      ("monokai-cyan"      . "#A1EFE4")
      ("monokai-cyan+1"    . "#BBF7EF")
      ("monokai-cyan+2"    . "#CBF7F1")
      ("monokai-fg-1"      . "#75715E")
      ("monokai-fg"        . "#F8F8F2")
      ("monokai-fg+1"      . "#F8F8F0"))
  "List of Monokai colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro monokai-with-color-variables (&rest body)
  "`let' bind all colors defined in `monokai-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   monokai-colors-alist))
     ,@body))

;;; Theme Faces
(monokai-with-color-variables
 (custom-theme-set-faces
  'monokai
;;;; Built-in
;;;;; basic coloring
   `(button ((t (:underline t))))
   `(link ((t (:foreground ,monokai-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,monokai-purple :underline t :weight normal))))

   `(default ((t (:foreground ,monokai-fg :background ,monokai-bg))))
   `(cursor ((t (:foreground ,monokai-bg-1 :background ,monokai-fg))))
   `(escape-glyph ((t (:foreground ,monokai-fg-1 :bold t))))
   `(fringe ((t (:foreground ,monokai-fg :background ,monokai-bg))))
   `(header-line ((t (:
foreground ,monokai-fg-1
                                  :background ,monokai-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:foreground ,monokai-fg+1 :background ,monokai-bg+2))))
   `(success ((t (:foreground ,monokai-green :weight bold))))
   `(warning ((t (:foreground ,monokai-orange :weight bold))))
   `(menu ((t (:foreground ,monokai-fg :background ,monokai-bg))))
   `(minibuffer-prompt ((t (:foreground ,monokai-blue))))
   `(mode-line
     ((,class (:foreground ,monokai-fg
                           :background ,monokai-bg+2
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,monokai-green :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,monokai-bg+2
                      :background ,monokai-bg
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,monokai-bg+2))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,monokai-bg+2))))
   `(trailing-whitespace ((t (:background ,monokai-red))))
   `(vertical-border ((t (:foreground ,monokai-bg+2))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,monokai-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,monokai-green))))
   `(compilation-error-face ((t (:foreground ,monokai-red :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,monokai-fg))))
   `(compilation-info-face ((t (:foreground ,monokai-blue))))
   `(compilation-info ((t (:foreground ,monokai-purple :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,monokai-green))))
   `(compilation-line-face ((t (:foreground ,monokai-bg+2))))
   `(compilation-line-number ((t (:foreground ,monokai-bg+2))))
   `(compilation-message-face ((t (:foreground ,monokai-blue))))
   `(compilation-warning-face ((t (:foreground ,monokai-orange :weight bold :underline t))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,monokai-fg))))
   `(grep-error-face ((t (:foreground ,monokai-red :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,monokai-magenta))))
   `(grep-match-face ((t (:foreground ,monokai-orange :weight bold))))
   `(match ((t (:foreground ,monokai-green :background ,monokai-bg-1 :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,monokai-orange :background ,monokai-bg-1))))
   `(isearch-fail ((t (:foreground ,monokai-fg+1 :background ,monokai-red))))
   `(lazy-highlight ((t (:foreground ,monokai-magenta :background ,monokai-bg-1))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,monokai-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,monokai-fg-1))))
   `(font-lock-comment-face ((t (:foreground ,monokai-fg-1))))
   `(font-lock-constant-face ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-face ((t (:foreground ,monokai-fg-1))))
   `(font-lock-doc-string-face ((t (:foreground ,monokai-fg-1))))
   `(font-lock-function-name-face ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face ((t (:foreground ,monokai-red :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,monokai-fg-1))))
   `(font-lock-preprocessor-face ((t (:foreground ,monokai-red))))
   `(font-lock-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-type-face ((t (:foreground ,monokai-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-orange))))
   `(font-lock-warning-face ((t (:foreground ,monokai-yellow-1 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,monokai-fg))))
   `(newsticker-default-face ((t (:foreground ,monokai-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,monokai-green+1))))
   `(newsticker-extra-face ((t (:foreground ,monokai-fg-1 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,monokai-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,monokai-green))))
   `(newsticker-new-item-face ((t (:foreground ,monokai-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,monokai-red))))
   `(newsticker-old-item-face ((t (:foreground ,monokai-fg-1))))
   `(newsticker-statistics-face ((t (:foreground ,monokai-fg))))
   `(newsticker-treeview-face ((t (:foreground ,monokai-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,monokai-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,monokai-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,monokai-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,monokai-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,monokai-fg-1))))
   `(newsticker-treeview-selection-face ((t (:foreground ,monokai-orange))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,monokai-fg-1 :background ,monokai-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,monokai-green :background ,monokai-bg :inverse-video nil))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,monokai-fg))))
   `(ack-file ((t (:foreground ,monokai-blue))))
   `(ack-line ((t (:foreground ,monokai-yellow))))
   `(ack-match ((t (:foreground ,monokai-orange :background ,monokai-bg-1 :weight bold))))
;;;;; acutes
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,monokai-fg-1 :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,monokai-bg+1 :foreground ,monokai-orange))))
   `(ac-selection-face ((t (:background ,monokai-bg+2 :foreground ,monokai-green))))
   `(popup-tip-face ((t (:background ,monokai-bg :foreground ,monokai-fg-1))))
   `(popup-scroll-bar-foreground-face ((t (:background ,monokai-blue-1))))
   `(popup-scroll-bar-background-face ((t (:background ,monokai-bg-1))))
   `(popup-isearch-match ((t (:background ,monokai-bg :foreground ,monokai-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,monokai-green))))
   `(android-mode-error-face ((t (:foreground ,monokai-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,monokai-blue))))
   `(android-mode-verbose-face ((t (:foreground ,monokai-purple))))
   `(android-mode-warning-face ((t (:foreground ,monokai-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,monokai-yellow-1 :foreground ,monokai-bg))))
   `(bm-fringe-face ((t (:background ,monokai-yellow-1 :foreground ,monokai-bg))))
   `(bm-fringe-persistent-face ((t (:background ,monokai-green-1 :foreground ,monokai-bg))))
   `(bm-persistent-face ((t (:background ,monokai-green-1 :foreground ,monokai-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,monokai-red :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,monokai-orange :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,monokai-green :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,monokai-blue :foreground ,monokai-bg))))
   `(ctbl:face-continue-bar ((t (:background ,monokai-bg-1 :foreground ,monokai-bg))))
   `(ctbl:face-row-select ((t (:background ,monokai-cyan :foreground ,monokai-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,monokai-green :background nil))
                 (t (:foreground ,monokai-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,monokai-yellow))))
   `(diff-removed ((,class (:foreground ,monokai-red :background nil))
                   (t (:foreground ,monokai-red-1 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,monokai-bg+1))
                  (t (:background ,monokai-fg :foreground ,monokai-bg))))
   `(diff-file-header
     ((,class (:background ,monokai-bg+1 :foreground ,monokai-fg :bold t))
      (t (:background ,monokai-fg :foreground ,monokai-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,monokai-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,monokai-orange))))
   `(diredp-date-time ((t (:foreground ,monokai-magenta))))
   `(diredp-deletion ((t (:foreground ,monokai-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,monokai-red))))
   `(diredp-dir-heading ((t (:foreground ,monokai-blue :background ,monokai-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,monokai-cyan))))
   `(diredp-exec-priv ((t (:foreground ,monokai-red))))
   `(diredp-executable-tag ((t (:foreground ,monokai-green+1))))
   `(diredp-file-name ((t (:foreground ,monokai-blue))))
   `(diredp-file-suffix ((t (:foreground ,monokai-green))))
   `(diredp-flag-mark ((t (:foreground ,monokai-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,monokai-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,monokai-red))))
   `(diredp-link-priv ((t (:foreground ,monokai-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,monokai-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,monokai-orange))))
   `(diredp-no-priv ((t (:foreground ,monokai-fg))))
   `(diredp-number ((t (:foreground ,monokai-green+1))))
   `(diredp-other-priv ((t (:foreground ,monokai-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,monokai-red-1))))
   `(diredp-read-priv ((t (:foreground ,monokai-green-1))))
   `(diredp-symlink ((t (:foreground ,monokai-yellow))))
   `(diredp-write-priv ((t (:foreground ,monokai-magenta))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,monokai-green :background ,monokai-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,monokai-red :background ,monokai-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,monokai-fg :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,monokai-purple :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,monokai-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,monokai-green :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,monokai-red))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,monokai-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,monokai-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-red) :inherit unspecified))
      (t (:foreground ,monokai-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-orange) :inherit unspecified))
      (t (:foreground ,monokai-orange-1 :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,monokai-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,monokai-orange-1 :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,monokai-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,monokai-orange-1 :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-blue)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,monokai-blue-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-orange) :inherit unspecified))
      (t (:foreground ,monokai-orange-1 :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,monokai-red) :inherit unspecified))
      (t (:foreground ,monokai-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,monokai-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,monokai-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,monokai-fg-1))))
   `(erc-keyword-face ((t (:foreground ,monokai-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,monokai-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,monokai-green))))
   `(erc-pal-face ((t (:foreground ,monokai-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,monokai-orange :background ,monokai-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,monokai-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((,class (:foreground ,monokai-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((,class (:foreground ,monokai-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((,class (:foreground ,monokai-yellow :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,monokai-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,monokai-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,monokai-yellow :weight bold))))
   ;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,monokai-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,monokai-blue))))
   `(gnus-summary-high-read ((t (:foreground ,monokai-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,monokai-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,monokai-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,monokai-blue))))
   `(gnus-summary-low-read ((t (:foreground ,monokai-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,monokai-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,monokai-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,monokai-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,monokai-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,monokai-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,monokai-fg))))
   `(gnus-summary-selected ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,monokai-blue))))
   `(gnus-cite-10 ((t (:foreground ,monokai-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,monokai-yellow))))
   `(gnus-cite-2 ((t (:foreground ,monokai-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,monokai-cyan))))
   `(gnus-cite-4 ((t (:foreground ,monokai-purple))))
   `(gnus-cite-5 ((t (:foreground ,monokai-green+1))))
   `(gnus-cite-6 ((t (:foreground ,monokai-green))))
   `(gnus-cite-7 ((t (:foreground ,monokai-red))))
   `(gnus-cite-8 ((t (:foreground ,monokai-red))))
   `(gnus-cite-9 ((t (:foreground ,monokai-purple))))
   `(gnus-group-news-1-empty ((t (:foreground ,monokai-fg-1))))
   `(gnus-group-news-2-empty ((t (:foreground ,monokai-green+1))))
   `(gnus-group-news-3-empty ((t (:foreground ,monokai-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,monokai-cyan))))
   `(gnus-group-news-5-empty ((t (:foreground ,monokai-cyan+1))))
   `(gnus-group-news-6-empty ((t (:foreground ,monokai-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,monokai-bg+2))))
   `(gnus-signature ((t (:foreground ,monokai-fg-1))))
   `(gnus-x ((t (:background ,monokai-fg :foreground ,monokai-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,monokai-blue))))
   `(guide-key/key-face ((t (:foreground ,monokai-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,monokai-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,monokai-green
                      :background ,monokai-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,monokai-yellow
                      :background ,monokai-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,monokai-bg+2 :underline nil))))
   `(helm-selection-line ((t (:background ,monokai-bg+1))))
   `(helm-visible-mark ((t (:foreground ,monokai-fg-1 :background ,monokai-bg+2))))
   `(helm-candidate-number ((t (:foreground ,monokai-green+2 :background ,monokai-bg-1))))
   `(helm-ff-directory ((t (:foreground ,monokai-magenta))))
;;;;; hl-line-mode
   `(hl-line((nil (:background ,monokai-bg+1 :inherit t))))
;;;;; hl-sexp
   `(hl-sexp-face ((nil (:background ,monokai-bg+1 :inherit t))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,monokai-orange :background ,monokai-bg :weight bold))))
   `(ido-only-match ((t (:foreground ,monokai-green :background ,monokai-bg :weight bold))))
   `(ido-subdir ((t (:foreground ,monokai-blue :background ,monokai-bg))))
;;;;; Js2-mode
   `(js2-warning-face ((t (:underline ,monokai-orange))))
   `(js2-error-face ((t (:foreground ,monokai-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,monokai-green))))
   `(js2-jsdoc-type-face ((t (:foreground ,monokai-blue))))
   `(js2-jsdoc-value-face ((t (:foreground ,monokai-purple))))
   `(js2-function-param-face ((t (:foreground ,monokai-green))))
   `(js2-external-variable-face ((t (:foreground ,monokai-yellow))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,monokai-purple))))
   `(jabber-roster-user-online ((t (:foreground ,monokai-green))))
   `(jabber-roster-user-dnd ((t (:foreground ,monokai-red))))
   `(jabber-rare-time-face ((t (:foreground ,monokai-purple))))
   `(jabber-chat-prompt-local ((t (:foreground ,monokai-blue))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,monokai-orange))))
   `(jabber-activity-face((t (:foreground ,monokai-red))))
   `(jabber-activity-personal-face ((t (:foreground ,monokai-cyan))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,monokai-purple :background ,monokai-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,monokai-green+2 :background ,monokai-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,monokai-red+1 :background ,monokai-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,monokai-blue+1 :background ,monokai-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,monokai-magenta :background ,monokai-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,monokai-yellow :background ,monokai-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,monokai-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,monokai-blue :weight bold))))
   `(magit-item-highlight ((t (:background ,monokai-bg+2))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,monokai-green+1))))
   `(message-header-other ((t (:foreground ,monokai-green))))
   `(message-header-to ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(message-header-from ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(message-header-cc ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(message-header-subject ((t (:foreground ,monokai-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,monokai-green))))
   `(message-mml ((t (:foreground ,monokai-fg-1 :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,monokai-fg))))
   `(egg-help-header-1 ((t (:foreground ,monokai-blue))))
   `(egg-help-header-2 ((t (:foreground ,monokai-green))))
   `(egg-branch ((t (:foreground ,monokai-blue))))
   `(egg-branch-mono ((t (:foreground ,monokai-blue))))
   `(egg-term ((t (:foreground ,monokai-blue))))
   `(egg-diff-add ((t (:foreground ,monokai-green+1))))
   `(egg-diff-del ((t (:foreground ,monokai-red+1))))
   `(egg-diff-file-header ((t (:foreground ,monokai-fg-1))))
   `(egg-section-title ((t (:foreground ,monokai-blue))))
   `(egg-stash-mono ((t (:foreground ,monokai-purple))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,monokai-green+1))))
   `(message-header-other ((t (:foreground ,monokai-green))))
   `(message-header-to ((t (:foreground ,monokai-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,monokai-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,monokai-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,monokai-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,monokai-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,monokai-green))))
   `(message-mml ((t (:foreground ,monokai-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,monokai-orange))))
   `(mew-face-header-from ((t (:foreground ,monokai-yellow))))
   `(mew-face-header-date ((t (:foreground ,monokai-green))))
   `(mew-face-header-to ((t (:foreground ,monokai-red))))
   `(mew-face-header-key ((t (:foreground ,monokai-blue))))
   `(mew-face-header-private ((t (:foreground ,monokai-purple))))
   `(mew-face-header-important ((t (:foreground ,monokai-blue))))
   `(mew-face-header-marginal ((t (:foreground ,monokai-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,monokai-red))))
   `(mew-face-header-xmew ((t (:foreground ,monokai-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,monokai-red))))
   `(mew-face-body-url ((t (:foreground ,monokai-blue))))
   `(mew-face-body-comment ((t (:foreground ,monokai-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,monokai-cyan))))
   `(mew-face-body-cite2 ((t (:foreground ,monokai-purple))))
   `(mew-face-body-cite3 ((t (:foreground ,monokai-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,monokai-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,monokai-red))))
   `(mew-face-mark-review ((t (:foreground ,monokai-blue))))
   `(mew-face-mark-escape ((t (:foreground ,monokai-green))))
   `(mew-face-mark-delete ((t (:foreground ,monokai-red))))
   `(mew-face-mark-unlink ((t (:foreground ,monokai-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,monokai-orange))))
   `(mew-face-mark-unread ((t (:foreground ,monokai-purple))))
   `(mew-face-eof-message ((t (:foreground ,monokai-red))))
   `(mew-face-eof-part ((t (:foreground ,monokai-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,monokai-cyan :background ,monokai-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,monokai-bg :background ,monokai-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,monokai-bg :background ,monokai-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,monokai-blue))))
   `(mingus-pausing-face ((t (:foreground ,monokai-magenta))))
   `(mingus-playing-face ((t (:foreground ,monokai-cyan))))
   `(mingus-playlist-face ((t (:foreground ,monokai-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,monokai-yellow))))
   `(mingus-stopped-face ((t (:foreground ,monokai-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,monokai-fg-1))))
   `(nav-face-button-num ((t (:foreground ,monokai-blue))))
   `(nav-face-dir ((t (:foreground ,monokai-green))))
   `(nav-face-hdir ((t (:foreground ,monokai-red))))
   `(nav-face-file ((t (:foreground ,monokai-fg))))
   `(nav-face-hfile ((t (:foreground ,monokai-purple))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,monokai-blue   :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,monokai-purple :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,monokai-cyan   :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,monokai-green  :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,monokai-orange :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,monokai-purple :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,monokai-yellow :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,monokai-fg-1))))
   `(mu4e-trashed-face ((t (:foreground ,monokai-fg-1 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,monokai-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,monokai-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,monokai-fg-1))))
   `(mumamo-background-chunk-submode4 ((t (:background ,monokai-bg+2))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,monokai-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,monokai-fg :weight bold))))
   `(org-checkbox ((t (:background ,monokai-bg+2 :foreground ,monokai-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,monokai-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,monokai-red))))
   `(org-done ((t (:bold t :weight bold :foreground ,monokai-green+1))))
   `(org-formula ((t (:foreground ,monokai-yellow))))
   `(org-headline-done ((t (:foreground ,monokai-green+1))))
   `(org-hide ((t (:foreground ,monokai-bg-1))))
   `(org-level-1 ((t (:foreground ,monokai-orange))))
   `(org-level-2 ((t (:foreground ,monokai-orange+1))))
   `(org-level-3 ((t (:foreground ,monokai-cyan))))
   `(org-level-4 ((t (:foreground ,monokai-cyan+1))))
   `(org-level-5 ((t (:foreground ,monokai-purple))))
   `(org-level-6 ((t (:foreground ,monokai-purple+1))))
   `(org-level-7 ((t (:foreground ,monokai-red+1))))
   `(org-level-8 ((t (:foreground ,monokai-red+2))))
   `(org-link ((t (:foreground ,monokai-blue :underline t))))
   `(org-scheduled ((t (:foreground ,monokai-purple+1))))
   `(org-scheduled-previously ((t (:foreground ,monokai-red))))
   `(org-scheduled-today ((t (:foreground ,monokai-blue+1))))
   `(org-special-keyword ((t (:foreground ,monokai-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,monokai-purple))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,monokai-orange))))
   `(org-todo ((t (:bold t :foreground ,monokai-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,monokai-red :weight bold :underline nil))))
   `(org-column ((t (:background ,monokai-bg-1))))
   `(org-column-title ((t (:background ,monokai-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,monokai-orange))))
   `(outline-2 ((t (:foreground ,monokai-magenta))))
   `(outline-3 ((t (:foreground ,monokai-blue))))
   `(outline-4 ((t (:foreground ,monokai-yellow))))
   `(outline-5 ((t (:foreground ,monokai-cyan))))
   `(outline-6 ((t (:foreground ,monokai-purple))))
   `(outline-7 ((t (:foreground ,monokai-red))))
   `(outline-8 ((t (:foreground ,monokai-green))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,monokai-orange))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,monokai-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,monokai-purple))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,monokai-green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,monokai-cyan))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,monokai-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,monokai-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,monokai-blue+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,monokai-red+1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,monokai-green+1))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,monokai-purple+1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,monokai-orange+1))))
;;;;;rcirc
   `(rcirc-my-nick ((t (:foreground ,monokai-blue))))
   `(rcirc-other-nick ((t (:foreground ,monokai-orange))))
   `(rcirc-bright-nick ((t (:foreground ,monokai-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,monokai-blue-1))))
   `(rcirc-server ((t (:foreground ,monokai-green))))
   `(rcirc-server-prefix ((t (:foreground ,monokai-green+1))))
   `(rcirc-timestamp ((t (:foreground ,monokai-purple))))
   `(rcirc-nick-in-message ((t (:foreground ,monokai-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,monokai-fg-1 :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,monokai-red :bold t))))
;;;;; rhtml-mode
   `(erb-face ((t (:foreground ,monokai-fg+1 :background ,monokai-bg-1))))
   `(erb-delim-face ((t (:foreground ,monokai-cyan-1 :background ,monokai-bg-1))))
   `(erb-exec-face ((t (:foreground ,monokai-fg+1 :background ,monokai-bg-1))))
   `(erb-exec-delim-face ((t (:foreground ,monokai-cyan-1 :background ,monokai-bg-1))))
   `(erb-out-face ((t (:foreground ,monokai-fg+1 :background ,monokai-bg-1))))
   `(erb-out-delim-face ((t (:foreground ,monokai-cyan-1 :background ,monokai-bg-1))))
   `(erb-comment-face ((t (:foreground ,monokai-fg-1 :background ,monokai-bg-1))))
   `(erb-comment-delim-face ((t (:foreground ,monokai-cyan-1 :background ,monokai-bg-1))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,monokai-green))))
   `(rpm-spec-doc-face ((t (:foreground ,monokai-blue))))
   `(rpm-spec-ghost-face ((t (:foreground ,monokai-magenta))))
   `(rpm-spec-macro-face ((t (:foreground ,monokai-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,monokai-red))))
   `(rpm-spec-package-face ((t (:foreground ,monokai-purple))))
   `(rpm-spec-section-face ((t (:foreground ,monokai-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,monokai-purple))))
   `(rpm-spec-var-face ((t (:foreground ,monokai-orange))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,monokai-orange))))
   `(rst-level-2-face ((t (:foreground ,monokai-green))))
   `(rst-level-3-face ((t (:foreground ,monokai-blue))))
   `(rst-level-4-face ((t (:foreground ,monokai-yellow))))
   `(rst-level-5-face ((t (:foreground ,monokai-purple))))
   `(rst-level-6-face ((t (:foreground ,monokai-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,monokai-magenta :background ,monokai-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,monokai-cyan :background ,monokai-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,monokai-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,monokai-fg
                                    :background ,monokai-bg))))
   `(tabbar-selected ((t (:foreground ,monokai-fg
                                      :background ,monokai-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,monokai-fg
                                        :background ,monokai-bg+2
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,monokai-bg+2
                                       :background ,monokai-fg-1))))
   `(term-color-red ((t (:foreground ,monokai-red
                                       :background ,monokai-red+1))))
   `(term-color-green ((t (:foreground ,monokai-green
                                       :background ,monokai-green+1))))
   `(term-color-yellow ((t (:foreground ,monokai-orange
                                       :background ,monokai-orange+1))))
   `(term-color-blue ((t (:foreground ,monokai-blue-1
                                      :background ,monokai-blue+1))))
   `(term-color-magenta ((t (:foreground ,monokai-purple
                                         :background ,monokai-purple+1))))
   `(term-color-cyan ((t (:foreground ,monokai-cyan
                                       :background ,monokai-cyan+1))))
   `(term-color-white ((t (:foreground ,monokai-fg
                                       :background ,monokai-fg+1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,monokai-bg+2))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,monokai-fg-1 :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,monokai-yellow-1
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,monokai-red :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,monokai-fg-1
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,monokai-purple :background ,monokai-bg))))
   `(w3m-lnum-match ((t (:background ,monokai-bg-1
                                     :foreground ,monokai-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,monokai-fg-1))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,monokai-bg :foreground ,monokai-bg-1))))
   `(whitespace-hspace ((t (:background ,monokai-bg :foreground ,monokai-bg-1))))
   `(whitespace-tab ((t (:background ,monokai-red))))
   `(whitespace-newline ((t (:foreground ,monokai-bg-1))))
   `(whitespace-trailing ((t (:background ,monokai-red))))
   `(whitespace-line ((t (:background ,monokai-bg :foreground ,monokai-red))))
   `(whitespace-space-before-tab ((t (:background ,monokai-bg :foreground ,monokai-orange))))
   `(whitespace-indentation ((t (:background ,monokai-fg-1 :foreground ,monokai-red))))
   `(whitespace-empty ((t (:background ,monokai-fg-1))))
   `(whitespace-space-after-tab ((t (:background ,monokai-fg-1 :foreground ,monokai-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,monokai-purple))))
   `(wl-highlight-folder-many-face ((t (:foreground ,monokai-red))))
   `(wl-highlight-folder-path-face ((t (:foreground ,monokai-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,monokai-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,monokai-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,monokai-cyan))))
   `(wl-highlight-message-citation-header ((t (:foreground ,monokai-yellow))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,monokai-purple))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,monokai-purple+1))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,monokai-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,monokai-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,monokai-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,monokai-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,monokai-purple))))
   `(wl-highlight-message-header-contents ((t (:foreground ,monokai-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,monokai-purple+1))))
   `(wl-highlight-message-signature ((t (:foreground ,monokai-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,monokai-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,monokai-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,monokai-fg :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,monokai-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,monokai-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,monokai-fg-1))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,monokai-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,monokai-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,monokai-purple+1))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,monokai-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,monokai-bg-1 :foreground ,monokai-bg-1))))
   ))

;;; Theme Variables
(monokai-with-color-variables
  (custom-theme-set-variables
   'monokai
;;;;; ansi-color
   `(ansi-color-names-vector [,monokai-bg ,monokai-red ,monokai-green ,monokai-orange
                                          ,monokai-blue ,monokai-purple ,monokai-cyan ,monokai-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,monokai-bg-1)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,monokai-fg-1)
       ( 40. . ,monokai-bg+2)
       ( 60. . ,monokai-red)
       ( 80. . ,monokai-red+1)
       (100. . ,monokai-orange)
       (120. . ,monokai-orange+1)
       (140. . ,monokai-green)
       (160. . ,monokai-green+1)
       (180. . ,monokai-yellow)
       (200. . ,monokai-yellow+1)
       (220. . ,monokai-blue)
       (240. . ,monokai-blue+1)
       (260. . ,monokai-purple)
       (280. . ,monokai-purple+1)
       (300. . ,monokai-cyan)
       (320. . ,monokai-cyan+1)
       (340. . ,monokai-magenta)
       (360. . ,monokai-magenta+1)))
   `(vc-annotate-very-old-color ,monokai-magenta)
   `(vc-annotate-background ,monokai-bg)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar monokai-add-font-lock-keywords nil
  "Whether to add font-lock keywords for monokai color names.
In buffers visiting library `monokai-theme.el' the monokai
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar monokai-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after monokai activate)
;;   "Maybe also add font-lock keywords for monokai colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or monokai-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "monokai-theme.el")))
;;     (unless monokai-colors-font-lock-keywords
;;       (setq monokai-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car monokai-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc monokai-colors-alist))))))
;;     (font-lock-add-keywords nil monokai-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after monokai activate)
;;   "Also remove font-lock keywords for monokai colors."
;;   (font-lock-remove-keywords nil monokai-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'monokai)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; monokai-theme.el ends here
