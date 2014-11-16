
;; How it works

;; It uses one of the standard Emacs init files, =init.el= to load all
;; the configuration. This configuration is thought to be stored in the
;; standard =~/.emacs.d= directory and to setup this configuration you
;; need to symlink this =emacs= directory to that. The
;; =~/.emacs.d/init.el= comes from all the code blocks of this file
;; =~/emacs.d/readme.org= exported in a process that is called
;; "tangling". If a block is marked with =:tangle no= it will be
;; skipped. The tangling is made automatically each time that the
;; =readme.el= changes, via a hook, to ensure that both files are synced.

;; This is the hook to tangle a new =~/.emacs.d/init.el= each time that
;; this file changes.

;; originaly seen at https://github.com/larstvei/dot-emacs/blob/master/init.org
(defun joe/tangle-init ()
  "If the current buffer is 'readme.org' the code-blocks are
   tangled, and the tangled file is compiled."
  (when (or
           (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "readme.org")))
           (equal (buffer-file-name)
               (expand-file-name "~/dotfiles/emacs/readme.org")))
    (org-babel-tangle)))
    ;; (byte-compile-file (concat user-emacs-directory "init.el"))))

(add-hook 'after-save-hook 'joe/tangle-init)

;; Packages list

;; The list of packages to install in a fresh installation. The way to
;; maintain clean and updated this list for me is the following:

;; - Install a package from the =package.el= built-in interface via =M-x list-packages=
;; - Test it
;;   - If seems Ok and I want to use it regularly, add it to the list.
;;   - If I don't like it, delete the plugin directory in the =~/.emacs.d/elpa= tree.
;;     I don't care too much about other dependecies that could be also
;;     installed, I'll get rid of them in the next clean install.

; A package for line helps to mantain the list
(setq my-packages
    '(
        ag
        async
        auto-complete
        browse-kill-ring
        buffer-move
        calfw
        charmap
        diff-hl
        dired+
        elfeed
        evil
        evil-indent-textobject
        evil-leader
        evil-matchit
        evil-nerd-commenter
        fill-column-indicator
        flatland-theme
        git-commit-mode
        git-rebase-mode
        gitconfig-mode
        gitignore-mode
        google-maps
        graphviz-dot-mode
        helm
        helm-descbinds
        helm-themes
        haskell-mode
        ibuffer-vc
        ido-ubiquitous
        ido-vertical-mode
        ido-yes-or-no
        ipython
        jedi
        lua-mode
        magit
        markdown-mode
        monokai-theme
        mu4e-maildirs-extension
        multi-term
        org-plus-contrib
        paradox
        password-store
        perspective
        pretty-mode
        popwin
        rainbow-mode
        racket-mode
        rw-ispell
        rw-hunspell
        rw-language-and-country-codes
        smart-mode-line
        smartparens
        smex
        sml-mode
        sublime-themes
        surround
        swoop
        twittering-mode
        ujelly-theme
        undo-tree
        w3m
        yasnippet
))

;; Repositories

;; The ELPA repositories from where the packages are fetched.

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Auto-installation

;; The auto-installation process for all the packages that are not
;; already installed. This is for bootstrap a fresh install.

;;; initialize the packages and create the packages list if not exists
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;; install packages if not exists
(dolist (pkg my-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))

;; Encoding

;; Make sure that UTF-8 is used everywhere.

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)

;; Temporal directory

;; I like to keep all of the temporal files and dirs (cache, backups,
;; ...) in an unique directory. If this directory does not exists, then
;; create it

(unless (file-exists-p "~/.emacs.d/tmp")
   (make-directory "~/.emacs.d/tmp"))

;; Disable auto-save files

;; I prefer to use a undo-tree with branches that store auto-save files.

(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Disable Backups

;; Because I'm using gpg to authetication and encrypt/sign files, is more
;; secure don't have a plaint text backup of those files. Use a DVCS and
;; backup your files regularly, for God's sake!

(setq make-backup-files nil)

;; History

;; Maintain a history of past actions.

(setq savehist-file "~/.emacs.d/tmp/history")
(setq-default history-length 1000)
(savehist-mode t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Show matching parenthesis

;; Show the matching parenthesis when the cursor is above one of them.

(setq show-paren-delay 0)
(show-paren-mode t)

;; Toggle show trailing white-spaces

;; Show/hide the trailing white-spaces in the buffer.

;; from http://stackoverflow.com/a/11701899/634816
(defun joe/toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Always indent with spaces

;; No more tabs, please, use damn spaces, for God's sake!

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)

;; Replace yes/no questions with y/n

;; Less keystrokes, I already press enough keys along the day.

(fset 'yes-or-no-p 'y-or-n-p)

;; Mondays are the first day of the week (for M-x calendar)

;; Set the calendar to my country's calendar standards

(setq-default calendar-week-start-day 1)
(setq calendar-latitude 43.36)
(setq calendar-longitude 8.38)
(setq calendar-location-name "A Coruña, Spain")

;; Use undo-tree for better undo

;; Emacs's undo system allows you to recover any past state of a buffer
;; (the standard undo/redo system loses any "redoable" states whenever
;; you make an edit). However, Emacs's solution, to treat "undo" itself
;; as just another editing action that can be undone, can be confusing
;; and difficult to use.

;; Both the loss of data with standard undo/redo and the confusion of
;; Emacs' undo stem from trying to treat undo history as a linear
;; sequence of changes. =undo-tree-mode= instead treats undo history as
;; what it is: a branching tree of changes (the same system that Vim has
;; had for some time now). This makes it substantially easier to undo and
;; redo any change, while preserving the entire history of past states.

(require 'undo-tree)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo")))
(setq undo-tree-visualizer-timestamps t)
(global-undo-tree-mode)

;; Recent files

;; Recentf is a minor mode that builds a list of recently opened
;; files. This list is is automatically saved across Emacs sessions. You
;; can then access this list through a menu.

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/tmp/recentf")
(recentf-mode t)
(setq recentf-max-saved-items 100)

;; Keep session between emacs runs (Desktop)

;; Desktop Save Mode is a feature to save the state of Emacs from one
;; session to another.

(require 'desktop)
(setq desktop-path '("~/.emacs.d/tmp/"))
(setq desktop-dirname "~/.emacs.d/tmp/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-globals-to-save
      (append '((extended-command-history . 50)
                (file-name-history . 200)
                (grep-history . 50)
                (compile-history . 50)
                (minibuffer-history . 100)
                (query-replace-history . 100)
                (read-expression-history . 100)
                (regexp-history . 100)
                (regexp-search-ring . 100)
                (search-ring . 50)
                (shell-command-history . 50)
                tags-file-name
                register-alist)))
(desktop-save-mode 1)

;; Remove beep

;; Remove the annoying beep.

(setq visible-bell t)

;; Open large files

;; Warn only when opening files bigger than 100MB

(setq large-file-warning-threshold 100000000)

;; Save cursor position across sessions

;; Save the cursor position for every file you opened. So, next
;; time you open the file, the cursor will be at the position you last
;; opened it.

(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "tmp/saveplace.el") )
(setq-default save-place t)

;; Tramp

;; Store the Tramp history in the temporal directory.

(setq tramp-persistency-file-name (concat user-emacs-directory "tmp/tramp"))

;; Bookmarks

;; Store the Bookmarks file in the temporal directory.

(setq bookmark-default-file (concat user-emacs-directory "tmp/bookmarks"))

;; SemanticDB

;; Store the SemanticDB files in the temporal directory.

(setq semanticdb-default-save-directory (concat user-emacs-directory "tmp/semanticdb"))

;; Url

;; Store the url files in the temporal directory.

(setq url-configuration-directory (concat user-emacs-directory "tmp/url"))

;; eshell

;; Store the eshell files in the temporal directory.

(setq eshell-directory-name (concat user-emacs-directory "tmp/eshell" ))

;; Kill internal processes via the =list process= buffer

;; Add a functionality to be able to kill process directly in the =list process'= buffer

;; seen at http://stackoverflow.com/a/18034042
    (define-key process-menu-mode-map (kbd "C-c k") 'joe/delete-process-at-point)

    (defun joe/delete-process-at-point ()
      (interactive)
      (let ((process (get-text-property (point) 'tabulated-list-id)))
        (cond ((and process
                    (processp process))
               (delete-process process)
               (revert-buffer))
              (t
               (error "no process at point!")))))

;; TODO Use ibuffer by default

;; Ibuffer is an advanced replacement for BufferMenu, which lets you
;; operate on buffers much in the same manner as Dired.

(defalias 'list-buffers 'ibuffer)

;; User ibuffer-vc by default

;; [[https://github.com/purcell/ibuffer-vc][ibuffer-vc]] show the buffers grouped by the associated version control
;; project.

(add-hook 'ibuffer-hook
    (lambda ()
        (ibuffer-vc-set-filter-groups-by-vc-root)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic))))


(setq ibuffer-formats
    '((mark modified read-only vc-status-mini " "
        (name 18 18 :left :elide)
        " "
        (size 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        (vc-status 16 16 :left)
        " "
        filename-and-process)))

;; Remove the welcome screen

;; The welcome screen is for guests only, I'm in home now!

(setq inhibit-startup-screen t)

;; Remove the message in the scratch buffer

;; Idem as above for the same reasons.

(setq initial-scratch-message "")

;; Hide the menu bar

(menu-bar-mode -1)

;; Hide the tool bar

(tool-bar-mode -1)

;; Hide the scroll bar

(scroll-bar-mode -1)

;; Show the column number

(column-number-mode t)

;; Show the buffer size (bytes)

(setq size-indication-mode t)

;; Show the current function

;; This is very useful in programming and also to see the headers in
;; outlines modes.

(which-function-mode 1)

;; Smart mode line

;; This package shows a very nice and very informative mode line.

;; to avoid the annoying confirmation question at the beginning
(defvar sml-dark-theme
  (substring
   (shell-command-to-string
    "sha256sum ~/.emacs.d/elpa/smart-mode-line-*/smart-mode-line-dark-theme.el | cut -d ' ' -f 1")
   0 -1))

(add-to-list 'custom-safe-themes sml-dark-theme)

;;; smart-mode-line
(require 'smart-mode-line)
(setq sml/mode-width 'full)
(setq sml/name-width 30)
(setq sml/shorten-modes t)
;; since I'm using the emacs daemon, to work properly, I have to make
;; the setup after the frame is made. So, I call this command in the
;; "Color Theme" section.
;; (sml/setup)

;; Color Theme

;; Here I define the default theme, a total subjective decision, of
;; course. This configuration works in terminal/graphic mode and in
;; client/server or standalone frames.

;; *Remember: when testing a new theme, disable before the current one or
;; use =helm-themes=.*

(setq myGraphicModeHash (make-hash-table :test 'equal :size 2))
(puthash "gui" t myGraphicModeHash)
(puthash "term" t myGraphicModeHash)

(defun emacsclient-setup-theme-function (frame)
  (let ((gui (gethash "gui" myGraphicModeHash))
        (ter (gethash "term" myGraphicModeHash)))
    (progn
      (select-frame frame)
      (when (or gui ter)
        (progn
          (load-theme 'monokai t)
          ;; setup the smart-mode-line and its theme
          (sml/setup)
          (sml/apply-theme 'dark)
          (if (display-graphic-p)
              (puthash "gui" nil myGraphicModeHash)
            (puthash "term" nil myGraphicModeHash))))
      (when (not (and gui ter))
        (remove-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)))))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)
  (progn (load-theme 'monokai t)
         (sml/setup)))

;; Font

;; The font to use. I choose monospace and /Dejavu Sans Mono/ because is
;; an open font and has the best Unicode support, and looks very fine to me too!

(set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)

;; Font Fallback for Unicode

;; Set a font with great support for Unicode Symbols
;; to fallback in those case where certain Unicode glyphs are
;; missing in the current font.

(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Cursor not blinking

;; The blinking cursor is pretty annoying, so disable it.

(blink-cursor-mode -1)

;; Highlight the current line

;; To help us to locate where the cursor is.

(global-hl-line-mode 1)

;; Show empty lines

;; This option show the empty lines at the end (bottom) of the buffer.

(toggle-indicate-empty-lines)

;; Pretty mode

;; Use mathematical *Unicode* /symbols/ instead of expressions or keywords in
;; some programming languages

(global-pretty-mode t)

;; Better line numbers

;; Display a more appealing line numbers.

; 2014-04-04: Holy moly its effort to get line numbers like vim!
; http://www.emacswiki.org/emacs/LineNumbers#toc6
(unless window-system
  (add-hook 'linum-before-numbering-hook
(lambda ()
(setq-local linum-format-fmt
(let ((w (length (number-to-string
(count-lines (point-min) (point-max))))))
(concat "%" (number-to-string w) "d"))))))

(defun joe/linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'linum)))

(unless window-system
  (setq linum-format 'joe/linum-format-func))

;; Show fill column

;; Toggle the vertical column that indicates the fill threshold.

(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-column 79)

;; More thinner window divisions

;; The default windows divisions are more uglier than sin.

(fringe-mode '(1 . 1))

;; TODO Auto-completion

;; Auto Complete Mode (aka =auto-complete.el=, =auto-complete-mode=) is a
;; extension that automates and advances completion-system.

(require 'auto-complete)
(global-auto-complete-mode)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-ignore-case 'smart)
(setq ac-auto-start 2)
(ac-flyspell-workaround)

;; enable it globally

;; Make it available everywhere.

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;; auto-complete file

;; The file where store the history of auto-complete.

(setq ac-comphist-file (concat user-emacs-directory
             "temp/ac-comphist.dat"))

;; Delete after insertion over selection

;; Delete the previous selection when overrides it with a new insertion.

(delete-selection-mode)

;; TODO Basic indentation

(setq-default c-basic-offset 4)

;; Smartparens

;; Minor mode for Emacs that deals with parens pairs and tries to be
;; smart about it.

(require 'smartparens-config)
(smartparens-global-mode)

;; Backward-kill-word as alternative to Backspace

;; Kill the entire word instead of hitting Backspace key several
;; times. To do this will bind the =backward-kill-region= function to the
;; =C-w= key combination

(global-set-key "\C-w" 'backward-kill-word)

;; Rebind the original C-w binding

;; Now we reasigne the original binding to that combination to a new one

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Spell checking

;; Activate Spell Checking by default. Also use [[http://hunspell.sourceforge.net/][hunspell]] instead of
;; [[http://www.gnu.org/software/ispell/ispell.html][ispell]] as corrector.

;; Use hunspell instead of ispell
(setq ispell-program-name "hunspell")
(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(require 'rw-hunspell)
(setq ispell-dictionary "es_ES_hunspell")
;; The following is set via custom
(custom-set-variables
 '(rw-hunspell-default-dictionary "es_ES_hunspell")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t))

(defun joe/turn-on-spell-check ()
       (flyspell-mode 1))

;; enable spell-check in certain modes
(add-hook 'markdown-mode-hook 'joe/turn-on-spell-check)
(add-hook 'text-mode-hook 'joe/turn-on-spell-check)
(add-hook 'org-mode-hook 'joe/turn-on-spell-check)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Use evil

;; [[https://gitorious.org/evil/pages/Home][Evil]] is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.

;; | Binding | Call                        | Do                                      |
;; |---------+-----------------------------+-----------------------------------------|
;; | C-z     | evil-emacs-state            | Toggle evil-mode                        |
;; | \       | evil-execute-in-emacs-state | Execute the next command in emacs state |

(setq evil-shift-width 4)
(require 'evil)
(evil-mode 1)

;; ESC quits almost everywhere

;; Gotten from [[http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice][here]], trying to emulate the Vim behaviour

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; TODO Org-mode customization

;; Custom bindings for /Org-mode/.

(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "SPC") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "H") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "L") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "K") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "J") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "U") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "I") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "O") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "P") 'org-shiftmetadown)
(evil-define-key 'normal org-mode-map (kbd "t")   'org-todo)
(evil-define-key 'normal org-mode-map (kbd "-")   'org-cycle-list-bullet)

(evil-define-key 'insert org-mode-map (kbd "C-c .")
  '(lambda () (interactive) (org-time-stamp-inactive t)))

;; Elfeed customization

;; Custom bindings for Elfeed.

; elfeed-search
(evil-define-key 'normal elfeed-search-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal elfeed-search-mode-map (kbd "a") 'elfeed-search-update--force)
(evil-define-key 'normal elfeed-search-mode-map (kbd "A") 'elfeed-update)
(evil-define-key 'normal elfeed-search-mode-map (kbd "s") 'elfeed-search-live-filter)
(evil-define-key 'normal elfeed-search-mode-map (kbd "RET") 'elfeed-search-show-entry)
(evil-define-key 'normal elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url)
(evil-define-key 'normal elfeed-search-mode-map (kbd "y") 'elfeed-search-yank)
(evil-define-key 'normal elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread)
(evil-define-key 'normal elfeed-search-mode-map (kbd "u") 'elfeed-search-tag-all-unread)
(evil-define-key 'normal elfeed-search-mode-map (kbd "+") 'elfeed-search-tag-all)
(evil-define-key 'normal elfeed-search-mode-map (kbd "-") 'elfeed-search-untag-all)
(evil-define-key 'normal elfeed-search-mode-map (kbd "E") (lambda() (interactive)(find-file "~/.emacs.d/elfeed.el.gpg")))
; elfeed-show
(evil-define-key 'normal elfeed-show-mode-map (kbd "q") 'elfeed-kill-buffer)
(evil-define-key 'normal elfeed-show-mode-map (kbd "g") 'elfeed-show-refresh)
(evil-define-key 'normal elfeed-show-mode-map (kbd "n") 'elfeed-show-next)
(evil-define-key 'normal elfeed-show-mode-map (kbd "p") 'elfeed-show-prev)
(evil-define-key 'normal elfeed-show-mode-map (kbd "o") 'elfeed-show-visit)
(evil-define-key 'normal elfeed-show-mode-map (kbd "y") 'elfeed-show-yank)
(evil-define-key 'normal elfeed-show-mode-map (kbd "u") (elfeed-expose #'elfeed-show-tag 'unread))
(evil-define-key 'normal elfeed-show-mode-map (kbd "+") 'elfeed-show-tag)
(evil-define-key 'normal elfeed-show-mode-map (kbd "-") 'elfeed-show-untag)
(evil-define-key 'normal elfeed-show-mode-map (kbd "SPC") 'scroll-up)
(evil-define-key 'normal elfeed-show-mode-map (kbd "S-SPC") 'scroll-down)

;; Disable it in certain modes

;  (evil-set-initial-state 'elfeed-search-mode 'emacs)
;  (evil-set-initial-state 'elfeed-show-mode 'emacs)

;; TODO evil-leader

;; [[https://github.com/cofi/evil-leader][Evil Leader]] provides the =<leader>= feature from Vim that provides an
;; easy way to bind keys under a variable prefix key. For an experienced
;; Emacs User it is nothing more than a convoluted key map, but for a
;; Evil user coming from Vim it means an easier start.

;; The prefix =C-<leader>= allows to use it in those modes where evil is
;; not in normal state (e.g. magit)

;; | Binding    | Do                                           |
;; |------------+----------------------------------------------|
;; | <SPC>      | Leader key                                   |
;; | C-<leader> | Prefix + Leader key when not in normal state |
;; | .          | Repeat last leader command                   |
;; |------------+----------------------------------------------|
;; | <leader>-a |                                              |
;; | <leader>-b | Buffer bindings                              |
;; | <leader>-c | Flycheck bindings                            |
;; | <leader>-d |                                              |
;; | <leader>-e | Edition bindings                             |
;; | <leader>-f | File bindings                                |
;; | <leader>-g | Git bindings                                 |
;; | <leader>-h |                                              |
;; | <leader>-i | Internet bindings                            |
;; | <leader>-j |                                              |
;; | <leader>-k | Spell bindings                               |
;; | <leader>-l | Lisp bindings                                |
;; | <leader>-m | Menu bindings                                |
;; | <leader>-n | Narrow bindings                              |
;; | <leader>-o | Organization bindings                        |
;; | <leader>-p | Project bindings                             |
;; | <leader>-q | Exit bindings                                |
;; | <leader>-r |                                              |
;; | <leader>-s | Search bindings                              |
;; | <leader>-t |                                              |
;; | <leader>-u |                                              |
;; | <leader>-v |                                              |
;; | <leader>-w | Window bindings                              |
;; | <leader>-x | Shell/System bindings                        |
;; | <leader>-y |                                              |
;; | <leader>-z | Emacs Bindings                               |
;; |------------+----------------------------------------------|

(require 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader "<SPC>")

;; Buffer bindings

;; | Binding      | Call              | Do                                              |
;; |--------------+-------------------+-------------------------------------------------|
;; | <leader>-TAB |                   | Switch the last two buffers                     |
;; | <leader>-bb  | ido-switch-buffer | Switch buffer                                   |
;; | <leader>-bi  | ibuffer           | Switch buffer using ibuffer                     |
;; | <leader>-bj  | buf-move-up       | Move the buffer to the window above             |
;; | <leader>-bk  | buf-move-down     | Move the buffer to the window below             |
;; | <leader>-bh  | buf-move-left     | Move the buffer to the window at the left       |
;; | <leader>-bl  | buf-move-right    | Move the buffer to the window at the right      |
;; | <leader>-bd  | kill-buffer       | Kill a buffer                                   |
;; | <leader>-bw  | save-buffer       | Save current buffer in visited file if modified |
;; | <leader>-br  | read-only-mode    | Toggle between read & write and read-only mode  |
;; | <leader>-bu  |                   | Revert the buffer changes                       |

;; toggle between the last two buffers
(evil-leader/set-key "TAB"
      (lambda ()
        (interactive)
        (switch-to-buffer (other-buffer (current-buffer) t))))

(evil-leader/set-key
  "bb" 'ido-switch-buffer
  "bd" 'kill-buffer
  "bh" 'buf-move-left
  "bi" 'ibuffer
  "bj" 'buf-move-up
  "bk" 'buf-move-down
  "bl" 'buf-move-right
  "br" 'read-only-mode
  "bu" (lambda () (interactive) (revert-buffer nil t))
  "bw" 'save-buffer
  )

;; Window bindings

;; | Binding     | Call                        | Do                                                                  |
;; |-------------+-----------------------------+---------------------------------------------------------------------|
;; | <leader>-wd | delete-window               | Close a window                                                      |
;; | <leader>-wv | split-window-horizontally   | Split the selected window into two side-by-side windows             |
;; | <leader>-ws | split-window-vertically     | Split the selected window into two windows, one above the other     |
;; | <leader>-wz | delete-other-windows        | Make a Zoom (delete all the other windows)                          |
;; | <leader>-wj | windmove-down               | Move the window to the below position                               |
;; | <leader>-wk | windmove-up                 | Move the window to the above position                               |
;; | <leader>-wh | windmove-left               | Move the window to the left position                                |
;; | <leader>-wl | windmove-right              | Move the window to the right position                               |
;; | <leader>-wJ | shrink-window               | Shrink the window                                                   |
;; | <leader>-wK | enlarge-window              | Enlarge the window                                                  |
;; | <leader>-wH | shrink-window-horizontally  | Shrink the window horizontally                                      |
;; | <leader>-wL | enlarge-window-horizontally | Enlarge the window horizontally                                     |
;; | <leader>-ww | other-window                | Select other window in cycling order                                |
;; | <leader>-wr | winner-redo                 | Restore a more recent window configuration saved by Winner mode     |
;; | <leader>-wu | winner-undo                 | Switch back to an earlier window configuration saved by Winner mode |

(require 'windmove)
(winner-mode t)
(evil-leader/set-key
  "wd" 'delete-window
  "wH" 'shrink-window-horizontally
  "wh" 'windmove-left
  "wJ" 'shrink-window
  "wj" 'windmove-down
  "wK" 'enlarge-window
  "wk" 'windmove-up
  "wL" 'enlarge-window-horizontally
  "wl" 'windmove-right
  "wr" 'winner-redo
  "ws" 'split-window-vertically
  "wu" 'winner-undo
  "wv" 'split-window-horizontally
  "ww" 'other-window
  "wz" 'delete-other-windows
  )

;; Menu bindings
    
;; | Binding     | Call                     | Do                                                           |
;; |-------------+--------------------------+--------------------------------------------------------------|
;; | <leader>-ms | smex                     | Call smex (to execute a command)                             |
;; | <leader>-mm | smex-major-mode-commands | Idem as above but limited to the current major mode commands |
;; | <leader>-mh | helm-M-x                 | Call Helm M-x                                              |

(evil-leader/set-key
  "mh" 'helm-M-x
  "mm" 'smex-major-mode-commands
  "ms" 'smex
  )

;; File bindings

;; | Binding     | Call            | Do                        |
;; |-------------+-----------------+---------------------------|
;; | <leader>-fo | find-file       | Open a file               |
;; | <leader>-fr | helm-recentf    | Open a recent opened file |
;; | <leader>-fh | helm-find-files | Open a file using helm    |
;; | <leader>-fd | dired           | Call dired                |

(evil-leader/set-key
  "fo" 'find-file
  "fr" 'helm-recentf
  "fh" 'helm-find-files
  "fd" 'dired
  )

;; Edition bindings

;; | Binding     | Call                                | Do                                       |
;; |-------------+-------------------------------------+------------------------------------------|
;; | M-t         | transpose-words                     | Transpose two words                      |
;; | <leader>-ea | align-regexp                        | Align a region using regex               |
;; | <leader>-ec | evilnc-comment-or-uncomment-lines   | Comment/Uncomment lines                  |
;; | <leader>-ei | fci-mode                            | Show/hide fill column                    |
;; | <leader>-ef | variable-pitch-mode                 | Toggle variable/fixed space font         |
;; | <leader>-eh | whitespace-mode                     | Show/Hide hidden chars                   |
;; | <leader>-ek | count-words                         | Count words in a region                  |
;; | <leader>-el | linum-mode                          | Show/Hide line numbers                   |
;; | <leader>-et | joe/toggle-show-trailing-whitespace | Show/Hide trailing whitespace            |
;; | <leader>-eu | undo-tree-visualize                 | Visualize the current buffer's undo tree |
;; | <leader>-ew | whitespace-cleanup                  | Remove trailing whitespaces              |

(evil-leader/set-key
  "ea" 'align-regexp
  "ec" 'evilnc-comment-or-uncomment-lines
  "ef" 'fci-mode
  "eh" 'whitespace-mode
  "el" 'linum-mode
  "et" 'joe/toggle-show-trailing-whitespace
  "eu" 'undo-tree-visualize
  "ev" 'variable-pitch-mode
  "ew" 'whitespace-cleanup
  )

;; Narrow bindings

;; | Binding     | Call             | Do                                                    |
;; |-------------+------------------+-------------------------------------------------------|
;; | <leader>-nr | narrow-to-region | Restrict editing in this buffer to the current region |
;; | <leader>-np | narrow-to-page   | Restrict editing to the visible page                  |
;; | <leader>-nf | narrow-to-defun  | Restrict editing to the current defun                 |
;; | <leader>-nw | widen            | Remove restrictions (narrowing) from current buffer   |
;; T

(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen
  )

;; Shell/System bindings

;; | Binding     | Call            | Do                            |
;; |-------------+-----------------+-------------------------------|
;; | <leader>-xt | multi-term      | Create new term buffer        |
;; | <leader>-xn | multi-term-next | Go to the next term buffer    |
;; | <leader>-xe | eshell          | Call eshell                   |
;; | <leader>-xp | proced          | Show a list of system process |
;; | <leader>-xl | list-process    | Show a list of emacs process |

(evil-leader/set-key
  "xt" 'multi-term
  "xn" 'multi-term-next
  "xe" 'eshell
  "xp" 'proced
  "xl" 'list-process
  )

;; Search bindings

;; | Binding     | Call                   | Do                                                    |
;; |-------------+------------------------+-------------------------------------------------------|
;; | <leader>-sa | ag                     | Do a regex search using ag (The Silver Searcher)      |
;; | <leader>-sg | rgrep                  | Do a regex search using grep                          |
;; | <leader>-sf | swoop                  | Search through words within the current buffer        |
;; | <leader>-sw | swoop-multi            | Search words across currently opened multiple buffers |
;; | <leader>-st | helm-semantic-or-imenu | See the file tags                                     |
;; | <leader>-sk | browse-kill-ring       | Choose between previous yanked pieces of text         |

(evil-leader/set-key
  "sa" 'ag
  "sg" 'rgrep
  "sf" 'swoop
  "sw" 'swoop-multi
  "st" 'helm-semantic-or-imenu
  "sk" 'browse-kill-ring
  )

;; Organization bindings

;; | Binding     | Call                  | Do                                   |
;; |-------------+-----------------------+--------------------------------------|
;; | <leader>-oa | org-agenda            | Call the org-mode agenda             |
;; | <leader>-oc | org-capture           | Call the org-mode capture            |
;; | <leader>-om | mu4e                  | Start mu4e (email client)            |
;; | <leader>-od | cfw:open-org-calendar | Open the month calendar for org-mode |
;; | <leader>-op | org-contacts          | Search a contact                     |

(evil-leader/set-key
  "oa" 'org-agenda
  "oc" 'org-capture
  "om" 'mu4e
  "od" 'cfw:open-org-calendar
  "op" 'org-contacts
  )

;; Exit bindings

;; | Binding     | Call                       | Do                                |
;; |-------------+----------------------------+-----------------------------------|
;; | <leader>-qc | save-buffers-kill-terminal | Exit Emacs (standalone or client) |
;; | <leader>-qs | save-buffers-kill-emacs    | Shutdown the emacs daemon         |

(evil-leader/set-key
  "qc" 'save-buffers-kill-terminal
  "qs" 'save-buffers-kill-emacs
  )

;; Git bindings

;; | Binding     | Call         | Do         |
;; |-------------+--------------+------------|
;; | <leader>-gs | magit-status | Call Magit |

(evil-leader/set-key
  "gs" 'magit-status
  )

;; Internet bindings

;; | Binding     | Call         | Do                                                |
;; |-------------+--------------+---------------------------------------------------|
;; | <leader>-is | helm-surfraw | Search the web using [[http://surfraw.alioth.debian.org/][Surfraw]]                      |
;; | <leader>-if | elfeed       | Open Elfeed to read Atom/RSS entries              |
;; | <leader>-it | twit         | Open twittering-mode for an interface for twitter |

(evil-leader/set-key
  "is" 'helm-surfraw
  "if" 'elfeed
  "it" 'twit
  )

;; Spell bindings

;; | Binding     | Call                     | Do                          |
;; |-------------+--------------------------+-----------------------------|
;; | <leader>-ks | ispell-change-dictionary | Change the spell dictionary |
;; |             |                          |                             |

(evil-leader/set-key
  "kd" 'ispell-change-dictionary
  "kk" 'flyspell-auto-correct-word
  "kc" 'ispell-word
  "kn" 'flyspell-goto-next-error
  )

;; Emacs bindings

;; | Binding     | Call                | Do                                |
;; |-------------+---------------------+-----------------------------------|
;; | <leader>-zt | helm-themes         | Change the color theme using helm |
;; | <leader>-zp | list-packages       | List all the available packages   |
;; | <leader>-zi | package-install     | Install a package                 |
;; | <leader>-zu | text-scale-increase | Increase the size of the text     |
;; | <leader>-zd | text-scale-decrease | Decrease the size of the text     |

(evil-leader/set-key
  "zt" 'helm-themes
  "zp" 'list-packages
  "zi" 'package-install
  "zu" 'text-scale-increase
  "zd" 'text-scale-decrease
  )

;; evil-indent-textobject

;; Textobject for evil based on indentation, [[https://github.com/cofi/evil-indent-textobject][repository]]

(require 'evil-indent-textobject)

;; evil-nerd-commenter

;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
;; [[https://github.com/redguardtoo/evil-nerd-commenter][Repository]]

(require 'evil-nerd-commenter)

;; evil-surround

;; Use the [[https://github.com/timcharper/evil-surround][Surround]] plugin, the equivalent to the Vim one.

(require 'surround)
(global-surround-mode 1)

;; evil-matchit

;; Use the [[https://github.com/redguardtoo/evil-matchit][Matchit]] plugin, the equivalent to the Vim one.

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; change cursor color depending on mode

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("lawn green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("deep sky blue" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Browser

;    (setq browse-url-browser-function 'w3m-browse-url)
;    (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
    (setq browse-url-browser-function 'browse-url-generic
           browse-url-generic-program "firefox")
    (setq w3m-default-display-inline-images t)

;; Enable Org Mode

(require 'org)

;; TODO Org-mode modules

;; Set the modules enabled by default

(setq org-modules '(
    org-bbdb
    org-bibtex
    org-docview
    org-mhe
    org-rmail
    org-w3m
    org-crypt
    org-protocol
    org-gnus
    org-info
    org-habit
    org-irc
    org-annotate-file
    org-eval
    org-expiry
    org-man
    org-panel
    org-toc
))

;; Set default directories

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Highlight code blocks syntax

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; Record date and time when a task is marked as DONE

(setq org-log-done t)

;; Detect idle time when clock is running

(setq org-clock-idle-time 10)

;; Include diary entries

(setq org-agenda-include-diary t)

;; Agenda files

(setq org-agenda-files '("~/org"))

;; Configure the external apps to open files

(setq org-file-apps
   '(("\\.pdf\\'" . "zathura %s")
     ("\\.gnumeric\\'" . "gnumeric %s")))

;; Protect hidden trees for being inadvertily edited

(setq org-catch-invisible-edits 'error)
(setq org-ctrl-k-protect-subtree 'error)

;; Show images inline

;; Only works in GUI, but is a nice feature to have

(when (window-system)
    (setq org-startup-with-inline-images t))

;; Limit images width

(setq org-image-actual-width '(800))

;; Org-Babel

;; [[http://orgmode.org/worg/org-contrib/babel/][Babel]] is Org-mode's ability to execute source code within Org-mode documents.

;; languages supported
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote (
        (calc . t)
        (clojure . t)
        (ditaa . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (latex . t)
        (ledger . t)
        (octave . t)
        (org . t)
        (makefile . t)
        (plantuml . t)
        (python . t)
        (R . t)
        (ruby . t)
        (sh . t)
        (sqlite . t)
        (sql . nil))))
(setq org-babel-python-command "python2")

;; Refresh images after execution

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Don't ask confirmation to execute "safe" languages

(defun joe/org-confirm-babel-evaluate (lang body)
            (and (not (string= lang "ditaa"))
                 (not (string= lang "dot"))
                 (not (string= lang "gnuplot"))
                 (not (string= lang "ledger"))
                 (not (string= lang "plantuml"))))
(setq org-confirm-babel-evaluate 'joe/org-confirm-babel-evaluate)

;; Org-location-google-maps

;; The google-maps Emacs extension allows to display Google Maps directly
;; inside Emacs and integrate them in org-mode as addresses.

(require 'google-maps)
(require 'org-location-google-maps)

;; Org-protocol

;; org-protocol intercepts calls from emacsclient to trigger custom
;; actions without external dependencies. Only one protocol has to be
;; configured with your external applications or the operating system, to
;; trigger an arbitrary number of custom actions.

;; To use it to capture web urls and notes from Firefox, install this
;; [[http://chadok.info/firefox-org-capture/][Firefox extension]]

(require 'org-protocol)

(setq org-protocol-default-template-key "w")
(setq org-capture-templates
      (quote
       (("w" "Web captures" entry (file+headline "~/org/notes.org" "Web")
         "* %^{Title}    %^G\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1))))

;; Org-contacts

;; The org-contacts Emacs extension allows to manage your contacts using
;; Org-mode.

(require 'org-contacts)
(setq org-contacts-file "~/org/contacts.org")
(setq org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY")

(add-to-list 'org-capture-templates
  '("p" "Contacts" entry (file "~/org/contacts.org")
     "** %(org-contacts-template-name)
     :PROPERTIES:%(org-contacts-template-email)
     :END:"))

;; TODO Other captures

(add-to-list 'org-capture-templates
    '("t" "TODO" entry (file+headline "~/org/tasks.org" "Tasks")
       "* TODO %^{Task}  %^G\n   %?\n  %a"))
(add-to-list 'org-capture-templates
    '("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
       "* %^{Header}  %^G\n  %u\n\n  %?"))

;; Generic

(add-hook 'prog-mode-hook 'flycheck-mode)

;; Jedi

;; [[https://github.com/tkf/emacs-jedi][Jedi]] offers very nice auto completion for python-mode.

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Haskell

;; Haskell settings.

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Racket

;; Racket settings.

(setq racket-mode-pretty-lambda t)

;; Calfw

;; [[https://github.com/kiwanami/emacs-calfw][Calfw]] program displays a calendar view in the Emacs buffer.

;; [[file:img/cfw_calendar.png]]

(require 'calfw)
(require 'calfw-org)

;; Unicode chars for lines

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; Smex

;; [[https://github.com/nonsequitur/smex][Smex]] is a M-x enhancement for Emacs. Built on top of IDO, it provides
;; a convenient interface to your recently and most frequently used
;; commands. And to all the other commands, too.

;; | Binding | Call                     | Do                                                           |
;; |---------+--------------------------+--------------------------------------------------------------|
;; | M-x     | smex                     | Calls a interactive command using smex                       |
;; | M-X     | smex-major-mode-commands | Idem as above but limited to the current major mode commands |

(require 'smex)

;; Set cache file

;; Smex keeps a file to save its state betweens Emacs sessions.
;; The default path is =~/.smex-items=

(setq smex-save-file "~/.emacs.d/tmp/smex-items")

;; Useful bindings & Delayed Initation

;; #+BEGIN_QUOTE
;; I install smex with the following code to make emacs startup a little
;; faster.  This delays initializing smex until it's needed. IMO, smex
;; should load without this hack.

;; Just have smex call =smex-initialize= when it's needed instead of
;; having the user do it. --[[http://www.emacswiki.org/emacs/Smex][LeWang on EmacsWiki]]
;; #+END_QUOTE

(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;; set cache file

(setq ido-save-directory-list-file "~/.emacs.d/tmp/ido.last")

;; enable Ido

(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; Ido-ubiquitous

;; Gimme some ido... everywhere!

;; Does what you expected ido-everywhere to do.

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

;; Ido-vertical-mode

;; Makes ido-mode display vertically.

(require 'ido-vertical-mode)
(ido-vertical-mode t)

;; Ido for yes or no questions

(require 'ido-yes-or-no)
(ido-yes-or-no-mode t)

;; Magit

;; With [[https://github.com/magit/magit][Magit]], you can inspect and modify your Git repositories with
;; Emacs. You can review and commit the changes you have made to the
;; tracked files, for example, and you can browse the history of past
;; changes. There is support for cherry picking, reverting, merging,
;; rebasing, and other common Git operations.

(require 'magit)

;; Helm Surfraw (helm-net)

;; Set the default engine as searching on DuckDuckGo with a bang =!= by
;; default.

(setq helm-surfraw-duckduckgo-url "https://duckduckgo.com/lite/?q=!%s&kp=1")

;; Browse Kill Ring

(require 'browse-kill-ring)

;; TODO Charmap

;; [[https://github.com/lateau/charmap][Charmap]] is unicode table viewer for Emacs. With CharMap you can see
;; the unicode table based on The Unicode Standard 6.2.

(load-library "charmap")
(setq charmap-text-scale-adjust 2)

;; Swoop

(require 'swoop)
(setq swoop-font-size-change: nil)

;; Multi Term

(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Yasnippet

;; [[https://github.com/capitaomorte/yasnippet][YASnippet]] is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.

(require 'yasnippet)
(yas-global-mode)

;; Disable it in ansi-term

(add-hook 'after-change-major-mode-hook
      (lambda ()
        (when (find major-mode
            '(term-mode ansi-term))
          (yas-minor-mode 0))))

;; Ag

;; A simple ag frontend, loosely based on ack-and-half.el.

(require 'ag)
(setq ag-reuse-buffers 't)
(setq ag-highlight-search t)
(setq ag-arguments
    (list "--color" "--smart-case" "--nogroup" "--column" "--all-types" "--"))

;; Diff-hl

;; [[https://github.com/dgutov/diff-hl][diff-hl]] highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.

(require 'diff-hl)
(add-hook 'org-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; TODO Popwin

;; [[https://github.com/m2ym/popwin-el][Popwin]] is a popup window manager for Emacs which makes you free from
;; the hell of annoying buffers such like *Help*, *Completions*,
;; *compilation*, and etc.

;; + [ ] Test integration with w3m

(require 'popwin)
    (popwin-mode 1)
;    (require 'popwin-w3m)
;    (setq browse-url-browser-function 'popwin:w3m-browse-url)

;; Enable mu4e

(require 'mu4e)

;; Use encryption

;; Use encryption to protect the sensitive data like the servers configuration
;; (stored in =authinfo.gpg=) and the sensitive user's infomation.

(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;; User information

;; Sets the default user's information properly.

(setq user-full-name "joe di castro"
      user-mail-address "joe@joedicastro.com")

;; First load the user's sensitive information

;; This load the =mail.el.gpg= file where I store the email information
;; that I prefer to keep private (since I share this config in GitHub) to
;; inquisitive eyes.

(add-hook 'mu4e-main-mode-hook (lambda ()
    (load-library (concat user-emacs-directory "mail.el.gpg"))))

;; The rest of the SMTP configuration

;; This is the config needed to choose the right smtp configuration for
;; the proper account in each moment (for new messages, replies, forwards
;; & drafts editing).

;; set a stronger TLS configuration than the default to avoid nasty
;; warnings and for a little more secure configuration
(setq gnutls-min-prime-bits 2048)

;; the multiple functions that provide the multiple accounts selection functionality
(defun joe/mu4e-choose-account ()
    (completing-read (format "Compose with account: (%s) "
      (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
          (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                              nil t nil nil (caar my-mu4e-account-alist)))

(defun joe/mu4e-get-field (a)
    (let ((field (cdar (mu4e-message-field mu4e-compose-parent-message a))))
        (string-match "@\\(.*\\)\\..*" field)
        (match-string 1 field)))


(defun joe/mu4e-is-not-draft ()
    (let ((maildir (mu4e-message-field (mu4e-message-at-point) :maildir)))
       (if (string-match "drafts*" maildir)
              nil
              t)))

(defun joe/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
            (let ((field (if (joe/mu4e-is-not-draft)
                            (joe/mu4e-get-field :to)
                            (joe/mu4e-get-field :from))))
                (if (assoc field my-mu4e-account-alist)
                    field
                    (joe/mu4e-choose-account)))
            (joe/mu4e-choose-account)))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'joe/mu4e-set-account)

;; Queuing emails

;; When offline or when you want do delay a message, you can go to the
;; queuing mode and send them all turning it off.

;; Allow queuing mails
(setq smtpmail-queue-mail nil ;; start in non-queuing mode
    smtpmail-queue-dir "~/org/mail/mails/Queue")

;; Signature

;; Add the signature by default when a new email is composed.

(setq mu4e-compose-signature-auto-include t)
(setq
        message-signature t
        mu4e-compose-signature t)

;; Sending emails asynchronous

;; This is useful to send emails with attachments and do not block emacs
;; until end the transmission.

(require 'smtpmail-async)
(setq
    send-mail-function 'async-smtpmail-send-it
    message-send-mail-function 'async-smtpmail-send-it)

;; maildirs extension

;; [[https://github.com/agpchil/mu4e-maildirs-extension][Mu4e maildirs extension]] adds a maildir summary in mu4e-main-view.

(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-maildir-separator "*")
(setq mu4e-maildirs-extension-submaildir-separator "✉")
(setq mu4e-maildirs-extension-action-text nil)

;; Setup maildir & folders

;; The default Maildir path and subfolders.

(setq
    mu4e-maildir       "~/org/mail"        ;; top-level Maildir
    mu4e-sent-folder   "/mails/Sent"       ;; folder for sent messages
    mu4e-drafts-folder "/mails/Drafts"     ;; unfinished messages
    mu4e-trash-folder  "/mails/Trash"      ;; trashed messages
    mu4e-refile-folder "/mails/Archive")   ;; saved messages

;; where store the saved attachments
(setq mu4e-attachment-dir  "~/descargas")

;; General Options

;; mu4e's general options.

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; do not ask for confirmation on exit
(setq mu4e-confirm-quit  nil)

;; set mu4e as the default emacs email client
(setq mail-user-agent 'mu4e-user-agent)

;; decorate mu main view
(defun joe/mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
      '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'joe/mu4e-main-mode-font-lock-rules)

;; attempt to automatically retrieve public keys when needed
(setq mu4e-auto-retrieve-keys t)

;; don't reply to myself
(setq mu4e-compose-dont-reply-to-self t)

;; only personal messages get in the address book
(setq mu4e-compose-complete-only-personal t)

;; org-mode integration

;; Integrate with org-mode

(require 'org-mu4e)
(setq org-mu4e-convert-to-html t)
(defalias 'org-mail 'org-mu4e-compose-org-mode)

;; Updating the email

;; Update the index every 2 minutes but don't retrieve the email via
;; Emacs.

(setq
  mu4e-get-mail-command "true" ;; or fetchmail, or ...
  mu4e-update-interval 120)    ;; update every 2 minutes

;; Header's view config

;; The headers view configuration.

;; more cool and practical than the default
(setq mu4e-headers-from-or-to-prefix '("" . "➜ "))
;; to list a lot of mails, more than the default 500
;; is reasonable fast, so why not?
(setq mu4e-headers-results-limit 750)
;; columns to show
(setq mu4e-headers-fields
    '(
      (:human-date . 9)
      (:flags . 6)
      (:mailing-list . 10)
      (:size . 6)
      (:from-or-to . 22)
      (:subject)))

;; Message view config

;; Config for view mode.

;; visible fields
(setq mu4e-view-fields
    '(
        :from
        :to
        :cc
        :bcc
        :subject
        :flags
        :date
        :maildir
        :mailing-list
        :tags
        :attachments
        :signature
))

;; program to convert to pdf
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")

;; view email addresses not only the name
(setq mu4e-view-show-addresses t)

;; attempt to show images when viewing messages
(setq
   mu4e-view-show-images t
   mu4e-view-image-max-width 800)

;; use imagemagick if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Editor view config

;; The editor view configuration.

;; kill the buffer when is no needed any more
(setq message-kill-buffer-on-exit t)

;; set the text width and activate the spell checking
(add-hook 'mu4e-compose-mode-hook (lambda ()
                                    (set-fill-column 80)
                                    (flyspell-mode)))

;; Message view actions

;; Define actions for message view.

;; add the action to open an HTML message in the browser
(add-to-list 'mu4e-view-actions
  '("browse mail" . mu4e-action-view-in-browser) t)

;; add the action to retag messages
(add-to-list 'mu4e-view-actions
  '("retag mail" . mu4e-action-retag-message) t)

;;Search for messages sent by the sender of the message at point
(defun joe/search-for-sender (msg)
    (mu4e-headers-search
        (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; define 'x' as the shortcut
(add-to-list 'mu4e-view-actions
    '("xsearch for sender" . joe/search-for-sender) t)

;; integration with org-contacts
(setq mu4e-org-contacts-file "~/org/contacts.org")

(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)


;; get a pgp key from a message
;; from  http://hugoduncan.org/post/snarf-pgp-keys-in-emacs-mu4e/
(defun joe/mu4e-view-snarf-pgp-key (&optional msg)
  "get the pgp key for the specified message."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
          (path (mu4e-message-field msg :path))
          (cmd (format "%s verify --verbose %s"
                 mu4e-mu-binary
                 (shell-quote-argument path)))
          (output (shell-command-to-string cmd)))
    (let ((case-fold-search nil))
      (when (string-match "key:\\([A-F0-9]+\\)" output)
        (let* ((cmd (format "%s --recv %s"
                            epg-gpg-program (match-string 1 output)))
               (output (shell-command-to-string cmd)))
          (message output))))))

(add-to-list 'mu4e-view-actions
             '("get PGP keys" . joe/mu4e-view-snarf-pgp-key) t)

;; Deal with HTML messages

;; Try to visualize as best as possible the HTML messages in text mode.

(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "w3m -dump -cols 110 -T text/html")

;; autosmiley.el

;; [[http://www.emacswiki.org/emacs/autosmiley.el][autosmiley.el]] by Damyan Pepper

(require 'smiley)

(defun autosmiley-overlay-p (overlay)
  "Return whether OVERLAY is an overlay of autosmiley mode."
  (memq (overlay-get overlay 'category)
        '(autosmiley)))

(defun autosmiley-remove-smileys (beg end)
  (dolist (o (overlays-in beg end))
    (when (autosmiley-overlay-p o)
      (delete-overlay o))))

(defvar *autosmiley-counter* 0
  "Each smiley needs to have a unique display string otherwise
  adjacent smileys will be merged into a single image.  So we put
  a counter on each one to make them unique")

(defun autosmiley-add-smiley (beg end image)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category 'autosmiley)
    (overlay-put overlay 'display (append image (list :counter (incf *autosmiley-counter*))))))


(defun autosmiley-add-smileys (beg end)
  (save-excursion
    (dolist (entry smiley-cached-regexp-alist)
      (let ((regexp (car entry))
            (group (nth 1 entry))
            (image (nth 2 entry)))
        (when image
          (goto-char beg)
          (while (re-search-forward regexp end t)
            (autosmiley-add-smiley (match-beginning group) (match-end group) image)))))))


(defun autosmiley-change (beg end &optional old-len)
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
        (end-line (save-excursion (goto-char end) (line-end-position))))
    (autosmiley-remove-smileys beg-line end-line)
    (autosmiley-add-smileys beg-line end-line)))


;;;###autoload
(define-minor-mode autosmiley-mode
  "Minor mode for automatically replacing smileys in text with
cute little graphical smileys."
  :group 'autosmiley :lighter " :)"
  (save-excursion
    (save-restriction
      (widen)
      (autosmiley-remove-smileys (point-min) (point-max))
      (if autosmiley-mode
          (progn
            (unless smiley-cached-regexp-alist
              (smiley-update-cache))
            (jit-lock-register 'autosmiley-change))
        (jit-lock-unregister 'autosmiley-change)))))

;; Use gnome emoticons

;; Seen [[https://github.com/ahilsend/dotfiles/blob/3b9756a4f544403b7013bff80245df1b37feecec/.emacs.d/rc/rc-smiley.el][here]]

(setq
    smiley-data-directory "/usr/share/icons/gnome/22x22/emotes/"
    smiley-regexp-alist '(("\\(:-?)\\)\\W" 1 "face-smile")
                          ("\\(;-?)\\)\\W" 1 "face-wink")
                          ("\\(:-|\\)\\W" 1 "face-plain")
                          ("\\(:-?/\\)[^/]\\W" 1 "face-uncertain")
                          ("\\(;-?/\\)\\W" 1 "face-smirk")
                          ("\\(:-?(\\)\\W" 1 "face-sad")
                          ("\\(:,-?(\\)\\W" 1 "face-crying")
                          ("\\(:-?D\\)\\W" 1 "face-laugh")
                          ("\\(:-?P\\)\\W" 1 "face-raspberry")
                          ("\\(8-)\\)\\W" 1 "face-cool")
                          ("\\(:-?\\$\\)\\W" 1 "face-embarrassed")
                          ("\\(:-?O\\)\\W" 1 "face-surprise")))
(add-to-list 'gnus-smiley-file-types "png")

;; View emoticons in mu4e

;; Show Smileys
(add-hook 'mu4e-view-mode-hook 'autosmiley-mode)
;; Test smileys:  :-] :-o :-) ;-) :-\ :-| :-d :-P 8-| :-(

;; Bookmarks

;; My bookmarks

(add-to-list 'mu4e-bookmarks
             '("flag:flagged" "Flagged" ?f))
(add-to-list 'mu4e-bookmarks
             '("date:48h..now" "Last 2 days" ?l))
(add-to-list 'mu4e-bookmarks
             '("date:1h..now" "Last hour" ?h))
(add-to-list 'mu4e-bookmarks
             '("flag:attach" "With attachments" ?a) t)
(add-to-list 'mu4e-bookmarks
             '("mime:application/pdf" "With documents" ?d) t)
(add-to-list 'mu4e-bookmarks
             '("size:3M..500M" "Big messages" ?b) t)

;; Shortcuts

;; My defined shortcuts

;; Folder shortcuts
(setq mu4e-maildir-shortcuts
  '(
    ("/mails/Archive" . ?a)
    ("/mails/business" . ?b)
    ("/mails/Drafts" . ?d)
    ("/mails/education" . ?e)
    ("/mails/Inbox" . ?i)
    ("/mails/joedicastro" . ?j)
    ("/mails/lists" . ?l)
    ("/mails/Local" . ?h)
    ("/mails/motley" . ?m)
    ("/mails/publicity" . ?p)
    ("/mails/Sent" . ?s)
    ("/mails/Spam" . ?x)
    ("/mails/Trash" . ?t)
    ("/mails/work" . ?w)))

;; Dired integration

;; Integration with Dired, so we can attach a file to a new email
;; directly from Dired.

;; | Binding     | Call              | Do                           |
;; |-------------+-------------------+------------------------------|
;; | C-c RET C-a | gnus-dired-attach | Attach a file to a new email |

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Encrypt/Decrypt

;; Config for encrypt/decrypt emails

(setq mu4e-decryption-policy t)

;; ; Sign the messages by default
;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
;; ;rename to signature.asc
;; (defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
;;   (save-excursion
;;     (search-backward "Content-Type: application/pgp-signature")
;;     (goto-char (point-at-eol))
;;     (insert "; name=\"signature.asc\"; description=\"Digital signature\"")))

;; Attachment reminder

;; To avoid memory faults, as forget to attach a file after mention it
;; in the message's body.

;; simple regexp used to check the message. Tweak to your own need.
(defvar joe-message-attachment-regexp "\\(adjunto\\|attach\\)")
;; the function that checks the message
(defun joe/message-check-attachment nil
  "Check if there is an attachment in the message if I claim it."
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp joe-message-attachment-regexp nil t nil)
      (message-goto-body)
      (unless (or (search-forward "<#part" nil t nil)
        (message-y-or-n-p
   "No attachment. Send the message ?" nil nil))
  (error "No message sent")))))
  ;; check is done just before sending the message
  (add-hook 'message-send-hook 'joe/message-check-attachment)

;; Open a mu4e search in a new frame

;; This is useful when you are composing a new email and need to do a
;; search in your emails to get a little context in the conversation.

(defun joe/mu4e-headers-search-in-new-frame
    (&optional expr prompt edit ignore-history)
        "Execute `mu4e-headers-search' in a new frame."
        (interactive)
        (select-frame (make-frame))
        (mu4e-headers-search expr prompt edit ignore-history))

;; Elfeed

;; [[https://github.com/skeeto/elfeed][Elfeed]] is an extensible web feed reader for Emacs, supporting both
;; Atom and RSS

;; *Search mode*

;; [[file:img/elfeed.png]]

;; | Binding | Call                           | Do                                          |
;; |---------+--------------------------------+---------------------------------------------|
;; | q       | quit-window                    | exit                                        |
;; | a       | elfeed-search-update--force    | refresh view of the feed listing            |
;; | A       | elfeed-update                  | fetch feed updates from the servers         |
;; | s       | elfeed-search-live-filter      | update the search filter (date & tags)      |
;; | RET     | elfeed-search-show-entry       | view selected entry in a buffer             |
;; | o       | elfeed-search-browse-url       | open selected entries in your browser       |
;; | y       | elfeed-search-yank             | copy selected entries URL to the clipboard  |
;; | r       | elfeed-search-untag-all-unread | mark selected entries as read               |
;; | u       | elfeed-search-tag-all-unread   | mark selected entries as unread             |
;; | +       | elfeed-search-tag-all          | add a specific tag to selected entries      |
;; | -       | elfeed-search-untag-all        | remove a specific tag from selected entries |
;; | E       |                                | open the feed urls file                     |

;; *Show mode*

;; [[file:img/elfeed_show.png]]
;; | Binding | Call                | Do                                  |
;; |---------+---------------------+-------------------------------------|
;; | q       | elfeed-kill-buffer  | exit the entry                      |
;; | g       | elfeed-show-refresh | refresh the entry                   |
;; | n       | elfeed-show-next    | go to the next entry                |
;; | p       | elfeed-show-prev    | go to the previous entry            |
;; | o       | elfeed-show-visit   | open the entry in your browser      |
;; | y       | elfeed-show-yank    | copy the entry URL to the clipboard |
;; | u       |                     | mark the entry as unread            |
;; | +       | elfeed-show-tag     | add tag to the entry                |
;; | -       | elfeed-show-untag   | remove tag from the entry           |
;; | SPC     | scroll-up           | scroll up the buffer                |
;; | S-SPC   | scroll-down         | scroll down the buffer              |

(require 'elfeed)

; Load the feeds file
(load "~/.emacs.d/elfeed.el.gpg")

; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
        (elfeed-make-tagger :before "2 weeks ago"
                            :remove 'unread))

(setq elfeed-db-directory "~/.emacs.d/tmp/elfeed")
(setq elfeed-search-filter "@2-days-old +unread ")

(setq elfeed-search-title-max-width 100)

;; Twittering-mode

;; [[https://github.com/kiwanami/emacs-calfw][Twittering-mode]] enables you to twit on Emacsen.

;; [[file:img/twittering_mode.png]]

;; | Binding   | Call                                           | Do                                    |
;; |-----------+------------------------------------------------+---------------------------------------|
;; | q         | twittering-kill-buffer                         | Kill buffer                           |
;; | Q         | twittering-edit-mode                           | Edit mode                             |
;; | j         | twittering-goto-next-status                    | Next Twitter                          |
;; | k         | twittering-goto-previous-status                | Previous Twitter                      |
;; | h         | twittering-switch-to-next-timeline             | Next Timeline                         |
;; | l         | twittering-switch-to-previous-timeline         | Previous Timeline                     |
;; | g         | beginning-of-buffer                            | Top of the Timeline                   |
;; | G         | end-of-buffer                                  | Bottom of the Timeline                |
;; | t         | twittering-update-status-interactive           | Post a tweet                          |
;; | X         | twittering-delete-status                       | Delete a own tweet                    |
;; | RET       | twittering-reply-to-user                       | Reply to user                         |
;; | r         | twittering-native-retweet                      | Retweet                               |
;; | R         | twittering-organic-retweet                     | Retweet & Edit                        |
;; | k         | twittering-direct-message                      | Direct Message                        |
;; | u         | twittering-current-timeline                    | Update Timeline                       |
;; | b         | twittering-favorite                            | Mark as Favorite                      |
;; | B         | twittering-unfavorite                          | Unmark as Favorite                    |
;; | f         | twittering-follow                              | Follow current user                   |
;; | F         | twittering-unfollow                            | Unfollow current user                 |
;; | i         | twittering-view-user-page                      | View user profile (Browser)           |
;; | /         | twittering-search                              | Search                                |
;; | .         | twittering-visit-timeline                      | Open a new Timeline                   |
;; | @         | twittering-other-user-timeline                 | Open the Timeline of the current user |
;; | T         | twittering-toggle-or-retrieve-replied-statuses | Show Thread                           |
;; | o         | twittering-click                               | Open item in a Browser                |
;; | TAB       | twittering-goto-next-thing                     | Go to next item                       |
;; | <backtab> | twittering-goto-previous-thing                 | Go to previous item                   |
;; | n         | twittering-goto-next-status-of-user            | Go to next current user's tweet       |
;; | p         | twittering-goto-previous-status-of-user        | Go to previous current user's tweet   |
;; | SPC       | twittering-scroll-up                           | Timeline scroll up                    |
;; | S-SPC     | twittering-scroll-down                         | Timeline scroll down                  |
;; | y         | twittering-push-uri-onto-kill-ring             | Yank current url                      |
;; | Y         | twittering-push-tweet-onto-kill-ring           | Yank current tweet                    |
;; | a         | twittering-toggle-activate-buffer              | Toggle Active Timeline                |

(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-use-icon-storage t)
(setq twittering-icon-storage-file "~/.emacs.d/tmp/twittering-mode-icons.gz")
(setq twittering-convert-fix-size 52)
(setq twittering-initial-timeline-spec-string
      '(":home"))
(setq twittering-edit-skeleton 'inherit-any)
(setq twittering-display-remaining t)
(setq twittering-status-format
    "%i  %S, %RT{%FACE[bold]{%S}} %@  %FACE[shadow]{%p%f%L%r}\n%FOLD[        ]{%T}\n")

;; Define my own bindings (based in [[https://github.com/alejandrogomez/turses][Turses]] style)

; remove the current bindings
(eval-after-load "twittering-mode"
    '(setq twittering-mode-map (make-sparse-keymap)))
; set the new bindings
(add-hook 'twittering-mode-hook
         (lambda ()
           (mapc (lambda (pair)
                   (let ((key (car pair))
                         (func (cdr pair)))
                     (define-key twittering-mode-map
                       (read-kbd-macro key) func)))
                 '(
                   ("q" . twittering-kill-buffer)
                   ("Q" . twittering-edit-mode)
                   ("j" . twittering-goto-next-status)
                   ("k" . twittering-goto-previous-status)
                   ("h" . twittering-switch-to-next-timeline)
                   ("l" . twittering-switch-to-previous-timeline)
                   ("g" . beginning-of-buffer)
                   ("G" . end-of-buffer)
                   ("t" . twittering-update-status-interactive)
                   ("X" . twittering-delete-status)
                   ("RET" . twittering-reply-to-user)
                   ("r" . twittering-native-retweet)
                   ("R" . twittering-organic-retweet)
                   ("d" . twittering-direct-message)
                   ("u" . twittering-current-timeline)
                   ("b" . twittering-favorite)
                   ("B" . twittering-unfavorite)
                   ("f" . twittering-follow)
                   ("F" . twittering-unfollow)
                   ("i" . twittering-view-user-page)
                   ("/" . twittering-search)
                   ("." . twittering-visit-timeline)
                   ("@" . twittering-other-user-timeline)
                   ("T" . twittering-toggle-or-retrieve-replied-statuses)
                   ("o" . twittering-click)
                   ("TAB" . twittering-goto-next-thing)
                   ("<backtab>" . twittering-goto-previous-thing)
                   ("n" . twittering-goto-next-status-of-user)
                   ("p" . twittering-goto-previous-status-of-user)
                   ("SPC" . twittering-scroll-up)
                   ("S-SPC" . twittering-scroll-down)
                   ("y" . twittering-push-uri-onto-kill-ring)
                   ("Y" . twittering-push-tweet-onto-kill-ring)
                   ("a" . twittering-toggle-activate-buffer)
                  ))))

;; Spell checking on tweets

(add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode)))

;; Dired+

;; Reuse the same buffer for directories

(diredp-toggle-find-file-reuse-dir 1)
