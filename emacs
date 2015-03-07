;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)

(defvar brent-home
  (cond ((or (string= system-name "archimedes")
             (string= system-name "hippasus")
	 )
	 "/home/brent/")
	((string= system-name "eudoxus") "/Users/brent/")
        (t "/home1/b/byorgey/")))

;; Custom load path
(add-to-list 'load-path (expand-file-name "~/local/lib/emacs"))
(add-to-list 'load-path (expand-file-name "~/local/share/emacs/site-lisp"))

;; Enable some commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Turn off menus and such
(tool-bar-mode 0)
(menu-bar-mode 0)

; get rid of annoying splash screen when started with command line args.
(when (> (length command-line-args) 1)
  (setq inhibit-splash-screen t))

; use ido mode
(require 'ido)

; saving macros
(defun save-macro (name)
  "save a macro. Take a name as argument
   and save the last defined macro under
   this name at the end of your .emacs"
   (interactive "SName of the macro: ")  ; ask for the name of the macro
   (kmacro-name-last-macro name)         ; use this name for the macro
   (find-file "~/.emacs")                   ; open ~/.emacs or other user init file
   (goto-char (point-max))               ; go to the end of the .emacs
   (newline)                             ; insert a newline
   (insert-kbd-macro name)               ; copy the macro
   (newline)                             ; insert a newline
   (save-buffer)
   (switch-to-buffer nil))               ; return to the initial buffer

; toggle vertical/horizontal split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(add-hook 'emacs-startup-hook 'toggle-window-split)

;; Toggle window dedication

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)

  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
       "Window '%s' is normal")
   (current-buffer)))

; zap-up-to-char

(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
    'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

; touch
; taken from http://stackoverflow.com/questions/8989540/touch-current-file-in-emacs

(defun touch ()
    "updates mtime on the file for the current buffer"
    (interactive)
    (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
    (clear-visited-file-modtime))

; copy-line
; http://emacswiki.org/emacs/CopyingWholeLines

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "C-c C-k") 'copy-line)

; Mac-specific stuff

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete

; (require 'auto-complete)
; (add-to-list 'ac-modes 'java-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet

(require 'yasnippet)
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-dropdown-prompt yas-ido-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-compile

(require 'smart-compile)
(add-to-list 'smart-compile-alist '("\\.java$" . "javac %f"))
(add-to-list 'smart-compile-alist '("\\.tex$" . "pdflatex %f"))
(add-to-list 'smart-compile-alist '("\\.lhs$" . "runhaskell Shake"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow-delimiters

(require 'rainbow-delimiters)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ott

;; (add-to-list 'load-path (expand-file-name "~/local/lib/ott_distro_0.10.16/emacs"))
;; (require 'ottmode)
;; (add-to-list 'auto-mode-alist '("\.ott$" . ott-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; (require 'markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX

(load "auctex.el" nil t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

(require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list (quote ((38 "land") (124 "lor") (right "Rightarrow") (up "iff") (64 "aleph") (49 "preceq") (50 "succeq") (51 "cong") (61 "equiv") (95 "models") (118 "varphi") (37 "emptyctx") (32 "sqrt") (! "neg"))))
 '(agda-input-user-translations (quote (("bB" "𝔹"))))
 '(agda2-include-dirs (quote ("." "/home/brent/local/share/agda-lib-0.8/src" "/Users/brent/local/share/agda-stdlib-0.9/src")))
 '(agda2-program-args (quote ("+RTS" "-K200M" "-H10G" "-M10G" "-RTS")))
 '(company-ghc-show-info t)
 '(compilation-read-command nil)
 '(darcsum-whatsnew-switches "-l")
 '(delete-selection-mode nil)
 '(face-font-family-alternatives (quote (("arial black" "arial" "DejaVu Sans") ("arial" "DejaVu Sans") ("courier" "Monospace") ("monaco" "Monospace") ("xiki" "verdana") ("verdana" "DejaVu Sans"))))
 '(font-lock-keywords-case-fold-search t t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-program-name "ghci \"+.\"")
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi$")))
 '(ido-mode (quote both) nil (ido))
 '(load-home-init-file t t)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/notes/")))
 '(perl-indent-level 2)
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace t)
 '(tex-dvi-view-command "xdvi -s 5")
 '(tex-start-commands "")
 '(tool-bar-mode nil)
 '(unicode-fonts-fallback-font-list (quote ("Symbola" "Quivira" "DejaVu Sans Mono")))
 '(whitespace-style (quote (face tabs trailing lines space-before-tab newline empty space-after-tab tab-mark)))
 '(writegood-weasel-words (quote ("many" "various" "very" "fairly" "several" "extremely" "exceedingly" "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny" "are a number" "is a number" "excellent" "interestingly" "significantly" "substantially" "clearly" "vast" "relatively" "completely" "literally" "not rocket science" "outside the box" "note that" "a number of" "trivial" "trivially" "not hard" "easy" "easily" "clear" "clearly" "obvious" "obviously"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom faces, font lock, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:bold t :foreground "ForestGreen"))))
 '(font-lock-comment-face ((((class color) (background light)) (:bold t :foreground "DarkOrchid4"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:bold t :foreground "DarkGreen"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Red"))))
 '(font-lock-type-face ((((class color) (background light)) (:italic t :foreground "Purple"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Green4")))))
(add-hook  'text-mode-hook
	   (function (lambda ()
		       (auto-fill-mode 1))))
(add-hook  'cc-mode-hook
	(function (lambda ()
		(font-lock-mode 1))))

(defvar font-lock-auto-mode-list
  (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode 'ruby-mode 'python-mode 'haskell-mode 'latex-mode 'agda2-mode 'ott-mode 'markdown-mode)
  "List of modes to always start in font-lock-mode")

(defvar font-lock-mode-keyword-alist
  '((c++-c-mode . c-font-lock-keywords)
    (perl-mode . perl-font-lock-keywords))
  "Associations between modes and keywords")

(defun font-lock-auto-mode-select ()
  "Automatically select font-lock-mode if the current major mode is
    in font-lock-auto-mode-list"
  (if (memq major-mode font-lock-auto-mode-list)
      (progn
        (font-lock-mode t))
    )
  )

(add-hook 'find-file-hooks 'font-lock-auto-mode-select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; built-in emacs functions
(global-set-key (kbd "C-c c") 'smart-compile)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x t") 'text-scale-increase)
(global-set-key (kbd "<f9>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c SPC") 'delete-horizontal-space-forward)

(global-set-key (kbd "C-x C-k c") 'BAY-comment)
(global-set-key (kbd "C-c n") 'note-other-window)

(global-set-key (kbd "<f2>") 'toggle-window-split) ;; misc emacs stuff @ top
(global-set-key (kbd "<f6>") 'toggle-stylish-on-save)
(global-set-key (kbd "<f7>") 'touch)

(global-set-key (kbd "M-<f11>") 'get-firefox-title)
(global-set-key (kbd "<f11>") 'get-firefox-url)
(global-set-key (kbd "M-<f12>") 'get-firefox-markdown-link)
(global-set-key (kbd "<f12>") 'get-firefox-org-link)  ;; mozrepl

(global-set-key (kbd "C-c e") 'journal-goto-end)    ;; org-mode
(global-set-key (kbd "C-c d") 'journal-add-today)   ;; org-mode
(global-set-key (kbd "C-c w") 'journal-count-words) ;; org-mode

(global-set-key (kbd "C-x w") 'darcsum-whatsnew)  ;; darcsum

(global-set-key (kbd "C-x g") 'magit-status)      ;; magit

(global-set-key (kbd "C-x y") 'typo-fix)

(global-set-key (kbd "C-c g") 'writegood-mode)    ;; writegood-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grading/feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; argh, don't remember exactly how this works.  Need to document it.
(fset 'note
   (lambda (&optional arg) "Insert a ##n note from the other frame." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 18 35 35 13 23 24 111 134217788 19 134217849 13 1 down 67108896 19 35 35 13 1 134217847 24 111 25 backspace] 0 "%d")) arg)))

(fset 'typo-fix
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 34 18 13 67108896 19 34 19 13 134217847 32 45 45 62 32 25] 0 "%d")) arg)))

(fset 'BAY-comment
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("-- *** BAY: " 0 "%d")) arg)))

;; Type a number and then execute this macro.  If the point is
;; following nn, it searches for #nn in the other window and replaces
;; nn in the current window with the result of C-x C-k c (currently
;; bound to BAY-comment) followed by the rest of the line following
;; #nn in the other window.
(fset 'note-other-window
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([M-left 67108896 5 23 24 111 134217788 19 35 25 13 right 67108896 5 134217847 24 111 24 11 99 25] 0 "%d")) arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Haskell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'turn-on-hi2)

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))



(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; hindent

(add-hook 'haskell-mode-hook #'hindent-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make undefined red

(font-lock-add-keywords 'haskell-mode
  '(("undefined" . font-lock-warning-face)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; insert LANGUAGE pragmas

(defun insert-language-pragma (pragma)
  "Insert a LANGUAGE pragma at the top of the file."
  (interactive "SPragma: ")  ; ask for the name of the pragma
  (let ((string  (format "{-# LANGUAGE %s #-}" pragma)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat string "\n")))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; delete whitespace forward

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; hpaste

;(when (not (string= system-name "ampersand.seas.upenn.edu"))
;  (load (expand-file-name "~/local/lib/emacs/hpaste/hpaste.el")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ghc-mod

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; company-ghc

(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)

(add-to-list 'company-backends 'company-ghc)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; toggle stylish-on-save

(defun toggle-stylish-on-save ()
  "Toggle haskell-stylish-on-save"
  (interactive)
  (setq haskell-stylish-on-save (if (eq haskell-stylish-on-save t) nil t))
  (message "Stylish-on-save is now %s." (if (eq haskell-stylish-on-save t) "on" "off"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

(fset 'journal-goto-end
   (lambda (&optional arg) "Collapse everything in my journal file except the last entry." (interactive "p") (kmacro-exec-ring-item (quote ([21 21 tab 134217790 3 16 tab 134217790 3 16 tab 134217790 3 16 tab] 0 "%d")) arg)))

(fset 'journal-add-today
   (lambda (&optional arg) "Add an entry for today." (interactive "p") (kmacro-exec-ring-item (quote ([21 21 tab 134217790 3 16 tab 134217790 3 16 tab 134217790 M-return 67108896 67108896 134217777 134217852 100 97 116 101 32 43 34 37 45 100 34 13] 0 "%d")) arg)))

(defun journal-count-words ()
  (interactive)
  (save-excursion
    (outline-previous-visible-heading 1)
    (next-line)
    (let ((beginning (point)))
      (outline-next-visible-heading 1)
      (message "%s" (count-words-region beginning (point))))))

;; (require 'ob-diagrams)

;; unused

;; (org-remember-insinuate)
;; (setq org-directory "~/notes/")
;; (setq org-default-notes-file (concat org-directory "journal.org"))
;; (define-key global-map "\C-cr" 'org-remember)

;; (setq org-remember-templates
;;       '(("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/notes/journal.org" date-tree)
;;         ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/JOURNAL.org" "New Ideas")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mozrepl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(when (not (string= system-name "ampersand.seas.upenn.edu"))
  (require 'moz))

(defun jk/moz-get (attr)
  (comint-send-string (inferior-moz-process) attr)
  ;; try to give the repl a chance to respond
  (sleep-for 0 100))

(defun jk/moz-get-current-url ()
  (interactive)
  (jk/moz-get "repl._workContext.content.location.href"))

(defun jk/moz-get-current-title ()
  (interactive)
  (jk/moz-get "repl._workContext.content.document.title"))

(defun jk/moz-get-current (moz-fun)
  (funcall moz-fun)
  ;; doesn't work if repl takes too long to output string
  (save-excursion
                  (set-buffer (process-buffer (inferior-moz-process)))
                  (goto-char (point-max))
                  (previous-line)
                  (setq jk/moz-current (buffer-substring-no-properties
                                        (+ (point-at-bol) (length moz-repl-name) 3)
                                        (- (point-at-eol) 1))))
  (message "%s" jk/moz-current)
  jk/moz-current
)

(defun jk/moz-url ()
  (interactive)
  (jk/moz-get-current 'jk/moz-get-current-url)
  )

(defun jk/moz-title ()
  (interactive)
  (jk/moz-get-current 'jk/moz-get-current-title)
  )

(defun get-firefox-title ()
  (interactive)
  (insert (jk/moz-title))
)

(defun get-firefox-url ()
  (interactive)
  (insert (jk/moz-url))
)

(defun get-firefox-markdown-link ()
  (interactive)
  (insert (concat "[" (jk/moz-title) "](" (jk/moz-url) ")"))
)

(defun get-firefox-org-link ()
  (interactive)
  (insert (concat "[[" (jk/moz-url) "][" (jk/moz-title) "]]"))
)

;; Templates used by the guy who wrote the above code.  Keeping it
;; here in case I ever want to use something like this.
;;
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/all.org" "Tasks")
;;          "* TODO %?\n %i\n %a")
;;         ("n" "Notes" entry (file+datetree "~/org/notes.org")
;;          "* %?\nEntered on %U\n %i\n %a")
;;         ("b" "Bookmark" entry (file+datetree "~/org/notes.org")
;;          "* %(concat \"[[\" (jk/moz-url) \"][\" (jk/moz-title) \"]]\")\n Entered on %U\n")
;;         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proof General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/local/lib/ProofGeneral/generic/proof-site.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (string= system-name "ampersand.seas.upenn.edu"))
  (load-file (let ((coding-system-for-read 'utf-8))
                  (shell-command-to-string "agda-mode locate"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Darcsum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (autoload 'darcsum-changes "darcsum.el" nil t)
; (autoload 'darcsum-whatsnew "darcsum.el" nil t)
; (autoload 'darcsum-view "darcsum.el" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (string= system-name "ampersand.seas.upenn.edu"))
  (require 'magit)
  (autoload 'magit-status "magit" nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'footnote-mode "footnote" nil t)
(add-hook 'mail-mode-hook 'footnote-mode)

(defun brent-mail-mode-hook ()
  (turn-on-auto-fill)
  (turn-on-font-lock)
  ; (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*")
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil)
)

(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))
(add-hook 'mail-mode-hook 'brent-mail-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyphens

;; (defun insert-hyphens ()
;;   (interactive)
;;   (let ((next-line-len
;;      (save-excursion
;;        (next-line)
;;        (move-beginning-of-line nil)
;;        (let ((here (point)))
;;          (move-end-of-line nil)
;;          (- (point) here)))))
;;     (insert-char ?- next-line-len)))
;;
;; (global-set-key [(f6)] 'insert-hyphens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whizzytex

; (autoload 'whizzytex-mode
;            "~/local/share/whizzytex/emacs/whizzytex"
;            "WhizzyTeX, a minor-mode WYSIWIG environment for LaTeX" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-latex-list

;; (defun latex-list (v i j op)
;;   "Write out a LaTeX list of the form x_1, \dots, x_n."
;;   (interactive "sVariable? \nsStart index: \nsEnd index: \nsOperator: ")
;;   (insert (format "%s_{%s} %s \\dots %s %s_{%s}" v i op op v j)))

;; (global-set-key [(f6)] 'latex-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings

;; (global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))
;; (global-set-key [(f2)] 'font-lock-mode)
;; (global-set-key [(f3)] 'auto-fill-mode)
;; (global-set-key [(f4)] 'hpaste-get-paste)
;; (global-set-key [(f5)] 'hpaste-paste-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference rules

;; (add-hook 'text-mode-hook '(lambda ()
;;   (require 'rules)
;;   (local-set-key "\C-c\C-r" 'rules-center-this-infrule)))
;; (setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))


;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
;(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;(require 'el4r)
;(el4r-boot)
;; End of the el4r block.
;; User-setting area is below this line.

