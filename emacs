;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; See https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto package installation

;; taken from http://y.tsutsumi.io/emacs-from-scratch-part-2-package-management.html

(defvar required-packages
  '(
    use-package
    magit
    yasnippet
    yasnippet-snippets
    java-snippets
    smart-compile
    rainbow-delimiters
    ;;moz
    attrap
    idris-mode
    darcsum
    company
    auto-complete
    markdown-mode
    request       ;; needed for beeminder
    seq           ;; needed for beeminder
    anaphora      ;; needed for beeminder
    synosaurus
    unicode-fonts
    writegood-mode
    auctex
    floobits
    tidal
    proof-general
    kotlin-mode
    polymode
    poly-markdown
    auto-yasnippet
    vimish-fold
    which-key
    dash
    f
    flycheck
    magit-section
    s
    thingatpt
    editorconfig
    ef-themes
    highlight-indentation
    ; Haskell
    lsp-mode
    lsp-ui
    lsp-ivy
    lsp-haskell
    haskell-snippets
   ) "a list of packages to ensure are installed at launch.")

; method to check if all packages are installed
(defun packages-installed-p ()
  (cl-loop for p in required-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Custom load path
(add-to-list 'load-path (expand-file-name "~/local/lib/emacs"))
(add-to-list 'load-path (expand-file-name "~/.local/lib/emacs"))
(add-to-list 'load-path (expand-file-name "~/local/share/emacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.local/share/emacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.local/lib/lean4-mode"))

;; A few other packages
(require 'beeminder)
(require 'swarm-mode)
(require 'lean4-mode)

;; ;; GitHub copilot

;; (use-package copilot
;;   :load-path (lambda () (expand-file-name "~/dotfiles/copilot.el"))
;;   ;; don't show in mode line
;;   :diminish)

;; ;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; (define-key copilot-mode-map (kbd "<backtab>") #'copilot-next-completion)
;; (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
;; (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)

;; (defun bay/copilot-tab ()
;;   "Tab command that will complete with copilot if a completion is
;; available. Otherwise will try normal tab-indent."
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (indent-for-tab-command)))

;; seems to do away with special meaning for tab in certain modes
;; (define-key global-map (kbd "<tab>") #'bay/copilot-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)

(defvar brent-home "/home/brent")

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

; (global-set-key (kbd "C-c C-k") 'copy-line)

; Align paragraph on =
(defun align-on-equals ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (align-regexp (point) (mark) "\\(\\s-*\\)=")))

; Mac-specific stuff

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

; from https://emacs.stackexchange.com/questions/12613/convert-the-first-character-to-uppercase-capital-letter-using-yasnippet
(defun my/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

; Incrementing the integer at pt, taken from https://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "C-c +") #'increment-integer-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicode-fonts

(unicode-fonts-setup)
; (set-frame-font "PragmataPro 12")   -- €19 for very basic version of the font

;; https://emacs.stackexchange.com/questions/251/line-height-with-unicode-characters
;; https://www.emacswiki.org/emacs/UnicodeFonts

(set-fontset-font t '(#x1d400 . #x1d7ff)
  ;; this should throw an error if there is no such font
  (font-xlfd-name (find-font (font-spec :family "Symbola"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete

; (require 'auto-complete)
; (add-to-list 'ac-modes 'java-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet

(require 'yasnippet)
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-dropdown-prompt yas-ido-prompt))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand-from-trigger-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-compile

(require 'smart-compile)
;; smart-compile-alist configured above in custom variables section

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

;; (use-package poly-markdown
;;   :ensure polymode
;;   :defer t
;;   :mode ("\\.md" . poly-markdown-mode))

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

(require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swarm

;; Attempted to define a poly-mode for Swarm within YAML.  It works;
;; the problem is that all Swarm chunks are sent to the Swarm LSP as
;; single, concatenated buffer, which leads to spurious type errors
;; all over the place.  Not sure how to work around this.

;; (use-package polymode
;;   :ensure t
;;   :mode ("\.yaml$" . poly-yaml-swarm-mode)
;;   :config

;;   (define-hostmode poly-yaml-hostmode :mode 'yaml-mode)

;;   (define-innermode poly-swarm-condition-yaml-innermode
;;     :mode 'swarm-mode
;;     :head-matcher "^  \\( \\|-\\) condition: |\n"
;;     :tail-matcher "^ \\{,4\\}[^ ]"
;;     :head-mode 'host
;;     :tail-mode 'host)

;;   (define-polymode poly-yaml-swarm-mode
;;     :hostmode 'poly-yaml-hostmode
;;     :innermodes '(poly-swarm-condition-yaml-innermode)))

(add-hook 'swarm-mode-hook #'lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CFDG mode

;; (require 'cfdg3-mode)
;; (autoload 'cfdg3-mode "cfdg3-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.cfdg\\'" . cfdg3-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   '((38 "land")
     (124 "lor")
     (right "Rightarrow")
     (up "iff")
     (64 "aleph")
     (49 "preceq")
     (50 "succeq")
     (51 "cong")
     (61 "equiv")
     (95 "models")
     (118 "varphi")
     (37 "emptyctx")
     (32 "sqrt")
     (33 "neg")
     (35 "frac")
     (36 "sum")
     (34 "dots")))
 '(agda-input-user-translations '(("bB" "𝔹")))
 '(agda2-include-dirs '("."))
 '(agda2-program-args
   (if
       (equal
        (system-name)
        "hypatia")
       '("-i" "." "+RTS" "-K200M" "-M8G" "-RTS")
     '("-i" "." "+RTS" "-K200M" "-M4G" "-RTS")))
 '(beeminder-auth-token "DXWqHnPzAkYStnxVc76s")
 '(beeminder-default-filter-days 2)
 '(beeminder-everyday-goals-list
   '(time-with-god work-journal jn dishes ac-liturgy morning itch))
 '(beeminder-username "byorgey")
 '(compilation-always-kill t)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(darcsum-whatsnew-switches "-l")
 '(delete-selection-mode nil)
 '(face-font-family-alternatives
   '(("arial black" "arial" "DejaVu Sans")
     ("arial" "DejaVu Sans")
     ("courier" "Monospace")
     ("monaco" "Monospace")
     ("xiki" "verdana")
     ("verdana" "DejaVu Sans")))
 '(font-lock-keywords-case-fold-search t t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-program-name "ghci \"+.\"")
 '(haskell-tags-on-save t)
 '(ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi$"))
 '(ido-mode 'both nil (ido))
 '(load-home-init-file t t)
 '(lsp-haskell-formatting-provider "fourmolu")
 '(lsp-haskell-plugin-fourmolu-config-external t)
 '(lsp-yaml-schema-store-enable nil)
 '(lsp-yaml-schemas
   '((/home/brent/projects/swarm/data/schema/scenario.json .
                                                           ["data/scenarios/**/*.yaml"])))
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/notes/"))
 '(package-selected-packages
   '(highlight-indentation gnu-elpa-keyring-update ef-themes rust-mode yaml-mode markdown-mode+ floobits anaphora writeroom-mode writegood-mode unicode-fonts synosaurus smart-compile seq scala-mode2 request rainbow-delimiters markdown-mode magit java-snippets idris-mode darcsum auto-complete))
 '(perl-indent-level 2)
 '(safe-local-variable-values
   '((lsp-haskell-formatting-provider . "fourmolu")
     (lsp-haskell-formatting-provider . "stylish-haskell")))
 '(scroll-bar-mode nil)
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/usr/bin/msmtp")
 '(show-trailing-whitespace t)
 '(smart-compile-alist
   '(("\\.lhs$" . "./build.sh")
     (emacs-lisp-mode emacs-lisp-byte-compile)
     (html-mode browse-url-of-buffer)
     (nxhtml-mode browse-url-of-buffer)
     (html-helper-mode browse-url-of-buffer)
     (octave-mode run-octave)
     ("\\.c$" . "gcc -O2 %f -lm -o %n")
     ("\\.[Cc]+[Pp]*$" . "g++ -O2 %f -lm -o %n")
     ("\\.m$" . "gcc -O2 %f -lobjc -lpthread -o %n")
     ("\\.java$" . "javac %f")
     ("\\.php$" . "php -l %f")
     ("\\.f90$" . "gfortran %f -o %n")
     ("\\.[Ff]$" . "gfortran %f -o %n")
     ("\\.cron\\(tab\\)?$" . "crontab %f")
     ("\\.tex$" . "rubber -d %f")
     ("\\.texi$" . "makeinfo %f")
     ("\\.mp$" . "mptopdf %f")
     ("\\.pl$" . "perl %f")
     ("\\.rb$" . "ruby %f")))
 '(tex-dvi-view-command "xdvi -s 5")
 '(tex-start-commands "")
 '(tool-bar-mode nil)
 '(unicode-fonts-block-font-mapping
   '(("Aegean Numbers"
      ("Noto Sans Symbols" "Aegean" "Symbola" "Quivira" "Code2001" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Alchemical Symbols"
      ("Noto Sans Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Alphabetic Presentation Forms"
      ("DejaVu Sans:width=condensed" "Arial Unicode MS" "Cardo" "Code2000" "Quivira" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Ancient Greek Numbers"
      ("Noto Sans Symbols" "Apple Symbols" "New Athena Unicode" "Cardo" "Aegean" "Quivira" "Symbola" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Ancient Symbols"
      ("Noto Sans Symbols" "Analecta" "New Athena Unicode" "Cardo" "Aegean" "Quivira" "Symbola" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Arabic Mathematical Alphabetic Symbols"
      ("Amiri"))
     ("Arrows"
      ("DejaVu Sans Mono" "Apple Symbols" "Cambria Math" "Segoe UI Symbol" "DejaVu Sans:width=condensed" "Arial Unicode MS" "BabelStone Modern" "Symbola" "Quivira" "Code2000" "Noto Sans Symbols" "Everson Mono:weight=bold" "FreeMono"))
     ("Block Elements"
      ("DejaVu Sans Mono" "Noto Sans Symbols" "FreeMono" "DejaVu Sans:width=condensed" "Apple Symbols" "Segoe UI Symbol" "BabelStone Modern" "Symbola" "Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Box Drawing"
      ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans" "Everson Mono" "Quivira" "Code2000" "Noto Sans Symbols" "Segoe UI Symbol" "Symbola"))
     ("Braille Patterns"
      ("Quivira" "Apple Braille" "DejaVu Sans:width=condensed" "Apple Symbols" "Segoe UI Symbol" "Symbola" "Noto Sans Symbols" "FreeMono" "Code2000" "Everson Mono:weight=bold"))
     ("Cherokee"
      ("Aboriginal Sans" "Aboriginal Serif" "Plantagenet Cherokee" "Noto Sans Cherokee" "Gadugi" "MPH 2B Damase" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Code2000"))
     ("Cherokee Supplement"
      ("Everson Mono:weight=bold"))
     ("CJK Compatibility"
      ("SimHei" "FangSong" "SimSun" "MingLiU" "Meiryo" "Microsoft JhengHei" "Microsoft JhengHei UI" "Lantinghei SC" "Lantinghei TC" "HAN NOM A" "Arial Unicode MS" "WenQuanYi Zen Hei Mono" "HanaMinA" "BabelStone Han" "Code2000"))
     ("CJK Compatibility Forms"
      ("WenQuanYi Zen Hei Mono" "Lantinghei SC" "SimHei" "FangSong" "SimSun" "LiSong Pro" "Baoli SC" "Microsoft YaHei" "Microsoft YaHei UI" "Lantinghei TC" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "Symbola" "Xingkai SC" "DFKai-SB" "Code2000"))
     ("CJK Compatibility Ideographs"
      ("SimHei" "FangSong" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "WenQuanYi Zen Hei Mono" "BabelStone Han" "UnBatang" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "Arial Unicode MS" "Lantinghei SC" "HanaMinA"))
     ("CJK Compatibility Ideographs Supplement"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "SimSun" "MingLiU" "HanaMinA" "Hiragino Kaku Gothic Pro" "Hiragino Maru Gothic Pro" "Hiragino Mincho Pro" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM B" "LiSong Pro"))
     ("CJK Radicals Supplement"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "SimSun" "Microsoft YaHei" "Microsoft YaHei UI" "HanaMinA" "BabelStone Han" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "DFKai-SB" "Apple Symbols" "Code2000"))
     ("CJK Strokes"
      ("WenQuanYi Zen Hei Mono" "HanaMinA" "BabelStone Han" "Code2000"))
     ("CJK Symbols and Punctuation"
      ("Lantinghei SC" "SimHei" "FangSong" "SimSun" "HanaMinA" "WenQuanYi Zen Hei Mono" "LiSong Pro" "STFangsong" "Microsoft YaHei" "Microsoft YaHei UI" "Lantinghei TC" "MingLiU" "HAN NOM A" "Arial Unicode MS" "PCMyungjo" "BabelStone Han" "Osaka:spacing=m" "Code2000"))
     ("CJK Unified Ideographs"
      ("WenQuanYi Zen Hei Mono" "Lantinghei SC" "Songti SC" "SimHei" "FangSong" "STFangsong" "SimSun" "LiSong Pro" "Baoli SC" "HanaMinA" "BabelStone Han" "Apple LiGothic" "Lantinghei TC" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HAN NOM A" "DFKai-SB" "Arial Unicode MS" "Xingkai SC" "GB18030 Bitmap" "UnBatang"))
     ("CJK Unified Ideographs Extension A"
      ("SimHei" "FangSong" "STFangsong" "SimSun" "Songti SC" "Microsoft YaHei" "Microsoft YaHei UI" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "HanaMinA" "HAN NOM A" "Code2000" "DFKai-SB" "BabelStone Han" "GB18030 Bitmap"))
     ("CJK Unified Ideographs Extension B"
      ("SimHei" "FangSong" "SimSun" "LiSong Pro" "Microsoft YaHei" "Microsoft YaHei UI" "HanaMinB" "HAN NOM B" "Code2002" "MingLiU" "Microsoft JhengHei" "Microsoft JhengHei UI" "BabelStone Han" "DFKai-SB"))
     ("CJK Unified Ideographs Extension C"
      ("HanaMinB" "BabelStone Han" "HAN NOM B"))
     ("CJK Unified Ideographs Extension D"
      ("HanaMinB" "BabelStone Han"))
     ("CJK Unified Ideographs Extension E"
      ("HanaMinB" "BabelStone Han"))
     ("Combining Diacritical Marks"
      ("Monaco" "Consolas" "Noto Sans" "Cambria Math" "Charis SIL" "Doulos SIL" "Courier New" "DejaVu Sans:width=condensed" "DejaVu Sans Mono" "Cardo" "Code2000" "Gentium Plus" "Junicode" "Tahoma" "Microsoft Sans Serif" "Arial" "Quivira" "Symbola" "Everson Mono" "FreeMono" "Arial Unicode MS" "ALPHABETUM Unicode"))
     ("Combining Diacritical Marks Extended"
      ("Monlam Uni Sans Serif"))
     ("Combining Diacritical Marks Supplement"
      ("Cardo" "FreeSerif" "Junicode" "Doulos SIL" "DejaVu Sans:width=condensed" "Noto Sans" "Segoe UI" "Code2000" "Everson Mono" "ALPHABETUM Unicode"))
     ("Combining Diacritical Marks for Symbols"
      ("Cambria Math" "Segoe UI Symbol" "Noto Sans Symbols" "Symbola" "Code2000" "Everson Mono" "Arial Unicode MS"))
     ("Combining Half Marks"
      ("Consolas" "DejaVu Sans:width=condensed" "Everson Mono:weight=bold" "Symbola"))
     ("Common Indic Number Forms"
      ("Noto Sans Kaithi" "Nirmala UI" "Siddhanta"))
     ("Control Pictures"
      ("Apple Symbols" "BabelStone Modern" "Noto Sans Symbols" "Segoe UI Symbol" "Arial Unicode MS" "Symbola" "Quivira" "FreeMono" "Code2000" "Everson Mono:weight=bold"))
     ("Counting Rod Numerals"
      ("WenQuanYi Zen Hei Mono" "Noto Sans Symbols" "BabelStone Modern" "Symbola" "Quivira" "Apple Symbols" "Code2001"))
     ("Cuneiform"
      ("Segoe UI Historic" "Noto Sans Cuneiform" "Noto Sans Sumero-Akkadian Cuneiform" "Akkadian"))
     ("Cuneiform Numbers and Punctuation"
      ("Akkadian" "Segoe UI Historic" "Noto Sans Cuneiform" "Noto Sans Sumero-Akkadian Cuneiform"))
     ("Currency Symbols"
      ("Monaco" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Consolas" "Noto Sans Symbols" "Noto Sans" "Segoe UI" "Apple Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Cyrillic"
      ("Consolas" "Monaco" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Noto Sans" "Courier New" "Calibri" "Microsoft Sans Serif" "Code2000" "Arial Unicode MS" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Charcoal CY" "Geneva CY" "ALPHABETUM Unicode"))
     ("Cyrillic Extended-A"
      ("Quivira" "Everson Mono:weight=bold" "FreeSerif" "ALPHABETUM Unicode"))
     ("Cyrillic Extended-B"
      ("Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Cyrillic Supplement"
      ("Consolas" "Courier New" "Calibri" "Noto Sans" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Code2000" "Everson Mono:weight=bold"))
     ("Dingbats"
      ("Apple Color Emoji" "DejaVu Sans Mono" "Segoe UI Symbol" "Zapf Dingbats" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Code2000" "Noto Sans Symbols" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Domino Tiles"
      ("DejaVu Sans:width=condensed" "Symbola" "Quivira" "Segoe UI Symbol" "Noto Sans Symbols" "Code2001" "Everson Mono:weight=bold"))
     ("Early Dynastic Cuneiform"
      ("Akkadian"))
     ("Egyptian Hieroglyphs"
      ("Segoe UI Historic:weight=bold" "Noto Sans Egyptian Hieroglyphs:weight=bold" "Aegyptus:weight=bold" "Gardiner"))
     ("Emoticons"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Quivira"))
     ("Enclosed Alphanumeric Supplement"
      ("Segoe UI Symbol" "Noto Sans Symbols" "Symbola" "Quivira" "BabelStone Han" "BabelStone Modern"))
     ("Enclosed Alphanumerics"
      ("Noto Sans Symbols" "Segoe UI Symbol" "Junicode" "Arial Unicode MS" "Symbola" "Quivira" "Code2000" "BabelStone Han" "WenQuanYi Zen Hei Mono" "BabelStone Modern" "HAN NOM A" "Everson Mono:weight=bold"))
     ("Enclosed CJK Letters and Months"
      ("WenQuanYi Zen Hei Mono" "SimHei" "FangSong" "MingLiU" "Arial Unicode MS" "HanaMinA" "Meiryo" "BabelStone Han" "Quivira" "Code2000" "UnBatang" "HAN NOM A"))
     ("Enclosed Ideographic Supplement"
      ("Segoe UI Symbol" "Noto Sans Symbols" "HanaMinA" "BabelStone Han" "Symbola"))
     ("General Punctuation"
      ("Monaco" "Apple Symbols" "Segoe UI Symbol" "Cambria Math" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Antinoou" "Symbola" "Code2000" "Quivira" "Noto Sans" "Everson Mono:weight=bold" "FreeMono" "BabelStone Modern"))
     ("Geometric Shapes"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Segoe UI Symbol" "Arial Unicode MS" "Symbola" "Noto Sans Symbols" "Quivira" "BabelStone Modern" "Everson Mono" "FreeMono" "Code2000"))
     ("Geometric Shapes Extended"
      ("Symbola" "Quivira"))
     ("Gothic"
      ("Noto Sans Gothic" "Segoe UI Historic" "Segoe UI Symbol" "Analecta" "Junicode" "Sadagolthina" "MPH 2B Damase" "FreeSerif" "Code2001" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Greek Extended"
      ("Consolas" "DejaVu Sans Mono" "Courier New" "Antinoou" "Noto Sans" "DejaVu Sans:width=condensed" "Cardo" "Junicode" "New Athena Unicode" "Microsoft Sans Serif" "Gentium Plus Compact" "Gentium Plus" "Arial Unicode MS" "Arial" "Tahoma" "Aegean" "Code2000" "Quivira" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Greek and Coptic"
      ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Antinoou" "Noto Sans" "Segoe UI Historic" "Segoe UI Symbol" "New Athena Unicode" "Calibri" "Microsoft Sans Serif" "Gentium Plus Compact" "Gentium Plus" "Lucida Console" "Arial Unicode MS" "Cardo" "Aegean" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode" "Noto Sans Coptic"))
     ("Halfwidth and Fullwidth Forms"
      ("Meiryo" "Arial Unicode MS" "Microsoft JhengHei" "Microsoft JhengHei UI" "Microsoft YaHei" "Microsoft YaHei UI" "BabelStone Han" "Apple Symbols" "Quivira" "Code2000" "HAN NOM A"))
     ("Hebrew"
      ("Miriam Fixed" "Ezra SIL" "Ezra SIL SR" "Arial Hebrew" "Raanana" "New Peninim MT" "Aharoni" "David" "FrankRuehl" "Gisha" "Levenim MT" "Narkisim" "Rod" "Cardo" "Courier New" "Adobe Hebrew" "Code2000" "Aramaic Imperial Yeb" "Microsoft Sans Serif" "Tahoma" "Lucida Sans Unicode" "Arial Unicode MS" "Arial" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("IPA Extensions"
      ("Monaco" "Consolas" "DejaVu Sans Mono" "Courier New" "Noto Sans" "Arial Unicode MS" "Arial" "Tahoma" "Microsoft Sans Serif" "Aboriginal Sans" "Cardo" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono" "Code2000" "ALPHABETUM Unicode"))
     ("Kannada"
      ("Kannada Sangam MN" "Noto Sans Kannada" "Noto Sans Kannada UI" "Tunga" "Akshar Unicode" "Kedage" "Nirmala UI" "Kannada MN" "Arial Unicode MS" "Code2000"))
     ("Latin Extended-C"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Noto Sans" "Cambria Math" "Gentium Plus" "Charis SIL" "Doulos SIL" "Code2000" "Quivira" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Latin Extended-D"
      ("FreeMono" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Charis SIL" "Doulos SIL" "Junicode" "Cardo" "Quivira" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Latin Extended-E"
      ("Quivira" "Everson Mono:weight=bold" "HanaMinA"))
     ("Letterlike Symbols"
      ("Monaco" "Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "Cambria Math" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Code2000" "Symbola" "Quivira" "HAN NOM A" "Everson Mono:weight=bold"))
     ("Mathematical Alphanumeric Symbols"
      ("Quivira" "Cambria Math" "Noto Sans Symbols" "Code2001" "Symbola" "Everson Mono:weight=bold"))
     ("Mathematical Operators"
      ("Monaco" "DejaVu Sans Mono" "Segoe UI Symbol" "Cambria Math" "DejaVu Sans:width=condensed" "Noto Sans Symbols" "Apple Symbols" "Arial Unicode MS" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Miscellaneous Mathematical Symbols-A"
      ("Noto Sans Symbols" "Apple Symbols" "Segoe UI Symbol" "Code2000" "Symbola" "Quivira" "Cambria Math" "Everson Mono:weight=bold"))
     ("Miscellaneous Mathematical Symbols-B"
      ("Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "Cambria Math" "Code2000" "Symbola" "Quivira"))
     ("Miscellaneous Symbols"
      ("Noto Sans Symbols" "Segoe UI Symbol" "Apple Symbols" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Arial Unicode MS" "Symbola" "Quivira" "MS Reference Sans Serif" "Cardo" "Code2000" "Everson Mono:weight=bold"))
     ("Miscellaneous Symbols and Arrows"
      ("Symbola" "Quivira" "Code2000" "Segoe UI Symbol" "Noto Sans Symbols"))
     ("Miscellaneous Symbols and Pictographs"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Quivira"))
     ("Miscellaneous Technical"
      ("Segoe UI Symbol" "Noto Sans Symbols" "Apple Symbols" "Cambria Math" "DejaVu Sans Mono" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Musical Symbols"
      ("Noto Sans Symbols" "Musica" "FreeSerif" "Symbola" "Quivira"))
     ("Number Forms"
      ("DejaVu Sans:width=condensed" "Arial Unicode MS" "Junicode" "Symbola" "Quivira" "Charis SIL" "Doulos SIL" "Code2000" "Everson Mono:weight=bold" "FreeMono" "ALPHABETUM Unicode"))
     ("Optical Character Recognition"
      ("Apple Symbols" "Segoe UI Symbol" "Noto Sans Symbols" "Arial Unicode MS" "Symbola" "Quivira" "FreeMono" "BabelStone Modern" "Code2000" "Everson Mono"))
     ("Ornamental Dingbats"
      ("Symbola"))
     ("Phonetic Extensions"
      ("Monaco" "Consolas" "Calibri" "Noto Sans" "Aboriginal Sans" "Charis SIL" "Doulos SIL" "Quivira" "Courier New" "DejaVu Sans:width=condensed" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Phonetic Extensions Supplement"
      ("Consolas" "Calibri" "Courier New" "Noto Sans" "Aboriginal Sans" "Charis SIL" "Doulos SIL" "Quivira" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Code2000" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Playing Cards"
      ("DejaVu Sans:width=condensed" "Symbola" "Noto Sans Symbols" "Segoe UI Symbol" "Quivira"))
     ("Runic"
      ("Noto Sans Runic" "Segoe UI Historic" "Segoe UI Symbol" "Aboriginal Serif" "Junicode" "FreeMono" "Quivira" "Code2000" "Cardo" "Everson Mono:weight=bold" "ALPHABETUM Unicode"))
     ("Small Form Variants"
      ("Apple Symbols" "Arial Unicode MS" "WenQuanYi Zen Hei Mono" "Microsoft YaHei" "Microsoft YaHei UI" "Code2000"))
     ("Specials"
      ("BabelStone Modern" "Noto Sans Symbols" "Apple Symbols" "Arial Unicode MS" "Symbola" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Quivira" "FreeMono" "BabelStone Han"))
     ("Superscripts and Subscripts"
      ("Consolas" "Monaco" "Apple Symbols" "Cambria Math" "DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Segoe UI Symbol" "Charis SIL" "Doulos SIL" "Symbola" "Quivira" "Everson Mono:weight=bold" "FreeMono"))
     ("Supplemental Arrows-A"
      ("Segoe UI Symbol" "Cambria Math" "DejaVu Sans:width=condensed" "Quivira" "Symbola" "Apple Symbols" "Noto Sans Symbols" "Code2000" "Everson Mono:weight=bold" "FreeMono" "BabelStone Modern"))
     ("Supplemental Arrows-B"
      ("Cambria Math" "Segoe UI Symbol" "Apple Symbols" "Noto Sans Symbols" "Quivira" "Symbola" "Code2000" "Everson Mono:weight=bold"))
     ("Supplemental Arrows-C"
      ("Symbola"))
     ("Supplemental Mathematical Operators"
      ("Cambria Math" "Segoe UI Symbol" "Noto Sans Symbols" "Apple Symbols" "Code2000" "Symbola" "Quivira" "Everson Mono:weight=bold"))
     ("Supplemental Punctuation"
      ("DejaVu Sans Mono" "Segoe UI Symbol" "Noto Sans Symbols" "Antinoou" "New Athena Unicode" "Cardo" "Aegean" "Symbola" "Quivira" "Everson Mono:weight=bold" "Code2000" "ALPHABETUM Unicode"))
     ("Supplemental Symbols and Pictographs"
      ("Symbola"))
     ("Transport and Map Symbols"
      ("Apple Color Emoji" "Segoe UI Symbol" "Symbola"))
     ("Variation Selectors"
      ("BabelStone Modern" "BabelStone Han" "Code2000"))
     ("Variation Selectors Supplement"
      ("BabelStone Modern" "BabelStone Han"))
     ("Vertical Forms"
      ("Microsoft YaHei" "Microsoft YaHei UI" "Symbola"))))
 '(unicode-fonts-fallback-font-list '("Symbola" "Quivira" "DejaVu Sans Mono"))
 '(user-mail-address "yorgey@hendrix.edu")
 '(vimish-fold-marks '("-- {{{" . "-- }}}"))
 '(warning-suppress-types '((comp)))
 '(whitespace-style
   '(face tabs trailing lines space-before-tab newline empty space-after-tab tab-mark))
 '(writegood-weasel-words
   '("many" "much" "various" "very" "fairly" "several" "extremely" "exceedingly" "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny" "are a number" "is a number" "excellent" "interestingly" "significantly" "substantially" "clearly" "vast" "relatively" "completely" "literally" "not rocket science" "outside the box" "note that" "a number of" "trivial" "trivially" "not hard" "easy" "easily" "clear" "clearly" "obvious" "obviously" "of course" "really" "nice" "in fact")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom faces, font lock, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; old colors

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
;;  '(font-lock-builtin-face ((((class color) (background light)) (:bold t :foreground "ForestGreen"))))
;;  '(font-lock-comment-face ((((class color) (background light)) (:bold t :foreground "DarkOrchid4"))))
;;  '(font-lock-constant-face ((((class color) (background light)) (:foreground "DarkGreen"))))
;;  '(font-lock-keyword-face ((((class color) (background light)) (:bold t :foreground "DarkGreen"))))
;;  '(font-lock-string-face ((((class color) (background light)) (:foreground "Red"))))
;;  '(font-lock-type-face ((((class color) (background light)) (:italic t :foreground "Purple"))))
;;  '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Green4")))))

;; From https://github.com/protesilaos/ef-themes
(require 'ef-themes)
(load-theme 'ef-frost t)

(add-hook  'text-mode-hook
	   (function (lambda ()
		       (auto-fill-mode 1))))
(add-hook  'cc-mode-hook
	(function (lambda ()
		(font-lock-mode 1))))

(defvar font-lock-auto-mode-list
  (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode 'ruby-mode 'python-mode 'intero-mode 'latex-mode 'agda2-mode 'ott-mode 'markdown-mode)
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
(global-set-key (kbd "C-c c")   'smart-compile)
(global-set-key (kbd "C-c x")   'recompile-latex-harder)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-=")     'align-on-equals)
(global-set-key (kbd "<f9>")    'delete-trailing-whitespace)
(global-set-key (kbd "C-c SPC")   'delete-horizontal-space-forward)
(global-set-key (kbd "C-c C-SPC") 'delete-horizontal-space-forward)
(global-set-key (kbd "M-C-}")   'transpose-paragraphs)

(global-set-key (kbd "C-x C-k c") 'BAY-comment)
; (global-set-key (kbd "C-c n") 'note-other-window)

(global-set-key (kbd "<f2>") 'toggle-window-split) ;; misc emacs stuff @ top
(global-set-key (kbd "<f5>") 'auto-fill-mode)
(global-set-key (kbd "<f6>") 'toggle-stylish-on-save)
(global-set-key (kbd "<f7>") 'touch)
(global-set-key (kbd "<f8>") 'toggle-window-dedicated)

(global-set-key (kbd "M-<f11>") 'get-firefox-title)
(global-set-key (kbd "<f11>") 'get-firefox-url)
(global-set-key (kbd "M-<f12>") 'get-firefox-markdown-link)
;; (global-set-key (kbd "<f12>") 'get-firefox-org-link)  ;; mozrepl

(global-set-key (kbd "C-c e") 'journal-goto-end)    ;; org-mode
(global-set-key (kbd "C-c w") 'journal-count-words) ;; org-mode

(global-set-key (kbd "C-x w") 'darcsum-whatsnew)  ;; darcsum
(add-hook 'darcsum-mode-hook
  (lambda () (local-set-key (kbd "s") #'darcs-push)))  ;; darcs push
(add-hook 'darcsum-mode-hook
  (lambda () (local-set-key (kbd "x") #'darcs-pull)))  ;; darcs pull
(global-set-key (kbd "C-x g") 'vc-status)         ;; magit & darcsum
(global-set-key (kbd "C-x v") 'darcsum-view)

(global-set-key (kbd "C-c C-b") 'beeminder-list-goals)

(global-set-key (kbd "C-x y") 'typo-fix)

(global-set-key (kbd "C-c g") 'writegood-mode)    ;; writegood-mode

(global-set-key (kbd "C-c m") 'commit-to-line-or-region)

(global-set-key (kbd "C-c t") 'mail-abbrev-insert-alias)

; (global-set-key (kbd "H-w") #'aya-create)
; (global-set-key (kbd "H-y") #'aya-expand)

(global-set-key (kbd "C-c C-f") #'vimish-fold)
(global-set-key (kbd "C-c C-d") #'vimish-fold-delete)

(add-hook 'find-file-hooks 'vimish-fold-from-marks)

(global-set-key (kbd "C-c C-k n") 'kattis-new)
(global-set-key (kbd "C-c C-k t") 'kattis-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grading/feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; argh, don't remember exactly how this works.  Need to document it.
(fset 'note
   (lambda (&optional arg) "Insert a ##n note from the other frame." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 18 35 35 13 23 24 111 134217788 19 134217849 13 1 down 67108896 19 35 35 13 1 134217847 24 111 25 backspace] 0 "%d")) arg)))

(fset 'typo-fix
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 34 18 13 67108896 19 34 19 13 134217847 32 45 45 62 32 25] 0 "%d")) arg)))

(fset 'BAY-comment
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("*** BAY: " 0 "%d")) arg)))

;; Type a number and then execute this macro.  If the point is
;; following nn, it searches for #nn in the other window and replaces
;; nn in the current window with the result of C-x C-k c (currently
;; bound to BAY-comment) followed by the rest of the line following
;; #nn in the other window.
(fset 'note-other-window
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([M-left 67108896 5 23 24 111 134217788 19 35 25 13 right 67108896 5 134217847 24 111 24 11 99 25] 0 "%d")) arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commits.to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://ergoemacs.org/emacs/emacs_region.html

(defun commit-to-line-or-region ()
  "Commit to something in the current line or region."
(interactive)
(let (pos1 pos2 bds)
  (if (use-region-p)
     (setq pos1 (region-beginning) pos2 (region-end))
    (progn
      (setq bds (bounds-of-thing-at-point 'line))
      (setq pos1 (car bds) pos2 (cdr bds))))

  (copy-region-as-kill pos1 pos2)
  (shell-command "/bin/bash /home/brent/local/mybin/commit &")
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kattis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun kattis-new (problem lang)
  "Start a new solution to a Kattis problem."
  (interactive "sProblem name: \nsLanguage: ")
  (cd "~/learning/Kattis")
  (shell-command (concat "kattis new " problem " " lang))
  (find-file (concat (downcase problem) "/" problem "." lang)))

(defun kattis-test ()
  "Test the Kattis solution in the current buffer."
  (interactive)
  (shell-command (concat "kattis test " (file-name-nondirectory (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-yaml
  :after lsp-mode
  :config
  (add-hook 'yaml-mode-hook #'lsp)
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode)
  )

  ;; :custom
  ;; (lsp-yaml-schemas '(("https://raw.githubusercontent.com/swarm-game/swarm/main/data/schema/scenario.json" . ["data/scenarios/*.yaml" "data/scenarios/**/*.yaml"]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  LSP, Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; M-x lsp-doctor
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package company
  :config (setq lsp-completion-provider :capf)
)

(use-package lsp-mode
  :commands (lsp lsp-execute-code-action)
  :hook ((go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :bind ("C-c C-c" . #'lsp-execute-code-action)
  :custom
  (lsp-diagnostics-modeline-scope :project)
  (lsp-file-watch-threshold 5000)
  (lsp-enable-file-watchers nil))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-sideline-diagnostic-max-lines 10)
  :after lsp-mode)

(use-package lsp-ivy
  :after (ivy lsp-mode))

(use-package haskell-snippets
  :after (haskell-mode yasnippet)
  :defer)

;; (use-package lsp-haskell
;;   :hook (haskell-mode . lsp)
;;   :custom
;;   (lsp-haskell-process-path-hie "haskell-language-server-wrapper")
;;   (lsp-haskell-process-args-hie '())
;;   )

(defun lsp-haskell-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'haskell-mode-hook #'lsp-haskell-install-save-hooks)

(font-lock-add-keywords 'haskell-mode
  '(("undefined" . font-lock-warning-face)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; delete whitespace forward

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

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

;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;; (defun javascript-custom-setup ()
;;   (moz-minor-mode 1))

;; (when (not (string= system-name "ampersand.seas.upenn.edu"))
;;   (require 'moz))

;; (defun jk/moz-get (attr)
;;   (comint-send-string (inferior-moz-process) attr)
;;   ;; try to give the repl a chance to respond
;;   (sleep-for 0 100))

;; (defun jk/moz-get-current-url ()
;;   (interactive)
;;   (jk/moz-get "repl._workContext.content.location.href"))

;; (defun jk/moz-get-current-title ()
;;   (interactive)
;;   (jk/moz-get "repl._workContext.content.document.title"))

;; (defun jk/moz-get-current (moz-fun)
;;   (funcall moz-fun)
;;   ;; doesn't work if repl takes too long to output string
;;   (save-excursion
;;                   (set-buffer (process-buffer (inferior-moz-process)))
;;                   (goto-char (point-max))
;;                   (previous-line)
;;                   (setq jk/moz-current (buffer-substring-no-properties
;;                                         (+ (point-at-bol) (length moz-repl-name) 3)
;;                                         (- (point-at-eol) 1))))
;;   (message "%s" jk/moz-current)
;;   jk/moz-current
;; )

;; (defun jk/moz-url ()
;;   (interactive)
;;   (jk/moz-get-current 'jk/moz-get-current-url)
;;   )

;; (defun jk/moz-title ()
;;   (interactive)
;;   (jk/moz-get-current 'jk/moz-get-current-title)
;;   )

;; (defun get-firefox-title ()
;;   (interactive)
;;   (insert (jk/moz-title))
;; )

;; (defun get-firefox-url ()
;;   (interactive)
;;   (insert (jk/moz-url))
;; )

;; (defun get-firefox-markdown-link ()
;;   (interactive)
;;   (insert (concat "[" (jk/moz-title) "](" (jk/moz-url) ")"))
;; )

;; (defun get-firefox-org-link ()
;;   (interactive)
;;   (insert (concat "[[" (jk/moz-url) "][" (jk/moz-title) "]]"))
;; )

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
;; Agda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Darcsum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vc-status ()
  (interactive)
  (let ((gitproject (locate-dominating-file (buffer-file-name) ".git"))
        (darcsproject (locate-dominating-file (buffer-file-name) "_darcs")))
    (when gitproject
      (call-interactively 'magit-status))
    (when darcsproject
      (darcsum-whatsnew darcsproject))))

(defun darcs-push ()
  (interactive)
  (message "Pushing changes...")
  (shell-command "darcs push -a"))

(defun darcs-pull ()
  (interactive)
  (message "Pulling...")
  (shell-command "darcs pull -a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)
(autoload 'magit-status "magit" nil t)

(setq magit-last-seen-setup-instructions "1.4.0")

; See http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
; Doesn't really seem to work, 2015 Oct 28

;; (defun endless/add-PR-fetch ()
;;   "If refs/pull is not defined on a GH repo, define it."
;;   (let ((fetch-address
;;          "+refs/pull/*/head:refs/pull/origin/*")
;;         (magit-remotes
;;          (magit-get-all "remote" "origin" "fetch")))
;;     (unless (or (not magit-remotes)
;;                 (member fetch-address magit-remotes))
;;       (when (string-match
;;              "github" (magit-get "remote" "origin" "url"))
;;         (magit-git-string
;;          "config" "--add" "remote.origin.fetch"
;;          fetch-address)))))

;; (add-hook 'magit-mode-hook #'endless/add-PR-fetch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Recompile LaTeX harder" if necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recompile-latex-harder ()
  (shell-command "/bin/zsh -c 'rm *.{aux,out}'")
  (smart-compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX + Haskell polymode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; See https://polymode.github.io/

(define-innermode poly-haskell-latex-innermode
  :mode 'haskell-mode
  :head-matcher "\\\\begin{\\(diagram\\|code\\)}"
  :tail-matcher "\\\\end{\\(diagram\\|code\\)}"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-latex-haskell-mode
  :hostmode 'pm-host/latex
  :innermodes '(poly-haskell-latex-innermode))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . poly-latex-haskell-mode))
;; (add-to-list 'auto-mode-alist '("\\.lhs\\'" . poly-latex-haskell-mode))

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

(put 'scroll-left 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
