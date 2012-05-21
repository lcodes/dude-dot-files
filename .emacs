; -*- mode: Emacs-Lisp; tab-width: 4; fill-column: 80; -*-


;; Author:
;;
;;   Jeremie Pelletier
;;
;; References:
;;
;;   https://sites.google.com/site/steveyegge2/effective-emacs
;;   https://github.com/bodil/emacs.d/blob/master/bodil-js.el


; User information
; ------------------------------------------------------------------------------

(setq user-mail-address          "jeremiep@gmail.com")
(setq user-full-name             "Jeremie Pelletier")
(setq smtpmail-smtp-server       "localhost")

(setq mail-user-agent            'message-user-agent)
(setq message-send-mail-function 'message-smtpmail-send-it)


; Load path
; ------------------------------------------------------------------------------

(mapcar '(lambda (path) (add-to-list 'load-path path))
		'("/etc/site-lisp"
		  "/usr/local/share/emacs/site-lisp"
		  "~/.site-lisp"
		  "~/.emacs.d/lisp"
		  "~/.site-lisp/cc-mode"
		  "~/.site-lisp/haskell-mode"
		  "~/.site-lisp/ghc-mod"
		  "~/share/emacs/site-lisp"))

(setenv (concat "~/bin:~/Library/Haskell/bin:/usr/local/bin:/opt/local/bin:/sw/bin:" (getenv "PATH")))
(setq exec-path (append '("~/bin" "~/Library/Haskell/bin" "/usr/local/bin" "/opt/local/bin" "/sw/bin") exec-path))


; General
; ------------------------------------------------------------------------------

;; Start maximized.
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Start in the home directory.
(cd "~")

;; Use a better buffer switching.
(ido-mode)

;; Ignore the annoying bell sounds.
(setq ring-bell-function 'ignore)

(defalias 'qrr 'query-replace-regexp)

;; Use C-z rather than <C-backspace> to kill the word behind the point.
(global-set-key "\C-z" 'backward-kill-word)

;; Use C-c C-m rather than M-x to invoke interactive functions.
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Use F5 to execute keyboard macros.
(global-set-key [f5] 'call-last-kbd-macro)

(global-subword-mode)

(setq next-line-add-newlines t)

;(global-set-key (kbd "\C-c d") 'insert-standard-date)

(defun insert-standard-date ()
    "Inserts standard date time string." 
    (interactive)
    (insert (format-time-string "%c")))

;; Don't show the GNU startup buffer. (*scratch* is shown instead.)
(setq inhibit-startup-message t)

;; Don't show the UI.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Replace "yes" or "no" choices by "y" or "n".
(fset 'yes-or-no-p 'y-or-n-p)

;; Set titles formats
(setq-default frame-title-format (list "Emacs - %f"))
(setq-default icon-title-format "Emacs - %b")

;; Use bash as the shell
(setq explicit-shell-file-name "/bin/bash")

;(require 'breadcrumb)

;(require 'filladapt)

;(add-hook 'c-mode-common-hook
;          (lambda ()
;            (when (featurep 'filladapt)
;              (c-setup-filladapt))))

(setq desktop-save 'if-exists)
(desktop-save-mode 1)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 100)
                tags-file-name
                register-alist)))

; Buffers & Windows
; ------------------------------------------------------------------------------

(defun insert-default-file-variables ()
  "Inserts the text for file variables with default values."
  (interactive)
  (let ((tab-width 4)
        (fill-column 80)
        (comment-column 40))
    (insert-file-variables)))

(defun insert-file-variables ()
  "Inserts the text for file variables"
  (interactive)
  (insert-string (format "-*- mode: %s; tab-width: %d; fill-column: %d; comment-column: %d; -*-"
          mode-name tab-width fill-column comment-column)))

;; Don't abort rendering to process user input.
(setq redisplay-dont-pause t)

(which-func-mode 1)

(require 'ispell)
(setq ispell-dictionary "english")

;; Also display column number.
(setq-default column-number-mode t)

; Don't wrap lines.
(setq-default truncate-lines t)

;; Highlight matching parenthesis.
(show-paren-mode t)

;; Use spaces instead of tabs for indentation.
(setq-default indent-tabs-mode nil)

;; Basic formatting variables.
(setq-default tab-width 4)
(setq-default comment-column 40)
(setq-default fill-column 80)

(global-set-key (kbd "C-x a r") 'align-regexp)

;; TODO autoload
(require 'magit)
(require 'flymake)

(require 'thesaurus)
(setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")
(global-set-key (kbd "\C-x t") 'thesaurus-choose-synonym-and-replace)

(setq default-indicate-empty-lines t)

(set-fringe-mode '(1, 1))

;; Show mark in fringe
(defvar ash-mark-bol
  (save-excursion
    (goto-char (or (mark) (point)))
    (forward-line 0)
    (point-marker))
  "Marker from `beginning-of-line' for for `mark'.")

(defun ash-mark-hook-fun ()
  "Run with `activate-mark-hook'."
  (let ((mark-bol-posn (save-excursion
                         (goto-char (mark))
                         (forward-line 0)
                         (point))))
    (if (markerp ash-mark-bol)
        (set-marker ash-mark-bol mark-bol-posn)
      (setq ash-mark-bol
                     (save-excursion
                     (goto-char mark-bol-posn)
                     (point-marker))))))

(add-to-list 'activate-mark-hook 'ash-mark-hook-fun)
(setq overlay-arrow-position ash-mark-bol)


; comint-mode
; ------------------------------------------------------------------------------

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(defun clear-shell ()
  "Clears the current comint buffer."
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

;; TODO: comint hook to add kbd shortcut


; auto-complete-mode
; ------------------------------------------------------------------------------

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.site-lisp/ac-dict")

(ac-config-default)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

; text-mode
; ------------------------------------------------------------------------------

(setq-default default-major-mode 'text-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)


; markdown-mode
; ------------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.Markdown$" . markdown-mode))


; yaml-mode
; ------------------------------------------------------------------------------

(autoload 'yaml-mode "yaml-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


; haskell-mode
; ------------------------------------------------------------------------------

(load "~/.site-lisp/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(when (fboundp 'resize-minibuffer-mode) ; for old emacs
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(add-hook 'haskell-mode-hook '(lambda () (define-key haskell-mode-map "\C-cd"
                                           'credmp/flymake-display-err-minibuf)))

;; flymake-haskell

;(defun flymake-haskell-init ()
;  (flymake-simple-make-init-impl
;   'flymake-create-temp-with-folder-structure nil nil
;   (file-name-nondirectory buffer-file-name)
;   'flymake-get-haskell-cmdline))

;(defun flymake-get-haskell-cmdline (source base-dir)
;  (list "flycheck_haskell.pl"
;        (list source base-dir)))

;(push '(".+\\.hs$" flymake-haskell-init flymake-simple-java-cleanup)
;      flymake-allowed-file-name-masks)

;(push '(".+\\.lhs$" flymake-haskell-init flymake-simple-java-cleanup)
;      flymake-allowed-file-name-masks)

;(push '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)" 1 2 3 4)
;      flymake-err-line-patterns)

;(add-hook 'haskell-mode-hook
;          '(lambda ()
;             (if (not (null buffer-file-name)) (flymake-mode))))


; js2-mode
; ------------------------------------------------------------------------------

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js-indent-level 4)
(setq js2-auto-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-indent-on-enter-key t)
(setq js2-mirror-mode nil)
(setq js2-mode-indent-ignore-first-tab t)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(setq js2-global-externs '("process" "require" "__filename" "__dirname" "module"
                           "console" "$" "_"))

;(add-hook 'js2-mode-hook (lambda () (h1-line-mode t)))

                           
; json-mode
; ------------------------------------------------------------------------------

(autoload 'json-mode "json-mode.el" "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.json" . json-mode))


; coffee-mode
; ------------------------------------------------------------------------------

(autoload 'coffee-mode "coffee-mode.el" "Major mode for editing CoffeeScript files" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))
(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

(add-to-list 'ac-modes 'coffee-mode)

(add-to-list 'which-func-modes 'coffee-mode)

(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(require 'flymake-coffeelint)
(setq coffeelintnode-location "~/coffeelintnode")
(setq coffeelintnode-coffeelint-set "indentation:4")
(setq coffeelintnode-autostart t)

(add-hook 'coffee-mode-hook
          (lambda ()
            (coffeelintnode-hook)
            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key [f2] 'flymake-goto-prev-error)
            (local-set-key [f3] 'flymake-goto-next-error)))


; cc-mode
; ------------------------------------------------------------------------------

;; Use 4 spaces indentation as default for C-like syntaxes.
(setq-default c-basic-offset 4)

;; Redefine <RET> to indent the new line.
(add-hook 'c-initialization-hook
	  '(lambda () (define-key c-mode-base-map "\C-m" 'c-context-line-break)))


; d-mode
; ------------------------------------------------------------------------------

;; Load d-mode.
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)

;; Automatically switch to d-mode for *.d and *.di files.
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(defun d-module-name ()
  "Return the name of the current D module."
  (file-name-nondirectory (file-name-sans-versions (file-name-sans-extension (buffer-file-name)))))

(defun d-insert-module-name ()
  "Insert the name of the current D module at the current point."
  (interactive) (insert (d-module-name)))

;(defun d-insert-import (name)
;  ""
;  (interactive)
;  (save-excursion
;    (goto-char (point-min))
;    (when (search-forward ""

(defun d-insert-header ()
  "Inserts a generic header for D files."
  (interactive)
  (insert "// Written in the D programming language.\n")
  (insert "\n")
  (insert "/**\n")
  (insert " * \n")
  (insert " * Authors: $(EMAIL Jeremie Pelletier, jeremiep@gmail.com)\n") ;; TODO: use user name
  (insert " * License: \n")
  (insert " */\n")
  (insert (format "module %s;\n\n" (d-module-name))))

;; (require 'flymake-d)
;; (require 'flymake-cursor)
;; (require 'rfringe)

;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:underline "red"))))
;;  '(flymake-warnline ((((class color)) (:underline "yellow")))))


; Lisp-mode
; ------------------------------------------------------------------------------

(require 'auto-compile)
(auto-compile-global-mode 1)


; Color theme
; ------------------------------------------------------------------------------

(require 'font-lock)

;; Enable colors
(global-font-lock-mode t)

;; Mode line colors.
(add-hook 'font-lock-mode-hook
	  '(lambda ()
	     (set-face-background 'modeline "DarkGoldenrod4")
	     (set-face-foreground 'modeline "grey8")))

;; Background/Foreground colors.
(set-background-color "grey8")
(set-foreground-color "grey88")

;; Make comments italic.
(make-face-italic 'font-lock-comment-face)

;; Syntax highlighting colors.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-built-face ((t (:foreground "SteelBlue4"))))
 '(font-lock-comment-face ((t (:foreground "chartreuse4"))))
 '(font-lock-constant-face ((t (:bold t :foreground "MediumPurple4"))))
 '(font-lock-doc-face ((t (:foreground "DodgerBlue3"))))
 '(font-lock-function-name-face ((t (:foreground "gold3"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "DarkSlateGray4"))))
 '(font-lock-string-face ((t (:foreground "chocolate3"))))
 '(font-lock-variable-name-face ((t (:foreground "NavajoWhite1")))))


; IDE features
; ------------------------------------------------------------------------------

;; GNU global tags.
(autoload 'gtags-mode "gtags" "Minor mode for GNU global tags support." t)

;; EDE mode
(global-ede-mode t)
(semantic-mode 1)

;; Add DMD compiler to EDE.
;;(defvar ede-dmd-compiler
;;  (ede-compiler
;;   "ede-dmd-compiler"
;;   :name "dmd"
;;   :variables '(("DMD" . "dmd"))
;;   :commands '("dmd -o $ $<")
;;   :autoconf '(("AC_CHECK_PROG" . "DMD, dmd"))
;;   :sourcetype '(ede-dmd-source))
;;  "Compile D files.")


; Functions
; ------------------------------------------------------------------------------

;; Function to open this file
(defun open-dot-emacs ()
  "Open the ~/.emacs file."
  (interactive)
  (find-file "~/.emacs"))

(desktop-load-default)
(desktop-read)
