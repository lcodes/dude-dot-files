; -*- mode: Emacs-Lisp; tab-width: 4; fill-column: 80; -*-


;; Inspired from snippets found in various blogs articles, other dot-emacs files
;; and EmacsWiki pages.
;;
;; Written by Jeremie Pelletier <jeremiep@gmail.com>
;;
;; License: Public Domain


(eval-when-compile (require 'cl))

(defvar *emacs-load-start* (current-time))


; User information
; ------------------------------------------------------------------------------

(setq user-mail-address          "jeremiep@gmail.com")
(setq user-full-name             "Jeremie Pelletier")
(setq smtpmail-smtp-server       "localhost")

(setq mail-user-agent            'message-user-agent)
(setq message-send-mail-function 'message-smtpmail-send-it)


; dot-files
; ------------------------------------------------------------------------------
; Define at the top, if something goes wrong they still get evaluated

(defun open-dot-file (name) "Open the ~/.NAME file."
  (interactive) (find-file (concat "~/." name)))

(defun open-dot-emacs () "Open the ~/.emacs file"
  (interactive) (open-dot-file "emacs"))

(defun open-dot-zshrc () "Open the ~/.zshrc file"
  (interactive) (open-dot-file "zshrc"))

(defun open-dot-bash-profile () "Open the ~/.bash_profile file"
  (interactive) (open-dot-file "bash_profile"))

(defun open-dot-bashrc () "Open the ~/.bashrc file"
  (interactive) (open-dot-file "bashrc"))

(global-set-key (kbd "C-c C-d C-e") 'open-dot-emacs)
(global-set-key (kbd "C-c C-d C-z") 'open-dot-zshrc)
(global-set-key (kbd "C-c C-d C-p") 'open-dot-bash-profile)
(global-set-key (kbd "C-c C-d C-b") 'open-dot-bashrc)


; Paths
; ------------------------------------------------------------------------------

(defvar my-load-path '("~/.site-lisp"
                       "~/.site-lisp/cc-mode"
                       "~/.site-lisp/haskell-mode"
                       "~/.site-lisp/ghc-mod"))

(mapc #'(lambda (path) (add-to-list 'load-path path)) my-load-path)

(if window-system
    (let ((shell-path (replace-regexp-in-string
                       "[ \t\n]*$" ""
                       (shell-command-to-string
                        "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" shell-path)
      (setq exec-path (split-string shell-path path-separator))))


; Startup
; ------------------------------------------------------------------------------

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default frame-title-format (list "Emacs - %f"))
(setq-default icon-title-format "Emacs - %b")

(setq inhibit-startup-message t)

(setq explicit-shell-file-name "/bin/bash")

(setq ring-bell-function 'ignore)

(cd "~")

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


; Color theme
; ------------------------------------------------------------------------------

(add-hook 'font-lock-mode-hook
          '(lambda ()
             (set-face-background 'modeline "DarkGoldenrod4")
             (set-face-foreground 'modeline "grey8")))

(set-background-color "grey8")
(set-foreground-color "grey88")

(make-face-italic 'font-lock-comment-face)

(custom-set-faces
 '(font-lock-built-face         ((t (:foreground "SteelBlue4"))))
 '(font-lock-comment-face       ((t (:foreground "chartreuse4"))))
 '(font-lock-constant-face      ((t (:foreground "MediumPurple4"  :bold t))))
 '(font-lock-doc-face           ((t (:foreground "DodgerBlue3"))))
 '(font-lock-function-name-face ((t (:foreground "gold3"))))
 '(font-lock-keyword-face       ((t (:foreground "DarkSlateGray4" :bold t))))
 '(font-lock-string-face        ((t (:foreground "chocolate3"))))
 '(font-lock-variable-name-face ((t (:foreground "NavajoWhite1")))))


; Global packages
; ------------------------------------------------------------------------------

(autoload 'ispell "ispell" "" t)
(eval-after-load "ispell" '(progn (setq ispell-dictionary "english")))

(autoload 'thesaurus-choose-synonym-and-replace "thesaurus" "" t)
(eval-after-load "thesaurus"
  '(progn (setq thesaurus-bhl-api-key "699761ef74acd451675d335fa614f48e")))

;(require 'magit)

(autoload 'gtags-mode "gtags" "Minor mode for GNU global tags support." t)

; TODO: figure out how to autoload these

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.site-lisp/ac-dict")
(ac-config-default)

(require 'auto-compile)
(auto-compile-global-mode)


; Global Settings
; ------------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(ido-mode)

(global-ede-mode)

(column-number-mode)

(show-paren-mode)

(global-subword-mode)

(semantic-mode)

(which-function-mode)

(setq-default next-line-add-newlines t)

(setq-default truncate-lines         t)

(setq-default indent-tabs-mode       nil)
(setq-default tab-width              4)

(setq-default comment-column         40)
(setq-default fill-column            80)

(setq-default redisplay-dont-pause   t)


; Global key bindings
; ------------------------------------------------------------------------------

(defalias 'qrr 'query-replace-regexp)

(global-set-key (kbd "C-z") 'backward-kill-word)

(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key [f5] 'call-last-kbd-macro)

(global-set-key (kbd "C-x a r") 'align-regexp)

(global-set-key (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

(global-set-key (kbd "C-c s") 'replace-string)

; Fringes
; ------------------------------------------------------------------------------

(set-fringe-mode '(1, 1))
(setq-default indicate-empty-lines t)

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
      (setq ash-mark-bol (save-excursion
                           (goto-char mark-bol-posn)
                           (point-marker))))))

(add-to-list 'activate-mark-hook 'ash-mark-hook-fun)
(setq-default overlay-arrow-position ash-mark-bol)


; text-mode
; ------------------------------------------------------------------------------

(setq-default default-major-mode 'text-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)


; markdown-mode
; ------------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.Markdown$" . markdown-mode))


; yaml-mode
; ------------------------------------------------------------------------------

(autoload 'yaml-mode "yaml-mode.el" "Major mode for editing YAML files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


; json-mode
; ------------------------------------------------------------------------------

(autoload 'json-mode "json-mode.el" "Major mode for editing JSON files" t)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


; shell-script-mode
; ------------------------------------------------------------------------------

(autoload 'flymake-shell-load "flymake-shell" "" t)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


; js2-mode
; ------------------------------------------------------------------------------

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(eval-after-load "js2-mode"
  '(progn (setq js-indent-level 4)
          (setq js2-auto-indent-p t)
          (setq js2-cleanup-whitespace t)
          (setq js2-enter-indents-newline t)
          (setq js2-indent-on-enter-key t)
          (setq js2-mirror-mode nil)
          (setq js2-mode-indent-ignore-first-tab t)
          (setq js2-global-externs '("process" "require" "__filename"
                                     "__dirname" "module" "console" "$" "_"))))


; coffee-mode
; ------------------------------------------------------------------------------

(autoload 'coffee-mode "coffee-mode.el"
  "Major mode for editing CoffeeScript files" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.Cakefile$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))
(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

(add-to-list 'ac-modes 'coffee-mode)

;(add-to-list 'which-func-modes 'coffee-mode)

(autoload 'flymake-coffee-load "flymake-coffee.el" "" t)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(autoload 'coffeelintnode-hook "flymake-coffeelint.el" "" t)

(eval-after-load "flymake-coffeelint"
  '(progn (setq coffeelintnode-location "~/coffeelintnode")
          (setq coffeelintnode-coffeelint-set "")
          (setq coffeelintnode-autostart t)))

(add-hook 'coffee-mode-hook
          (lambda ()
            (coffeelintnode-hook)
            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key [f2] 'flymake-goto-prev-error)
            (local-set-key [f3] 'flymake-goto-next-error)))


; mustache-mode
; ------------------------------------------------------------------------------

(autoload 'mustache-mode "mustache-mode.el"
  "Major mode for editing Mustache files" t)
(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))


; haml-mode
; ------------------------------------------------------------------------------

(autoload 'haml-mode "haml-mode.el" "Major mode for editing Haml files" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))


; sass-mode
; ------------------------------------------------------------------------------

(autoload 'sass-mode "sass-mode.el" "Major mode for editing Sass files" t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))


; less-mode
; ------------------------------------------------------------------------------

(autoload 'less-mode "less-mode.el" "Major mode for editing Less files" t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-mode))


; cc-mode
; ------------------------------------------------------------------------------

(setq-default c-basic-offset 4)

(add-hook 'c-initialization-hook
	  '(lambda () (define-key c-mode-base-map "\C-m" 'c-context-line-break)))


; d-mode
; ------------------------------------------------------------------------------

(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.di?$" . d-mode))


; haskell-mode
; ------------------------------------------------------------------------------

(autoload 'haskell-mode "~/.site-lisp/haskell-mode/haskell-site-file" "" t)
(add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))


; comint-mode
; ------------------------------------------------------------------------------

(defun my-comint-clear ()
  "Clears the current comint buffer."
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))


; Misc.
; ------------------------------------------------------------------------------

(defvar my-mode-line
  "-*- mode: %s; tab-width: %d; fill-column: %d; comment-column: %d; -*-")

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
  (insert (format my-mode-line mode-name tab-width fill-column comment-column)))

(defun regexp-filter (regexp list)
  "Filter LIST of strings with REGEXP."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
        (setq new (cons string new))))
    (nreverse new)))

(defun directory-lisp-files (path)
  "Return a list of the lisp files found in the directory at PATH."
  (if (file-directory-p path) (regexp-filter "\\.el$" (directory-files path))
    '()))

(defun byte-compile-directory (path)
  "Compile the lisp files in the directory at PATH into byte code."
  (mapc 'byte-compile-file (mapcar #'(lambda (file) (concat path "/" file))
								   (directory-lisp-files path))))

(defun byte-compile-dot-emacs ()
  "Compile the ~/.emacs file into byte code."
  (byte-compile-file "~/.emacs"))

(defun byte-compile-world ()
  "Compile the ~/.emacs file and all files found on the LOAD-PATH
into byte code."
  (interactive)
  (byte-compile-dot-emacs)
  (mapc 'byte-compile-directory my-load-path))


; Load init files
; ------------------------------------------------------------------------------

(let ((base "~/.site-lisp/init.d"))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (not (equal f ".."))
                 (not (equal f ".")))
        (load-file name)))))


; Profile .emacs load time
; ------------------------------------------------------------------------------

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*)
                                             (second *emacs-load-start*)))))
