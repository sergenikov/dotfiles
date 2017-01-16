;; Setup largely based on http://aaronbedra.com/emacs.d/. Thanks to him.
(setq user-full-name "Sergey Skovorodnikov")

;; Package Management
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" magit)))

;; Define default packages
(defvar sergey/packages '(ac-slime
                          auto-complete
                          autopair
                          csharp-mode
                          deft
                          flycheck
                          go-autocomplete
                          go-eldoc
                          go-mode
                          haml-mode
                          magit
                          markdown-mode
                          marmalade
                          nodejs-repl
                          paredit
                          restclient
                          rvm
                          sml-mode
                          solarized-theme
                          monokai-theme
                          atom-dark-theme
                          web-mode
                          yaml-mode
                          evil
                          smex
                          projectile
			  )
  "Default packages")

(defun sergey/packages-installed-p ()
  (cl-loop for pkg in sergey/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (sergey/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg sergey/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Start-up options
;; Turn off splash screen
(setq inhibit-splash-screen t)

;; No scroll bar, toolbar or menubar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Writing with selected text will overwrite that text
(delete-selection-mode t)
;; Turn on highlighting similar to other editors
(transient-mark-mode t)
;; Make Emacs clipboard work with system clipboard
(setq x-select-enable-clipboard t)

;; Display settings
;; Frame title
(setq-default indicate-empty-lines t)
;; Show where file actually ends
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; Turn off built-in backup mechanism
(setq make-backup-files nil)

;; Define alias for "yes" and "no" to "y" and "n"
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; Miscellaneous key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;;(global-set-key (kbd "M-/") 'hippie-expand)
;;(global-set-key (kbd "C-+") 'text-scale-increase)
;;(global-set-key (kbd "C--") 'text-scale-decrease)
;;(global-set-key (kbd "C-c C-k") 'compile)
;;(global-set-key (kbd "C-x g") 'magit-status)

;;Turn down the time to echo keystrokes so I don't have to wait around for things to happen.
;;Dialog boxes are also a bit annoying, so just have Emacs use the echo area for everything.
;;Use a visual indicator instead of making horrible noises.
;;Always highlight parentheses.
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Vendor directories
;; For packages that don't come from Melpa
(defvar sergey/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path sergey/vendor-dir)
(dolist (project (directory-files sergey/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Temporary files
;; Disable them
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; autopair-mode
(require 'autopair)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Re-indent, untabify and clean up whitespace.
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

;; flyspell
;; Built in spell checker
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; YAML
;; Add additional file extensions to trigger yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; JavaScript mode
;; Default to 4 spaces for indentation, this changes it to 2
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

;; Markdown mode
;; Enable Markdown mode and setup additional file extensions.
;; Use pandoc to generate HTML previews from within the mode, and use a custom css file to make it a little prettier.
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths `(,(expand-file-name "markdown.css" sergey/vendor-dir)))

;; Go mode
(require 'go-autocomplete)

(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            (add-hook 'before-save-hook 'gofmt-before-save)))

;; Set solarized theme in graphical environment, wombat in terminal
(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

;; Evil mode
(setq evil-want-C-u-scroll t)
(evil-mode 1)

;; Server start
(server-start)

;; Smooth scrolling with mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Hook to have reverse-i-search C-r in terminal within emacs
(add-hook 'term-mode-hook
    (lambda()
      (global-unset-key (kbd "C-r"))
;	    (local-unset-key (kbd "C-r"))
      (message "%s" "This is in term mode and hook enabled.")
))

;; Smex
;; Provides history and searching on top of M-x
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
;; Filesystem navigation
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Column number mode
(setq column-number-mode t)

;; Helm
;; helm configuration
;;(require 'helm)
;;(require 'helm-config)
;;
;;(global-set-key (kbd "C-x b")   #'helm-mini)
;;(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
;;(global-set-key (kbd "C-x C-m") #'helm-M-x)
;;(global-set-key (kbd "M-s o")   #'helm-swoop)
;;
;;(helm-mode t)
;;(helm-adaptive-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (ztree ac-helm yaml-mode web-mode solarized-theme sml-mode rvm restclient paredit nodejs-repl monokai-theme marmalade markdown-mode magit haml-mode go-eldoc go-autocomplete flycheck evil deft csharp-mode autopair atom-dark-theme ac-slime)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background-mode nil)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'projectile)
