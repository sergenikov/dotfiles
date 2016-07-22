;;; package --- Summary

;;; Commentary
(require 'package)

;;; Code: 
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)


;; CONFIGS THAT MAKES SENSE
;; highlight matching parentheses
(show-paren-mode 1)
;; (set-frame-font "Monospace-9")
;; set tab width to 2
(setq tab-width 2)			;
;; (global-relative-line-numbers-mode)
(load-theme 'atom-dark t)
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
;; stop showing splash screen
(setq inhibit-startup-message t)
;; no toolbar
(tool-bar-mode -1)
;; smooth scrolling - one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

; start emacs server so that i can open files in current instance of emacs
(server-start)

;; Configure load path correctly
(add-to-list 'load-path "~/.emacs.d/lisp/") ;; corrected give load path error

;; add autocompletion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/sergey/.emacs.d/ac-dict")
(ac-config-default)
;; auto-complete config done

;; fly-check - syntax checker
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode)


;; install evil mode
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; turn off emacs's c-u to remap to c-u in vim - should happen before evil is enabled
;; (setq evil-want-C-u-scroll t)
;; (require 'evil)
;; (evil-mode 1)

;; go mode setup
;; (require 'go-mode-load)

;; setup PATH environment
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; call gofmt on save
(setenv "GOPATH" "/home/sergey/gowork")

(setq exec-path (cons "/home/sergey/gowork/bin" exec-path))
(add-to-list 'exec-path "/home/sergey/gowork/bin")
(add-hook 'before-save-hook 'gofmt-before-save)
;;=== go mode setup done


;; add goimports hook
;; Every time I save the file it will update my imports - i don't
;;  want that right now
;; (defun my-go-mode-hook ()
;;   ; Use goimports instead of go-fmt
;;   (setq gofmt-command "goimports")
;;   ; Call Gofmt before saving
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet"))
;;   ; Godef jump key binding
;;   (local-set-key (kbd "M-.") 'godef-jump))
;; (add-hook 'go-mode-hook 'my-go-mode-hook)


;; ;; go autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; powerline evil
(require 'powerline-evil)
(powerline-default-theme)


;; Hook to have reverse-i-search C-r in terminal within emacs
(add-hook 'term-mode-hook
    (lambda()
      (global-unset-key (kbd "C-r"))
;	    (local-unset-key (kbd "C-r"))
      (message "%s" "This is in term mode and hook enabled.")
))

;; set tab width to 2
(setq default-tab-width 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
	 (quote
		("c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" default)))
 '(fci-rule-color "#3E3D31")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
	 (quote
		(("#3E3D31" . 0)
		 ("#67930F" . 20)
		 ("#349B8D" . 30)
		 ("#21889B" . 50)
		 ("#968B26" . 60)
		 ("#A45E0A" . 70)
		 ("#A41F99" . 85)
		 ("#3E3D31" . 100))))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#F92672")
		 (40 . "#CF4F1F")
		 (60 . "#C26C0F")
		 (80 . "#E6DB74")
		 (100 . "#AB8C00")
		 (120 . "#A18F00")
		 (140 . "#989200")
		 (160 . "#8E9500")
		 (180 . "#A6E22E")
		 (200 . "#729A1E")
		 (220 . "#609C3C")
		 (240 . "#4E9D5B")
		 (260 . "#3C9F79")
		 (280 . "#A1EFE4")
		 (300 . "#299BA6")
		 (320 . "#2896B5")
		 (340 . "#2790C3")
		 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; MY FUNCTIONS
;; set evil mode and turn on C-u for vim-like scrolling
(defun evil ()
	('evil-mode 1)
	(setq evil-want-C-u-scroll t))

(defun no-evil ()
	('evil-mode 0)
	(setq evil-want-C-u-scroll f))

;; MY KEY BINDINGS
(global-set-key (kbd "<f5>") 'evil-mode)

(add-to-list 'load-path "~/.emacs.d/elpa/neotree-0.2.1")
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)


;; helm configuration
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
;;(global-set-key (kbd "C-x C-r") #'helm-recentf)
;;(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
;;(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s o")   #'helm-swoop)
;;(global-set-key (kbd "M-s /")   #'helm-multi-swoop)

(require 'helm-config)
(helm-mode t)
(helm-adaptive-mode t)


;;(global-set-key (kbd "M-i") 'helm-swoop)
