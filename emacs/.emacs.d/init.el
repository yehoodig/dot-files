;; James McConnel
;; Fri Feb  9 15:22:13 PST 2018
;; emacs configuration file
;; Major features:
;;    Evil mode
;;     
;; 

;; 

;; Setup packages
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Setup Evil mode
(use-package evil
  :ensure t
  :init
  (setq-default evil-shift-width 3)
  :config
  (evil-mode 1)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;     Custom Evil keybindings       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-key evil-motion-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-motion-state-map (kbd "C-k") 'previous-buffer)
  (add-hook 'buffer-menu-mode-hook '(local-unset-key (kbd "C-k")))
  (define-key evil-normal-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-normal-state-map (kbd "C-k") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-;") 'term-toggle))
  
  
;; Other packages
(use-package powerline
  :ensure t
  :init
  :config
  (powerline-center-evil-theme))

(use-package neotree
  :ensure t
  :init
  :config
  ;; Neotree stuff for Evil mode
  (define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

;; Get rid of ~ file clutter, but backups are good
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Word Wrapping
(global-visual-line-mode t)

;;;;;;;;;;;;;;;;;;;;;
;; Line numbering  ;;
;;;;;;;;;;;;;;;;;;;;;

;; Relative line numbers
(linum-relative-mode)

;; In relative mode, current line is absolute number, not 0.
(setq linum-relative-current-symbol "")

;; Per mode line numbering behaviour 
(add-hook 'prog-mode-hook 'linum-relative-mode 1)
(add-hook 'term-mode-hook (lambda ()
   (linum-mode 0)
   (linum-relative-mode 0)))

;;;;;;;;;;;;;;;;;
;; Appearance  ;;
;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-echo-syntactic-information-p nil)
 '(c-syntactic-indentation t)
 '(custom-enabled-themes (quote (deeper-blue))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom functions
(defun edit-dot (name)
  (interactive
   (list (read-string "Name: ")))
  (if (string= name "vim")
      (find-file "~/.vim/vimrc"))
  (if (string= name "emacs")
      (find-file "~/.emacs.d/init.el")))

(defun term-toggle ()
  "From: https://gist.github.com/msoeken/4b2e3ee07b7252f8cb99 Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*ansi-term*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (progn
        (ansi-term (getenv "SHELL"))
        (setq show-trailing-whitespace nil)))))
