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
  
  ;; Fixes the above navigation, so that the cyle doesn't stick in the buffer menu
  (define-key Buffer-menu-mode-map (kbd "C-k") 'previous-buffer)

  (define-key evil-normal-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-normal-state-map (kbd "C-k") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-<return>") 'term-toggle)

  ;; For org mode
  ;; Some of these are from evil-org-mode, but this seemed easier to implement somehow.
  (evil-define-key 'normal org-mode-map
   (kbd "C-c") 'org-toggle-checkbox
   "H" 'org-shiftleft
   "J" 'org-shiftdown
   "K" 'org-shiftup
   "L" 'org-shiftright
   "<" 'org-metaleft
   ">" 'org-metaright
   "-" 'org-cycle-list-bullet
   "t" 'org-todo)
  )

;;(use-package evil-org
;;  :ensure t
;;  :after org
;;  :config
;;  (add-hook 'org-mode-hook 'evil-org-mode)
;;  (add-hook 'evil-org-mode-hook
;;	    (lambda ()
;;	      (evil-org-set-key-theme))))
  
  
;; Other packages

(use-package git
  :ensure t
  :init
  :config
  ;; Non-Elpa packages to load
   (if (not (file-exists-p "~/.emacs.d/ks-mode"))
      (git-clone "https://github.com/jarpy/ks-mode" "~/.emacs.d/")) 
   (load "~/.emacs.d/ks-mode/ks.el"))

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-center-evil-theme))

(use-package all-the-icons
  :ensure t
  :init
  :config
  )


(use-package neotree
  :ensure t
  :init
  :config
  ;;from: https://github.com/jaypei/emacs-neotree/issues/77
  (defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
   
  ;;You may need to execute all-the-icons-install-fonts manually
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Neotree stuff for Evil mode
  (define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter-hide))

(use-package linum-relative
  :ensure t
  :init
  :config
  ;; Relative line numbers
  (linum-relative-mode)

  ;; In relative mode, current line is absolute number, not 0.
  (setq linum-relative-current-symbol "")

  ;; Per mode line numbering behaviour 
  (add-hook 'prog-mode-hook 'linum-relative-mode 1)
  (add-hook 'term-mode-hook (lambda ()
     (linum-mode 0)
     (linum-relative-mode 0))))

;;;;;;;;;;;
;; MISC  ;;
;;;;;;;;;;;

(server-start)
(if (file-exists-p "~/.emacs.d/misc/ks.el")
		   (load "~/.emacs.d/misc/ks.el"))

;; Get rid of ~ file clutter, but backups are good
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;;;;;;;;;;;;;;;;;
;; Appearance  ;;
;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Word Wrapping
(global-visual-line-mode t)

;;Tabs 
(setq-default indent-tabs-mode nil)
;;Autoindent
(electric-indent-mode 1)

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

;; Work in progress
(defun use-git-package (name)
  "Work in progress: Clone github repository if not existent in emacs.d"
   (if (not (file-exists-p "~/.emacs.d/ks-mode"))
      (git-clone "https://github.com/jarpy/ks-mode" "~/.emacs.d/")) 
   (load "~/.emacs.d/ks-mode/ks.el"))

