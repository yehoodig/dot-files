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


;; Setup Evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
;; Evil mode specific things
(setq-default evil-shift-width 3)
(require 'evil)
(evil-mode 1)

;; Other packages
(require 'powerline)
(powerline-center-evil-theme)
(require 'neotree)

;; Get rid of ~ file clutter, but backups are good
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Word Wrapping
(global-visual-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Custom Evil keybindings       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key evil-normal-state-map (kbd "C-j") 'next-buffer)
(define-key evil-normal-state-map (kbd "C-k") 'previous-buffer)

(define-key evil-normal-state-map (kbd "C-;") 'toggle-term)

;; Neotree stuff for Evil mode
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)


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
(defun edit-dot-emacs ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun toggle-term ()
  "From: https://gist.github.com/msoeken/4b2e3ee07b7252f8cb99 Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*ansi-term*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (progn
        (ansi-term (getenv "SHELL"))
        (setq show-trailing-whitespace nil)))))
;;(global-set-key (kbd "<f9>") 'toggle-term)