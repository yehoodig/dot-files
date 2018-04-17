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
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;;custom bits
(if (file-exists-p "~/.emacs.d/ks-mode/ks.el")
  (load "~/.emacs.d/ks-mode/ks.el"))

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

  ;; General
  ;;;;;;;;;;

  ;;Normal State
  (define-key evil-normal-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-normal-state-map (kbd "C-k") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-<return>") 'term-toggle)

  ;;Insert State
  (define-key evil-insert-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-insert-state-map (kbd "C-k") 'previous-buffer)
  (define-key evil-insert-state-map (kbd "C-<return>") 'term-toggle)

  ;;Motion State
  (define-key evil-motion-state-map (kbd "C-j") 'next-buffer)
  (define-key evil-motion-state-map (kbd "C-k") 'previous-buffer)
  (define-key evil-motion-state-map (kbd "C-<return>") 'term-toggle)
  
  ;; Specific Modes
  ;;;;;;;;;;;;;;;;;

  ;; Buffer Menu 
  (define-key Buffer-menu-mode-map (kbd "C-k") 'previous-buffer)
  ;;(define-key Buffer-menu-mode-map (kbd "C-<return>") 'term-toggle)

  ;; Org Mode
  ;;;;;;;;;;;
  ;; Some of these are from evil-org-mode, but this seemed easier to implement.
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

  
;; Other packages

(use-package git
  :ensure t
  :init
  :config
;;  ;; Non-Elpa packages to load
   ;;(if (not (file-exists-p "~/.emacs.d/ks-mode"))
      ;;----> this is the non-working line (git-clone "https://github.com/yehoodig/ks-mode" "~/.emacs.d/")) 
)

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-center-evil-theme))

(use-package all-the-icons
  :if window-system
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

;;;; infocom z games
;;(use-package malyon
;;  :ensure t
;;  :init
;;  :config
;;  )

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
  (add-hook 'ks-mode-hook 'linum-relative-mode 1)
  (add-hook 'term-mode-hook (lambda ()
     (linum-mode 0)
     (linum-relative-mode 0))))

;;;;;;;;;;;
;; MISC  ;;
;;;;;;;;;;;

(server-start)

;; Get rid of ~ file clutter, but backups are good
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(if
    (string-match "Microsoft"
         (with-temp-buffer (shell-command "uname -r" t)
                           (goto-char (point-max))
                           (delete-char -1)
                           (buffer-string)))
    (message "Running under Linux subsystem for Windows")
    (message "Not running under Linux subsystem for Windows")
)
;;;;;;;;;;;;;;;;;
;; Appearance  ;;
;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
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
  (if (string= name "bash")
      (find-file "~/.bashrc"))
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
