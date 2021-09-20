;; James McConnel
;; Fri Feb  9 15:22:13 PST 2018
;; emacs configuration file
;; Major features:
;;    Evil mode
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

;;;;;;;;;;;;;;;;;;;;;;;
;; Other Plugins     ;;
;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/.emacs.d/arduino-mode/arduino-mode.el")
    (load "~/.emacs.d/arduino-mode/arduino-mode.el")
  (progn
    (shell-command (concat "git clone https://github.com/bookest/arduino-mode.git ~/.emacs.d/arduino-mode"))
    (load "~/.emacs.d/arduino-mode/arduino-mode.el")))

(if (file-exists-p "~/.emacs.d/ks-mode/ks.el")
    (load "~/.emacs.d/ks-mode/ks.el")
  (progn
    (shell-command (concat "git clone https://github.com/yehoodig/ks-mode.git ~/.emacs.d/ks-mode"))
    (load "~/.emacs.d/ks-mode/ks.el")))

;;;;;;;;;;;;;;;
;; Evil mode ;;
;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq-default evil-shift-width 3)
  :config
  (evil-mode 1)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;     Custom Evil keybindings       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For neotree bindings, see that section

  ;;Normal State
  (define-key evil-normal-state-map (kbd "M-j") 'next-buffer)
  (define-key evil-normal-state-map (kbd "M-k") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "M-<return>") 'term-toggle)

  ;;Insert State
  (define-key evil-insert-state-map (kbd "M-j") 'next-buffer)
  (define-key evil-insert-state-map (kbd "M-k") 'previous-buffer)
  (define-key evil-insert-state-map (kbd "M-<return>") 'term-toggle)

  ;;Motion State
  (define-key evil-motion-state-map (kbd "M-j") 'next-buffer)
  (define-key evil-motion-state-map (kbd "M-k") 'previous-buffer)
  (define-key evil-motion-state-map (kbd "M-<return>") 'term-toggle)
  
  ;; Ex Commands
  ;;;;;;;;;;;;;;;;;

  ;; Specific Modes
  ;;;;;;;;;;;;;;;;;

  ;; Buffer Menu 
  (define-key Buffer-menu-mode-map (kbd "M-j") 'next-buffer)
  (define-key Buffer-menu-mode-map (kbd "M-k") 'previous-buffer)
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

(use-package evil-collection
  :after evil
  :ensure t
  :init
  :config
  :custom
  (evil-collection-init))

;;;;;;;;;;;;;
;; Neotree ;;
;;;;;;;;;;;;;
;; Depreciating in favor of treemacs
;;(use-package neotree
;;  :ensure t
;;  :init
;;  :config
;;  ;;from: https://github.com/jaypei/emacs-neotree/issues/77
;;  (defun neo-open-file-hide (full-path &optional arg)
;;    "Open a file node and hides tree."
;;    (neo-global--select-mru-window arg)
;;    (find-file full-path)
;;    (neotree-hide))
;;
;;  (defun neotree-enter-hide (&optional arg)
;;    "Enters file and hides neotree directly"
;;    (interactive "P")
;;    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
;;   
;;  ;;Fancy Icons
;;  ;;You may need to execute all-the-icons-install-fonts manually
;;  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;
;;  ;;window width
;;  ;;from: https://github.com/jaypei/emacs-neotree/issues/262
;;  (eval-after-load "neotree"
;;    '(add-to-list 'window-size-change-functions
;;                  (lambda (frame)
;;                    (let ((neo-window (neo-global--get-window)))
;;                      (unless (null neo-window)
;;                        (setq neo-window-width (window-width neo-window)))))))
;;
;;  ;; Evil mode
;;  (define-key evil-normal-state-map (kbd "C-;") 'neotree-toggle) 
;;  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
;;  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-change-root)
;;  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
;;  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;;  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter-hide)
;;  ;; Mode specific stuff
;;  (define-key Buffer-menu-mode-map (kbd "C-;") 'neotree-toggle)
;;)

;;;;;;;;;;;;;;
;; Treemacs ;;
;;;;;;;;;;;;;;
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    ;;(treemacs-hide-gitignored-files-mode nil)
    )
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
   :map evil-normal-state-map
        ("C-;" . treemacs)
   :map treemacs-mode-map
        ("C-;" . treemacs)))
      
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;;(use-package treemacs-projectile
;;  :after (treemacs projectile)
;;  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Perspectives))

;;;;;;;;;;;;;;;;;;;; 
;; Other packages
;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :init
  :config
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
(setq tramp-default-method "ssh")
(setq tramp-copy-size-limit nil)

;; Get rid of ~ file clutter, but backups are good
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;;(if (string= (buffer-name) "*ansi-term*")
  (if (string= (buffer-name) "*eshell*")
    (delete-window)
    ;;(if (get-buffer "*ansi-term*") 
    (if (get-buffer "*eshell*") 
        (progn
          (split-window-vertically)
          (other-window 1 nil)
          ;;(switch-to-buffer "*ansi-term*"))
          (switch-to-buffer "*eshell*"))
        (progn
          (split-window-vertically)
          (other-window 1 nil)
          (if (string= window-system "x")
              (progn
                (setenv "PATH"
                        (concat
                         "$HOME/.local/bin" ":"
                         (getenv "PATH")))))
          (eshell)
          ;;(ansi-term (getenv "SHELL"))
          (setq show-trailing-whitespace nil)))))

;;;;;;;;;;;;;;;;;
;; Appearance  ;;
;;;;;;;;;;;;;;;;;
(if
    (and (display-graphic-p) (string-match "Microsoft"
         (with-temp-buffer (shell-command "uname -r" t)
                           (goto-char (point-max))
                           (delete-char -1)
                           (buffer-string))))
    (progn
      (message "Running under Linux subsystem for Windows")
      (cd "~/")
      (setq initial-frame-alist '(
                                  (width . 156)
                                  (height . 45)
                                  (left . 50)
                                  (top . 50)))
      (setq default-frame-alist '(
                                  (width . 156)
                                  (height . 45)
                                  (left . 50)
                                  (top . 50)))
    )
    (message "Not running under Linux subsystem for Windows")
)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Framesize
(when window-system (set-frame-size (selected-frame) (floor (* (display-pixel-width) 0.75)) (floor (* (display-pixel-height) 0.75))))

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
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (linum-relative all-the-icons powerline neotree evil use-package)))
 '(speedbar-default-position (quote left))
 '(speedbar-frame-parameters
   (quote
    ((minibuff:er)
     (width . 10)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

