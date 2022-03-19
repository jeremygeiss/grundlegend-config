
(setq inhibit-startup-message t)

(scroll-bar-mode -1)         ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)              ; Disable tooltips
(set-fringe-mode 10)         ; Give some breathing room

; (menu-bar-mode -1)         ; Disable the menubar

;;windows fonts
;; Set the fixed pitch face: for windows: "Consolas" :height 170 
(set-face-attribute 'default nil :font "Consolas" :height 170)
(set-face-attribute 'fixed-pitch nil :font "Consolas" :height 170)
;; Set the variable pitch face: for windows: "Calibri" :height 170 
(set-face-attribute 'variable-pitch nil :font "Calibri" :height 170 :weight 'regular)

;; manjaro linux fonts
;;(set-face-attribute 'default nil :font "Noto Mono" :height 140)
;;(set-face-attribute 'fixed-pitch nil :font "Noto Mono" :height 140)
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

;; set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Column and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;;disable line numbers for some modes (org mode)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)
(setq package-archives '(
			  ("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; install ivy
(use-package ivy
  :diminish
;  :bind (("C-s" . swiper)
;         :map ivy-minibuffer-map
;         ("TAB" . ivy-alt-done)	
;         ("C-l" . ivy-alt-done)
;         ("C-j" . ivy-next-line)
;         ("C-k" . ivy-previous-line)
;         :map ivy-switch-buffer-map
;         ("C-k" . ivy-previous-line)
;         ("C-l" . ivy-done)
;         ("C-d" . ivy-switch-buffer-kill)
;         :map ivy-reverse-i-search-map
;         ("C-k" . ivy-previous-line)
;         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;(use-package all-the-icons)

;(use-package doom-modeline
;  :ensure t
;  :init (doom-modeline-mode 1)
;  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-city-lights t))
;; doom-city-lights
;; doom-palenight

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; which-key
; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer jkg/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (jkg/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "b"  '(:ignore t :which-key "buffer")
    "be" '(eval-buffer :which-key "eval buffer") 
    "bN" '(evil-buffer-new :which-key "new buffer")
    "bn" '(evil-next-buffer :which-key "next buffer")
    "bp" '(evil-prev-buffer :which-key "prev buffer")
    "bd" '(kill-current-buffer :which-key "kill current buffer")
    "bs" '(counsel-switch-buffer :which-key "switch buffer")
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save buffer")
    "fn" '((lambda () (interactive) (counsel-find-file "C:/Users/JeremyGeiss/OneDrive - Genuine Technology Group, Inc/Notes/")) :which-key "notes")
    "fc" '((lambda () (interactive) (counsel-find-file "C:/Users/JeremyGeiss/AppData/Roaming/.emacs.d/init.el")) :which-key "init.el")
    "w"  '(:ignore t :which-key "window")
    "wv" '(evil-window-vsplit :which-key "vertical split window")
    "wc" '(evil-window-delete :which-key "close window")
    "wn" '(evil-window-new :which-key "new window")
  ) )

;quit keys, killprompt quit emacs, etc
;windows and frames



(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defun jkg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun jkg/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Calibri" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . jkg/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("C:/Users/JeremyGeiss/OneDrive - Genuine Technology Group, Inc/org/tasks.org"))
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 2)
	  ("tasks.org" :maxlevel . 2)))
  ;;save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (jkg/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun jkg/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jkg/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))












;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(package-selected-packages
   '(evil-leader evil-collection evil general helpful doom-themes counsel ivy-rich which-key jetbrains-darcula-theme rainbow-delimiters doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
