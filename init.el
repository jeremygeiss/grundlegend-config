(setq inhibit-startup-message t)  ; Duh, Disable the startup message
(scroll-bar-mode -1)              ; Disable visible scrollbar
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(set-fringe-mode 10)              ; Give some breathing room
(menu-bar-mode -1)                ; Disable the menubar
(setq visible-bell t)             ; set up the visible bell

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; Column and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;disable line numbers for some modes (org mode)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;

;;Fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 170)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 170)
(set-face-attribute 'variable-pitch nil :font "Calibri" :height 170 :weight 'regular)

;;Initialize Package Sources
(require 'package)
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;;Ivy Rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;;Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;;themes
(use-package doom-themes
  :init (load-theme 'doom-city-lights t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
    :config
    (general-create-definer jkg/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (jkg/leader-keys
      "t"  '(:ignore t :which-key "Toggles")
      "tt" '(counsel-load-theme :which-key "choose theme")
      "tw" '(writeroom-mode :which-key "writeroom mode")
;;b for buffers
      "b"  '(:ignore t :which-key "Buffer")
      "be" '(eval-buffer :which-key "eval buffer") 
      "bN" '(evil-buffer-new :which-key "new buffer")
      "bn" '(evil-next-buffer :which-key "next buffer")
      "bp" '(evil-prev-buffer :which-key "prev buffer")
      "bd" '(kill-current-buffer :which-key "kill current buffer")
      "bs" '(counsel-switch-buffer :which-key "switch buffer")
;;f for files
      "f"  '(:ignore t :which-key "Files")
      "ff" '(counsel-find-file :which-key "find file")
      "fs" '(save-buffer :which-key "save buffer")
      "fn" '((lambda () (interactive) (counsel-find-file "~/Dropbox/Org/roam/")) :which-key "notes")
      "fc" '((lambda () (interactive) (counsel-find-file "~/Dropbox/Org/config.org")) :which-key "config.org")
      "fi" '((lambda () (interactive) (counsel-find-file "~/.emacs.d/init.el")) :which-key "init.el")
;;N for Notes
      "n"  '(:ignore t :which-key "Notes")
      "nf" '(org-roam-node-find :which-key "Find Node")
      "ni" '(org-roam-node-insert :which-key "Insert Node")
      "nl" '(org-roam-buffer-toggle :which-key "Toggle org-Roam Buffer")
      "ns" '(org-roam-db-sync :which-key "Org-Roam db Sync")
;; W for wWindows
        "w"  '(:ignore t :which-key "Window")
        "wv" '(evil-window-vsplit :which-key "vertical split window")
        "wc" '(evil-window-delete :which-key "close window")
        "wn" '(evil-window-new :which-key "new window")
        "wl" '(evil-window-move-far-right :which-key "move current window right")
;; A for applications (or modes or whatever)    
        "a"  '(:ignore t :which-key "app")
        "ac" '(org-capture :which-key "org capture templates")
        "ae" '(insert-esv-passage :which-key "insert esv passage")
        ))
                                          ;

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

;;org mode
(use-package org
  :hook (org-mode . jkg/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Dropbox/Org/agenda.org"))
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 2)
          ("tasks.org" :maxlevel . 2)))
  ;;save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (jkg/org-font-setup)
  ;; this if for the auto-tangle but it does not work
 ; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jkg/org-babel-tangle-config)))
  )

;;Org Setup
(defun jkg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;;Org Fonts Setup
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



;Org Bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;Org appearance
(defun jkg/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jkg/org-mode-visual-fill))

;;Org Babel Languages
;;add languages for org-babel, not sure if this is required or not
(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;Org Structure Templates
;;add some org structure templates for code blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;;Org Capture Templates

;    https://orgmode.org/manual/Using-capture.html
;    https://orgmode.org/manual/Capture-templates.html

;;define the global hot-key
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(
        ;;todo item capture
        ("t"           ; hotkey
         "Todo"        ; type
         entry         ; type
         (file+headline "~/Dropbox/Org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a") ; template
        ;; inbox item
        ("i"
         "Inbox capture"
         plain
         (file "~/Dropbox/Org/inbox.org")
         "\n* %U %^{Title}\n %?")
        ;; journal entry
        ("j"
         "Journal"
         entry
         (file+datetree "~/Dropbox/Org/journal.org")
         (file "~/Dropbox/Org/org-templates/journal.orgcaptmpl"))

        )
      )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/Org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package writeroom-mode)
(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))

;;insert-esv setup
 (use-package insert-esv
   :init
   (setq insert-esv-crossway-api-key "6d222046d13872627d6653c6325934474dfbd46f")
   (setq insert-esv-include-short-copyright 'true)
   (setq insert-esv-include-verse-numbers 'true)
   (setq insert-esv-include-headings 'true)
   ;;(setq insert-esv-include-passage-horizontal-lines 'false)
   ;;(setq insert-esv-line-length '50)
   :bind ("C-x C-e" . insert-esv-passage))
