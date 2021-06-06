(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; (setq package-native-compile t)

(use-package doom-themes :defer t)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

(defun my/apply-theme (appearance)
 "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
 (pcase appearance
   ('light (load-theme 'tango-dark t)) ;; or 'tango
   ('dark (load-theme 'tango-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;(add-to-list 'default-frame-alist '(undecorated . 1))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(setq comp-async-report-warnings-errors nil)

(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)

(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height efs/default-font-size :weight 'regular)

(use-package all-the-icons)

(use-package dashboard
 :init      ;; tweak dashboard config before loading it
 (add-hook 'after-init-hook 'dashboard-refresh-buffer)
 (setq dashboard-set-heading-icons t)
 (setq dashboard-set-file-icons t)
 ;; (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
 ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
 ;; (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
 ;; (setq dashboard-banner-logo-title "Welcome")
 (setq dashboard-startup-banner 'logo)
 (setq dashboard-center-content nil)
 (setq dashboard-items '((recents . 5)
                         (agenda . 5 )
                         (bookmarks . 3)
                         (projects . 3)
                         (registers . 3)))
 :config
 (dashboard-setup-startup-hook)
 (dashboard-modify-heading-icons '((recents . "file-text")
                             (bookmarks . "book"))))
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; (setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 6))) ;; how many lines at a time
;; (setq mouse-wheel-progressive-speed nil) ;; accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq inhibit-compacting-font-caches t)

;; (use-package ivy
  ;;   :diminish
  ;;   :bind (("C-s" . swiper)
  ;;          :map ivy-minibuffer-map
  ;;          ("TAB" . ivy-alt-done)
  ;;          ("C-l" . ivy-alt-done)
  ;;          ("C-j" . ivy-next-line)
  ;;          ("C-k" . ivy-previous-line)
  ;;          :map ivy-switch-buffer-map
  ;;          ("C-k" . ivy-previous-line)
  ;;          ("C-l" . ivy-done)
  ;;          ("C-d" . ivy-switch-buffer-kill)
  ;;          :map ivy-reverse-i-search-map
  ;;          ("C-k" . ivy-previous-line)
  ;;          ("C-d" . ivy-reverse-i-search-kill))
  ;;   :config
  ;;   (ivy-mode 1))

  ;; (use-package ivy-rich
  ;;   :after ivy
  ;;   :init
  ;;   (ivy-rich-mode 1))

  ;; (use-package counsel
  ;;   :bind (("C-M-j" . 'counsel-switch-buffer)
  ;;          :map minibuffer-local-map
  ;;          ("C-r" . 'counsel-minibuffer-history))
  ;;   :custom
  ;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  ;;   :config
  ;;   (counsel-mode 1))
  (use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

  ;; Individual history elements can be configured separately
  ;;(put 'minibuffer-history 'history-length 25)
  ;;(put 'evil-ex-history 'history-length 50)
  ;;(put 'kill-ring 'history-length 25))

  (defun js/minibuffer-backward-kill (arg)
  "When miibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))

(use-package orderless
:straight t
:init
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion))))))

;; (setq ivy-initial-inputs-alist nil)

;; (use-package ivy-posframe
;;   :init
;;   (setq ivy-posframe-display-functions-alist
;;     '((swiper                     . ivy-posframe-display-at-point)
;;       (complete-symbol            . ivy-posframe-display-at-point)
;;       (counsel-M-x                . ivy-display-function-fallback)
;;       (counsel-esh-history        . ivy-posframe-display-at-window-center)
;;       (counsel-describe-function  . ivy-display-function-fallback)
;;       (counsel-describe-variable  . ivy-display-function-fallback)
;;       (counsel-find-file          . ivy-display-function-fallback)
;;       (counsel-recentf            . ivy-display-function-fallback)
;;       (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
;;       (dmenu                      . ivy-posframe-display-at-frame-top-center)
;;       (nil                        . ivy-posframe-display))
;;     ivy-posframe-height-alist
;;     '((swiper . 20)
;;       (dmenu . 20)
;;       (t . 10)))
;;   :config
;;   (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

;; (use-package ivy-prescient
;;   :after counsel
;;   :custom
;;   (ivy-prescient-enable-filtering nil)
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   ;(prescient-persist-mode 1)
;;   (ivy-prescient-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Dev")
    (setq projectile-project-search-path '("~/Dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package evil-commentary)
(evil-commentary-mode)

;; (defun efs/org-font-setup ()
;;   ;; Replace list hyphen with dot
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;   ;; Set faces for heading levels
;;   (dolist (face '((org-level-1 . 1.2)
;;                   (org-level-2 . 1.1)
;;                   (org-level-3 . 1.05)
;;                   (org-level-4 . 1.0)
;;                   (org-level-5 . 1.1)
;;                   (org-level-6 . 1.1)
;;                   (org-level-7 . 1.1)
;;                   (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face)))

;;   ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;;   (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
;;   (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; (defun efs/org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode 1)
;;   (visual-line-mode 1))

;; (use-package org
;;   :hook (org-mode . efs/org-mode-setup)
;;   :config
;;   (setq org-ellipsis " ▾")

;;   (setq org-agenda-start-with-log-mode t)
;;   (setq org-log-done 'time)
;;   (setq org-log-into-drawer t)

;;   (require 'org-habit)
;;   (add-to-list 'org-modules 'org-habit)
;;   (setq org-habit-graph-column 60)

;;   (setq org-todo-keywords
;;     '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
;;       (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;;   (setq org-refile-targets
;;     '(("Archive.org" :maxlevel . 1)
;;       ("Tasks.org" :maxlevel . 1)))

;;   ;; Save Org buffers after refiling!
;;   (advice-add 'org-refile :after 'org-save-all-org-buffers)

;;   (setq org-tag-alist
;;     '((:startgroup)
;;        ; Put mutually exclusive tags here
;;        (:endgroup)
;;        ("@errand" . ?E)
;;        ("@home" . ?H)
;;        ("@work" . ?W)
;;        ("agenda" . ?a)
;;        ("planning" . ?p)
;;        ("publish" . ?P)
;;        ("batch" . ?b)
;;        ("note" . ?n)
;;        ("idea" . ?i)))

;;   ;; Configure custom agenda views
;;   (setq org-agenda-custom-commands
;;    '(("d" "Dashboard"
;;      ((agenda "" ((org-deadline-warning-days 7)))
;;       (todo "NEXT"
;;         ((org-agenda-overriding-header "Next Tasks")))
;;       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

;;     ("n" "Next Tasks"
;;      ((todo "NEXT"
;;         ((org-agenda-overriding-header "Next Tasks")))))

;;     ("W" "Work Tasks" tags-todo "+work-email")

;;     ;; Low-effort next actions
;;     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
;;      ((org-agenda-overriding-header "Low Effort Tasks")
;;       (org-agenda-max-todos 20)
;;       (org-agenda-files org-agenda-files)))

;;     ("w" "Workflow Status"
;;      ((todo "WAIT"
;;             ((org-agenda-overriding-header "Waiting on External")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "REVIEW"
;;             ((org-agenda-overriding-header "In Review")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "PLAN"
;;             ((org-agenda-overriding-header "In Planning")
;;              (org-agenda-todo-list-sublevels nil)
;;              (org-agenda-files org-agenda-files)))
;;       (todo "BACKLOG"
;;             ((org-agenda-overriding-header "Project Backlog")
;;              (org-agenda-todo-list-sublevels nil)
;;              (org-agenda-files org-agenda-files)))
;;       (todo "READY"
;;             ((org-agenda-overriding-header "Ready for Work")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "ACTIVE"
;;             ((org-agenda-overriding-header "Active Projects")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "COMPLETED"
;;             ((org-agenda-overriding-header "Completed Projects")
;;              (org-agenda-files org-agenda-files)))
;;       (todo "CANC"
;;             ((org-agenda-overriding-header "Cancelled Projects")
;;              (org-agenda-files org-agenda-files)))))))

;; (define-key global-map (kbd "C-c j")
;;   (lambda () (interactive) (org-capture nil "jj")))

;; (efs/org-font-setup))

;; (use-package org-tempo
;;   :ensure nil)

;; (use-package org-bullets
;;   :after or
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tutor)

(use-package general
  :config
  (general-evil-setup t))

(nvmap :keymaps 'override :prefix "SPC"
       "SPC"   '(counsel-M-x :which-key "M-x")
       "c c"   '(compile :which-key "Compile")
       "c C"   '(recompile :which-key "Recompile")
       "h r r" '((lambda () (interactive) (load-file "~/.emacs.default/init.el")) :which-key "Reload emacs config")
       "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines"))
(nvmap :keymaps 'override :prefix "SPC"
       "m *"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
       "m +"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
       "m ."   '(counsel-org-goto :which-key "Counsel org goto")
       "m e"   '(org-export-dispatch :which-key "Org export dispatch")
       "m f"   '(org-footnote-new :which-key "Org footnote new")
       "m h"   '(org-toggle-heading :which-key "Org toggle heading")
       "m i"   '(org-toggle-item :which-key "Org toggle item")
       "m n"   '(org-store-link :which-key "Org store link")
       "m o"   '(org-set-property :which-key "Org set property")
       "m t"   '(org-todo :which-key "Org todo")
       "m x"   '(org-toggle-checkbox :which-key "Org toggle checkbox")
       "m B"   '(org-babel-tangle :which-key "Org babel tangle")
       "m I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
       "m T"   '(org-todo-list :which-key "Org todo list")
       "o a"   '(org-agenda :which-key "Org agenda")
       )

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(menu-bar-mode 1)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))
