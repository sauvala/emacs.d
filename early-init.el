;;; early-init.el -*- lexical-binding: t ; eval: (view-mode -1) -*-

(defvar js/gc-cons-threshold 16777216) ; 16mb
(setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold js/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun js/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun js/restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold js/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'js/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'js/restore-garbage-collection-h)

(setq read-process-output-max 65536) ; (* 64 1024)

(setq comp-deferred-compilation nil
    native-comp-deferred-compilation nil)

(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(defvar js--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist js--file-name-handler-alist)))

(push '(menu-bar-lines . 0)  default-frame-alist)
(push '(tool-bar-lines . 0)  default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(set-fringe-mode 10)
(setq visible-bell t
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode)

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defvar js/default-font-size 150)
(defvar js/default-variable-font-size 150)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono"))

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height js/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height js/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height js/default-variable-font-size
                    :weight 'light)

(setq straight-use-package-by-default t
      use-package-always-defer t
      use-package-always-ensure t
      straight-cache-autoloads t
      use-package-verbose t)

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

(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(use-package esup
  :commands esup)

(use-package benchmark-init
  :demand t
  :straight (benchmark-init :host github
                            :repo "nasyxx/benchmark-init-el")
  :config
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :config
  (gcmh-mode 1))

(provide 'early-init)
