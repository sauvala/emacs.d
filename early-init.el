;;; early-init.el -*- lexical-binding: t; -*-
;;; Taken from Doom Emacs guides:
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Test this for native comp
(setq comp-deferred-compilation nil
      native-comp-deferred-compilation nil)

(setq load-prefer-newer noninteractive)
(setq package-enable-at-startup nil)

(defvar js--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist js--file-name-handler-alist)))
