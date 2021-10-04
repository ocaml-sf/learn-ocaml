;;; .emacs --- Emacs conf file -*- coding: utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de package.el, MELPA et use-package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de Tuareg, Merlin et Company

(use-package tuareg
  :ensure t
  :defer t
  :init
  (setq tuareg-opam-insinuate t))

;; Merlin would require OPAM ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config générale

(setq column-number-mode t
      line-number-mode t
      require-final-newline t)

;; Marquage des parenthèses
(load-library "paren")
(show-paren-mode 1)

;; Raccourcis C-c/C-x/C-v/C-z standards
;; au lieu de M-w/C-w/C-y/C-_ par défaut dans GNU Emacs
(cua-mode 1)
