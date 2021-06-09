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

;; Merlin would require OPAM
; (use-package merlin
;   :ensure t
;   :hook
;   ((tuareg-mode caml-mode) . merlin-mode)
;   :config
;   (setq merlin-command 'opam))
; 
; (use-package merlin-eldoc
;   :ensure t
;   :hook
;   ((tuareg-mode caml-mode) . merlin-eldoc-setup)
;   :bind (:map merlin-mode-map
;               ("C-c <C-left>" . merlin-eldoc-jump-to-prev-occurrence)
;               ("C-c <C-right>" . merlin-eldoc-jump-to-next-occurrence)))
; 
; (use-package company
;   :ensure t
;   :hook
;   ((tuareg-mode caml-mode) . company-mode)
;   :config
;   (bind-key "<backtab>" 'company-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config de Magit

; (use-package magit
;   :ensure t
;   :defer t
;   :config
;   (setq magit-diff-refine-hunk 'all)
;   :bind (("C-x g" . magit-status)
;          ("C-x M-g" . magit-dispatch-popup)))
; 
; (use-package magit-gitflow
;   :ensure t
;   :after magit
;   :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
; 
; ;; Protect against accident pushes to upstream
; (defadvice magit-push-current-to-upstream
;     (around my-protect-accidental-magit-push-current-to-upstream)
;   "Protect against accidental push to upstream.
; 
; Causes `magit-git-push' to ask the user for confirmation first."
;   (let ((my-magit-ask-before-push t))
;     ad-do-it))
; 
; (defadvice magit-git-push (around my-protect-accidental-magit-git-push)
;   "Maybe ask the user for confirmation before pushing.
; 
; Advice to `magit-push-current-to-upstream' triggers this query."
;   (if (bound-and-true-p my-magit-ask-before-push)
;       ;; Arglist is (BRANCH TARGET ARGS)
;       (if (yes-or-no-p (format "Push %s branch upstream to %s? "
;                                (ad-get-arg 0) (ad-get-arg 1)))
;           ad-do-it
;         (error "Push to upstream aborted by user"))
;     ad-do-it))
; 
; (ad-activate 'magit-push-current-to-upstream)
; (ad-activate 'magit-git-push)

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
