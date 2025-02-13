;;; Syntax checking

(use-package flymake-cspell
  :init
  (setq flymake-cspell-diagnostic-type :note)
  :config
  (flymake-cspell-set-language-ids
   ("elisp" emacs-lisp-mode)
   ("org" org-mode)))

(use-package flymake
  :hook
  (after-init . global-flymake-mode))

(use-package sideline
  :hook
  (flymake-mode . sideline-mode)
  :custom
  (sideline-backends-right '(sideline-flymake)))

(define-globalized-minor-mode global-flymake-mode flymake-mode
  (lambda ()
    (unless (derived-mode-p
             'minibuffer-mode
             'vterm-mode
             'special-mode
             'org-agenda-mode
             'erc-mode)
      (flymake-cspell-setup)
      (flymake-mode 1))))

