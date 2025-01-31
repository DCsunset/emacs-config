;;/co -*- lexical-binding: t; -*-

;; zoom globally instead of per buffer
(use-package default-text-scale
  :commands default-text-scale-mode
  :bind
  (:map default-text-scale-mode-map
        ("C-=" . default-text-scale-increase)
        ("C--" . default-text-scale-decrease)
        ("C-0" . default-text-scale-reset))
  :init
  (default-text-scale-mode 1)
  :custom
  (default-text-scale-amount 10))
;; Set default font size for GUI
(set-frame-font "Monospace 12" nil t)

(use-package transient
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-one)))

(use-package minions
  :commands minions-minor-modes-menu)

;; rainbow-mode highlights color codes
(use-package rainbow-mode
  :commands rainbow-mode
  :hook (after-init  . global-rainbow-mode))
;; enable it by default
(define-globalized-minor-mode global-rainbow-mode
  rainbow-mode
  (lambda () (rainbow-mode 1))
  :group 'rainbow)

(use-package hl-todo
  :commands global-hl-todo-mode
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . "red")
          ("FIXME" . "red")
          ("BUG" . "red")
          ("WAITING" . "orchid")
          ("HACK" . "sandy brown")))
  (global-hl-todo-mode))

(use-package dashboard
  :commands dashboard-setup-startup-hook
  :init
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; show info about the packages loaded and the init time
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

;; xclip (to support clipboard in Xorg & Wayland)
;; need xclip or wl-clipboard-rs
(use-package xclip)

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi :no-confirm))

;; tabs
(use-package centaur-tabs
  :demand t
  :custom-face
  (centaur-tabs-active-bar-face ((t :background "#c4569e")))
  :custom
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  :config
  ;; hide in dired sidebar
  (setq centaur-tabs-hide-predicate
        (lambda () (memq major-mode '(dired-sidebar-mode
                                      org-agenda-mode))))
  (centaur-tabs-mode 1))

;; Fix from https://github.com/ema2159/centaur-tabs/issues/127#issuecomment-1126913492
(defun fix-centaur-tabs ()
  "Fix tabs in terminal."
  (centaur-tabs-mode -1)
  (centaur-tabs-mode)
  (centaur-tabs-headline-match))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (fix-centaur-tabs)))
              (fix-centaur-tabs)))

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.01)  ; show desc immediately
  (which-key-sort-order 'which-key-description-order)
  :config
  (which-key-mode))

;;; Completion UI in minibuffer
(use-package vertico
  :demand t
  ;; Tidy shadowed file names in find-file
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  ;; call mode in :init to enable it immediately
  (vertico-mode))

;;; Enable rich annotations in minibuffer completion
(use-package marginalia
  :demand t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :config
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;; Fuzzy completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; icons for completion UI in minibuffer
(use-package nerd-icons-completion
  :demand t
  :config
  (nerd-icons-completion-mode))

;;; company completion
(use-package company
  :commands (company-manual-begin company-abort)
  :hook (after-init  . global-company-mode)
  :bind (:map company-active-map
              ;; complete selection using tab
              ;; (must use TAB for terminal and <tab> for gui)
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("S-TAB" . company-complete-common)
              ("S-<tab>" . company-complete-common)
              ;; unbind following keys to make the default keybindings work
              ("RET" . nil)
              ("<return>" . nil)
              ("C-w" . nil)
              ("<escape>" . (lambda () (interactive) (company-abort) (modaled-set-main-state))))
  :custom
  ;; complete on 2 chars instead of 3
  (company-minimum-prefix-length 2)
  ;; prevent completing with wrong cases
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  ;; cycle candidates' selection
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-transformers '(company-sort-prefer-same-case-prefix)))

;; Manage popup window
(use-package popwin
  :demand t
  :config
  (popwin-mode 1)
  ;; make help window stick around
  (add-to-list 'popwin:special-display-config '(help-mode :stick t))
  (add-to-list 'popwin:special-display-config '("\\*eldoc.*\\*" :regexp t :noselect t)))


;; git
(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;; use margin mode to support terminal emacs
  (diff-hl-margin-mode 1))

;;; inline git blame
(use-package blamer
  :custom
  (blamer-idle-time 0.2)
  (blamer-min-offset 70))

(use-package magit
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; refresh magit after saving buffer
  (after-save . magit-after-save-refresh-status)
  (git-commit-mode . modaled-set-insert-state)
  :config
  ;; always show recent commits instead of unpushed commits
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-recent-commits
                          #'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  ;; always expand untracked & recent commits section
  (setf (alist-get 'recent magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'untracked magit-section-initial-visibility-alist) 'show))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

;; magit-status-mode specific keybindings
(modaled-define-substate "magit-status")
(modaled-define-keys
  :substates '("magit-status")
  :bind
  `(("a" . ("stage" . magit-stage))
    ("A" . ("stage all" . magit-stage-modified))
    ("u" . ("unstage" . magit-unstage))
    ("U" . ("unstage all" . magit-unstage-all))
    ("z" . ("stash" . magit-stash))
    ("P" . ("push changes" . magit-push))
    ("F" . ("pull changes" . magit-pull))
    ("d" . ("diff" . magit-diff))
    ("D" . ("discard" . magit-discard))
    ("r" . ("refresh" . magit-refresh))
    ("R" . ("rebase" . magit-rebase))
    ("c" . ("commit" . magit-commit))))
(modaled-enable-substate-on-state-change
  "magit-status"
  :states '("normal" "select")
  :major '(magit-status-mode))


;;; vterm (insert as default state)
(use-package vterm
  :commands (vterm-reset-cursor-point vterm--self-insert)
  :hook
  ((vterm-mode special-mode) . (lambda ()
                                 ;; turn off trailing whitespaces highlighting
                                 (setq show-trailing-whitespace nil))))

;; vterm specific keybindings
(modaled-define-substate "vterm")
; reset to the right position when entering insert mode
(modaled-define-keys
  :substates '("vterm")
  :bind
  `((("i" "a" "I" "A") . ("insert" . ,(hx :eval hx-no-sel (modaled-set-state "insert") vterm-reset-cursor-point)))))
;; passing control keys to terminal instead of modifying buffer (buffer is read-only)
(modaled-define-substate "vterm-insert"
  :sparse t
  :no-suppress t)
(modaled-define-keys
  :substates '("vterm-insert")
  ; pass keys to terminal
  :bind
  `((("C-w" "C-u" "C-c") . ("passthrough" . vterm--self-insert))))
;; enable substate only for vterm-mode & not insert state
(modaled-enable-substate-on-state-change
  "vterm"
  :states '("normal" "select")
  :major '(vterm-mode))
(modaled-enable-substate-on-state-change
  "vterm-insert"
  :states '("insert")
  :major '(vterm-mode))

(use-package vterm-toggle
  :commands vterm-toggle
  :config
  ;; show vterm buffer in bottom side
  ;; must be executed after the pkg is load to make sure vterm-buffer-name is defined
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;;; beframe
(use-package beframe
  :hook (after-init . beframe-mode))

;;; undo
(use-package undo-fu)
(use-package vundo)

(modaled-define-substate "vundo")
(modaled-define-keys
  :substates '("vundo")
  :bind
  `(("j" . ("backward" . vundo-backward))
    (";" . ("forward" . vundo-forward))
    ("k" . ("down" . vundo-next))
    ("l" . ("up" . vundo-previous))
    ("w" . ("next fork" . vundo-stem-root))
    ("b" . ("previous fork". vundo-next-root))
    ("g ;" . ("end". vundo-stem-end))
    ("m" . ("mark". vundo-diff-mark))
    ("u" . ("unmark". vundo-diff-unmark))
    ("d" . ("diff". vundo-diff))
    ("q" . ("quit". vundo-quit))))
(modaled-enable-substate-on-state-change
  "vundo"
  :states '("normal" "select")
  :major '(vundo-mode))


;;; erc
(use-package erc
  :hook
  (erc-mode . (lambda () (setq show-trailing-whitespace nil)))
  (modaled-insert-state-mode . (lambda ()
                                 ;; reset cursor after entering insert state for erc
                                 (when (and (eq major-mode 'erc-mode)
                                            modaled-insert-state-mode
                                            (not (eq (line-number-at-pos)
                                                     (line-number-at-pos (point-max)))))
                                   (goto-char (point-max)))))
  :config
  ;; switch to buffer after joining (:custom doesn't work)
  (setq erc-buffer-display 'buffer)
  ;; enable desktop notification when mentioned
  (custom-set-variables
   '(erc-modules (push 'notifications erc-modules))))

(modaled-define-substate "erc-insert"
  :sparse t
  :no-suppress t)
(modaled-define-keys
  :substates '("erc-insert")
  :bind
  `((("C-p" "<up>") . ("previous cmd" . erc-previous-command))
    (("C-n" "<down>") . ("next cmd" . erc-next-command))))
(modaled-enable-substate-on-state-change "erc-insert"
  :states '("insert")
  :major '(erc-mode))

