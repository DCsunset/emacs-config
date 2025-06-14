;; dir related config

(setq-default delete-by-moving-to-trash t)

;; direnv
(use-package envrc
  :hook
  (after-init . envrc-global-mode))

;; make hl-line more distinguishable (for dired)
(use-package hl-line
  :custom-face
  (hl-line ((t (:background "#4a4a4a")))))

;;; projectile
(use-package projectile
  :hook
  (after-init . projectile-mode))

;;; for projectile-ripgrep
(use-package rg
  :defer t)

;;; dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-open-with)

(use-package dired-du
  :custom
  (dired-du-size-format t))

(defvar dired-hide-details-init t
  "Initial state of `dired-hide-details-mode'.")

(defun dired-toggle-hide-details-mode ()
  "Toggle dired-hide-details to all and future dired buffers."
  (interactive)
  (let ((state (not dired-hide-details-init)))
    (setq dired-hide-details-init state)
	  (dolist (buf (buffer-list))
	    (with-current-buffer buf
        (when (memq major-mode '(dired-mode wdired-mode))
          (dired-hide-details-mode (if state 1 -1)))))))

(use-package dired
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode (if dired-hide-details-init 1 -1))))
  (dired-mode . dired-omit-mode)
  :custom
  (dired-listing-switches "-ahl")
  ;; hide files starting with dot
  (dired-omit-files "\\`[.]")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t))

(defun dired-open-marked ()
  "Open all marked files with one command asynchronously."
  (interactive)
  (when-let* ((files (dired-get-marked-files))
              (cmd (or (cadr (assoc (car files)
                                    openwith-associations
                                    (lambda (regex file) (string-match regex file))))
                       (read-from-minibuffer "Open with one command: "))))
    (start-process-shell-command
     "dired-open-marked" nil
     (concat
      "exec nohup " cmd " "
      (mapconcat 'shell-quote-argument files " ")))))

;; dired mode (used with major state)
(modaled-define-substate "dired")
(modaled-define-keys
  :substates '("dired")
  :inherit
  `((modaled-normal-state-keymap . ("l" "k" "L" "K" "C-u" "C-d" "g" "|" "/" "n" "N")))
  :bind
  `((("j" "<left>") . ("up dir" . ,(hx :eval (call-interactively
                                                    (if (eq major-mode 'dired-mode)
                                                        #'dired-up-directory
                                                      #'dired-subtree-up)))))
    ((";" "<right>") . ("down dir" . ,(hx :eval (call-interactively
                                                    (if (eq major-mode 'dired-mode)
                                                        #'dired-find-file
                                                      (unless (dired-subtree--is-expanded-p)
                                                        (dired-sidebar-subtree-toggle))
                                                      #'dired-subtree-down)))))
    ;; TAB is also supported in `hx-toggle-visibility'
    (("RET" "<return>" "o") . ("open" . dired-find-file))
    ;; run ! or & to open them separately
    ("O" . ("open (in one command)" . dired-open-marked))
    ("i" . ("toggle details" . dired-toggle-hide-details-mode))
    ("I" . ("enable dired-du-mode" . dired-du-mode))
    ("h" . ("toggle hidden files" . dired-omit-mode))
    ("m" . ("mark" . ,(hx :region :eval dired-mark)))
    ("M-m" . ("mark by regexp" . dired-mark-files-regexp))
    ("M" . ("toggle all marks" . dired-toggle-marks))
    ("u" . ("unmark" . ,(hx :region :eval dired-unmark)))
    ("U" . ("unmark all" . dired-unmark-all-marks))
    ("d" . ("delete" . ,(hx :let (dired-deletion-confirmer #'y-or-n-p) :eval dired-do-delete)))
    ("D" . ("delete permanently" . ,(hx :let (delete-by-moving-to-trash nil) :eval dired-do-delete)))
    ("H" . ("kill (hide)" . dired-do-kill-lines))
    ("y" . ("copy" . dired-do-copy))
    ("r" . ("rename" . dired-do-rename))
    ("R" . ("replace" . dired-do-find-regexp-and-replace))
    ("c m" . ("chmod" . dired-do-chmod))
    ("c o" . ("chown" . dired-do-chown))
    ("c g" . ("chgrp" . dired-do-chgrp))
    ("c t" . ("touch" . dired-do-touch))
    ;; use C-s or C-a to exit wdired mode
    ("' n" . ("new file/dir" . (keymap)))
    ("' n f" . ("new file" . dired-create-empty-file))
    ("' n d" . ("new dir" . dired-create-directory))
    ("' w" . ("enable wdired mode" . dired-toggle-read-only))
    (("M-RET" "M-<return>") . ("open (other window)" . dired-find-file-other-window))))
(modaled-enable-substate-on-state-change
  "dired"
  :states '("major")
  :major '(dired-mode dired-sidebar-mode))

(defun dired-highlight ()
  "Highlight line for Dired."
  (let ((enabled (and (eq major-mode 'dired-mode)
                      modaled-dired-substate-mode)))
    (hl-line-mode (if enabled 1 -1))))

;; The major mode hook won't run when changing from wdired-mode to dired-mode
;; Must use minor mode hook (with modaled-initialize)
(add-hook 'modaled-dired-substate-mode-hook #'dired-highlight)

(use-package dired-sidebar
  :defer t
  :custom
  (dired-sidebar-theme 'nerd))

(use-package consult
  :defer t
  :config
  (defvar beframe-consult-source
    `( :name "Buffers (current frame)"
       :narrow ?F
       :category buffer
       :face consult-buffer
       :history beframe-history
       :items ,#'beframe-buffer-names
       :action ,#'switch-to-buffer
       :state ,#'consult--buffer-state))
  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

