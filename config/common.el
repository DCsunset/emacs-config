;;;; Common packages and functions

;;; Common functions

(defun read-file (file)
  "Read contents of FILE and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ensure-dir (dir)
  "Ensure DIR exists unless it is an unsubstituted var.
Returns expanded dir name on success."
  (unless (and (string-prefix-p "@" dir)
               (string-suffix-p "@" dir))
    (let ((dir-path (expand-file-name dir)))
      (unless (file-exists-p dir-path)
        (mkdir dir-path t))
      dir-path)))


;;; UI config

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(electric-pair-mode 1)  ; auto complete pair
(setq-default visible-cursor nil ; for non-blinking cursor in console
              help-window-select t  ; always select help window to close it easily
              echo-keystrokes 0.01)  ; show presses keys immediately
(global-display-line-numbers-mode 1)
(global-so-long-mode 1)  ; faster loading for very long lines (e.g. minimized code)
(setq ring-bell-function #'ignore)  ; disable bell

;; Make jump history local to each window
(setq-default xref-history-storage 'xref-window-local-history)

;; put backup files in a dedicated dir
(setq-default backup-directory-alist `((".*" . ,(ensure-dir (concat user-emacs-directory "backups/")))))
(setq-default
 auto-save-file-name-transforms
 ;; strip directories (modified from default value)
 `(("\\`\\(/[^/]*:\\)?\\([^/]*/\\)*\\([^/]*\\)\\'" ,(ensure-dir (concat user-emacs-directory "auto-saves/" "\\3")) t)))

;; show paren when cursor is on the closing one rather than behind it except insert mode
(advice-add show-paren-data-function
            :around
            (lambda (orig-fun)
              (cond ((or (equal modaled-state "insert")
                         (looking-at "\\s("))
                     (funcall orig-fun))
                    ((looking-at "\\s)")
                     (save-excursion (forward-char 1) (funcall orig-fun))))))

;;; packages

;; don't use eval-when-compile to avoid bind-key errors
(require 'use-package)

(use-package nerd-icons
  :demand t)

(use-package modaled
  :demand t)

(use-package kkp
  :hook
  (after-init . global-kkp-mode))

;; faster loading for large files by chunks
(use-package vlf-setup
  :demand t
  :custom
  ;; it decides when vlf prompts
  (large-file-warning-threshold 10000000))

(use-package dash)

(use-package isearch-mb
  :hook
  (after-init . isearch-mb-mode)
  :bind
  (:map isearch-mb-minibuffer-map
        ("C-r" . consult-isearch-history))
  :config
  (add-to-list 'isearch-mb--with-buffer #'consult-isearch-history))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t))

(defun modaled-set-insert-state ()
  "Set insert modaled state."
  (interactive)
  (modaled-set-state "insert"))

;; Define normal states early so later substates can override them
(modaled-define-state "insert"
  :sparse t
  :no-suppress t
  :cursor-type 'bar)
(modaled-define-state "select"
  :cursor-type 'box)
(modaled-define-state "normal"
  :cursor-type 'box)
;; major-mode-specific state (keys defined in substates)
(modaled-define-state "major"
  :cursor-type 'box)

