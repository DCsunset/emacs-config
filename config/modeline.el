;; mode line

(use-package shrink-path
  :commands shrink-path-file)

(defmacro my-modeline-def-construct (symbol form)
  "Define my modeline segment with SYMBOL and FORM."
  (declare (indent defun))
  `(progn
     (defvar-local ,symbol '(:eval ,form))
     ;; need this propertize to work modeline
     (put ',symbol 'risky-local-variable t)))

(defun my-modeline-def-map (cmd1 cmd3)
  "Define mouse map for modeline.
CMD1 for \\`mouse-1' and CMD3 for \\`mouse-3'."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] cmd1)
    (define-key map [mode-line mouse-3] cmd3)
    map))

(defun surround-spaces (str)
  "Surround STR with spaces."
  (format " %s " str))

;;; modaled
(defface my-modeline-modaled-face
  '((t :weight bold :background "slate blue"))
  "Face for my-modeline-modaled.")
(my-modeline-def-construct my-modeline-modaled
  (propertize (surround-spaces (capitalize (substring modaled-state 0 1)))
              'face 'my-modeline-modaled-face))

;;; major mode
(my-modeline-def-construct my-modeline-major-mode
  (list
   (propertize (surround-spaces
                (capitalize (string-remove-suffix "-mode" (symbol-name major-mode))))
               'face 'bold)
   " "))

;;; encoding
(defvar my-modeline--encoding-map
  (my-modeline-def-map nil #'mode-line-change-eol))

(my-modeline-def-construct my-modeline-encoding
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
         (sys (coding-system-plist buffer-file-coding-system))
         (name (upcase (let ((ns (plist-get sys :name)))
                         (if (and (eq ns 'undecided)
                                  (plist-get sys :ascii-compatible-p))
                             "ascii"
                           (string-remove-prefix "prefer-" (symbol-name ns)))))))
    (list
     " "
     (propertize (pcase eol
                   (0 "LF")
                   (1 "CRLF")
                   (2 "CR")
                   (_ "AUTO"))
                 'help-echo (format "End of line: %s\nmouse-3: Cycle"
                                    (pcase eol
                                      (0 "Unix LF")
                                      (1 "DOS CRLF")
                                      (2 "Mac CR")
                                      (_ "Auto")))
                 'local-map my-modeline--encoding-map)
     " "
     (propertize name
                 'help-echo 'mode-line-mule-info-help-echo
                 'local-map mode-line-coding-system-map)
     " ")))

;;; input method
(defface my-modeline-input-method-face
  '((t :weight bold :foreground "#feacd0"))
  "Face for my-modeline-input-method.")
(defvar my-modeline--input-method-map
  (my-modeline-def-map nil #'set-input-method))

(my-modeline-def-construct my-modeline-input-method
  (when current-input-method
    (list
     " "
     (propertize current-input-method
                 'face 'my-modeline-input-method-face
                 'help-echo 'mode-line-mule-info-help-echo
                 'local-map my-modeline--input-method-map)
     " ")))

;;; buffer
(defun my-buffer-path ()
  "Get buffer path with home substituted."
  (when buffer-file-name
    (replace-regexp-in-string
     (concat "^" (expand-file-name "~"))
     "~"
     buffer-file-name)))

(defvar my-modeline--buffer-map
  (my-modeline-def-map
   (lambda () (interactive)
     (when-let ((path (my-buffer-path)))
       (xclip-set-selection 'clipboard path)))
   nil))

(my-modeline-def-construct my-modeline-buffer
  (let* ((path (my-buffer-path))
         (name (if path
                   (shrink-path-file path t)
                 (buffer-name))))
    (list
     ;; read-only
     (if buffer-read-only
         (concat " " (nerd-icons-faicon "nf-fa-lock"))
       "")
     ;; name
     (propertize (surround-spaces name)
                 'face (when (and path
                                  (buffer-modified-p))
                         'bold-italic)
                 'help-echo path
                 'local-map my-modeline--buffer-map)
     " "
     ;; column
     (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-split_horizontal") (current-column))
                 'help-echo "Column")
     ;; region
     (if (equal modaled-state "select")
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-primitive_square") (hx-region-size))
                     'help-echo "Region size")
       ""))))

;;; eglot
(defvar-local my-modeline-eglot nil)
(put 'my-modeline-eglot 'risky-local-variable t)
(defun my-modeline--update-eglot ()
  "Update eglot construct."
  (let* ((server (and (eglot-managed-p) (eglot-current-server)))
         (name (and server (plist-get (eglot--server-info server) :name)))
         (last-error (and server (jsonrpc-last-error server)))
         (face (cond (last-error 'error)
                     (server 'success)
                     (t 'shadow)))
         (help-echo (cond (last-error (format "Eglot error: %s" last-error))
                          (name (format "Eglot connected: %s" name))
                          (server "Eglot connected")
                          (t "Eglot disconnected"))))
    (setq my-modeline-eglot
          (propertize (surround-spaces (nerd-icons-octicon "nf-oct-rocket"))
                      'face face
                      'help-echo help-echo))))
(add-hook 'eglot-managed-mode-hook #'my-modeline--update-eglot)

;;; flymake
(defun my-modeline--flymake-count (type)
  "Count number of flymake errors for specific TYPE."
  (number-to-string
   (--reduce-from (+ acc
                     (if (eq type (flymake-diagnostic-type it)) 1 0))
                  0
                  (flymake-diagnostics))))

(defvar my-modeline--flymake-error-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:error)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:error)))))
(defvar my-modeline--flymake-warning-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:warning)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:warning)))))
(defvar my-modeline--flymake-note-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:note)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:note)))))

(my-modeline-def-construct my-modeline-flymake
  (when (bound-and-true-p flymake-mode)
    (list
     " "
     (propertize
      (concat (propertize (nerd-icons-codicon "nf-cod-error")
                          'face 'error)
              " "
              (my-modeline--flymake-count :error))
      'local-map my-modeline--flymake-error-map)
     " "
     (propertize
      (concat (propertize (nerd-icons-codicon "nf-cod-warning")
                          'face 'warning)
              " "
              (my-modeline--flymake-count :warning))
      'local-map my-modeline--flymake-warning-map)
     " "
     (propertize
      (concat (nerd-icons-codicon "nf-cod-info")
              " "
              (my-modeline--flymake-count :note))
      'local-map my-modeline--flymake-note-map)
     " ")))

;;; input method


;;; git
(defvar-local my-modeline--git-info nil
  "A list of (dirty branch); use a variable for caching.")
(defun my-modeline--update-git-info ()
  "Update cached git info."
  (setq my-modeline--git-info
        (when (and vc-mode buffer-file-name)
          (let ((status (shell-command-to-string "git status --porcelain")))
            (list
             ;; dirty
             (not (string-empty-p status))
             ;; branch
             (car (vc-git-branches)))))))
;; update info only on file init and save for better performance
;; need to add it to the end to make sure vc-mode is updated first
(add-hook 'find-file-hook #'my-modeline--update-git-info 100)
(add-hook 'after-save-hook #'my-modeline--update-git-info 100)

(defvar my-modeline--git-map
  (my-modeline-def-map #'magit-status nil))

(defface my-modeline-git-face
  '((t :foreground "light sky blue"))
  "Face for my-modeline-git.")
(my-modeline-def-construct my-modeline-git
  (pcase my-modeline--git-info
    (`(,dirty ,branch)
     (propertize
      (surround-spaces
       (concat (nerd-icons-devicon (if dirty
                                       "nf-dev-git_compare"
                                     "nf-dev-git_branch"))
               " "
               branch))
      'face 'my-modeline-git-face
      'help-echo (format "Git status: %s\nmoduse-1: Open magit"
                         (if dirty "dirty" "clean"))
      'local-map my-modeline--git-map))))


;;; mode line format
;; must use setq-default as it will become buffer local when set
(setq-default mode-line-format
              '("%e"
                my-modeline-modaled
                my-modeline-buffer

                mode-line-format-right-align  ; emacs 30

                my-modeline-eglot
                my-modeline-flymake
                my-modeline-encoding
                my-modeline-input-method
                my-modeline-git
                my-modeline-major-mode))

