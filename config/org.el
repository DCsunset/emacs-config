;;; org-mode config

;; template completion by inserting <KEY TAB
(use-package org-tempo
  :after org)

;; support exporting to markdown
(use-package ox-md
  :after org)

;; for CV export
(use-package org-moderncv
  :after org)

;; for CJK font alignment in table
(use-package valign
  :defer t
  :hook
  (org-mode . valign-mode))

(use-package org-appear
  :defer t
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

;;; gtd

(defvar gtd-directory (ensure-dir "@GTD_DIR@"))

(defun gtd-file (file)
  "Get path of a FILE in gtd dir."
  (file-name-concat gtd-directory file))
(defun gtd-save ()
  "Save all buffers in gtd."
  (interactive)
  (save-some-buffers t (lambda ()
                         (file-in-directory-p buffer-file-name gtd-directory))))

;; journal
(defvar journal-directory (ensure-dir "@JOURNAL_DIR@"))

(defun journal-file (file)
  "Get path of a FILE in journal dir."
  (file-name-concat journal-directory file))

;; org-mode
(use-package org
  :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda ()
                ;; turn off trailing whitespaces highlighting
                (setq show-trailing-whitespace nil)
                ;; don't pair angle bracket (used in tempo) in electric pair mode
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<)
                                   t
                                 ;; use the original pred
                                 (,electric-pair-inhibit-predicate c))))))
  (org-capture-mode . modaled-set-insert-state)
  :custom
  ;; use curly braces for sub/super script only so that underscore works normally
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-babel-python-mode 'python-ts-mode)
  ;; don't truncate lines (wrap lines instead)
  (org-startup-truncated nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :custom-face
  (org-headline-todo ((t (:foreground "#66acda"))))
  (org-headline-done ((t (:foreground "dark gray"))))
  :config
  ;; tree-sitter for org src buffer (map LANG to mode)
  (add-to-list 'org-src-lang-modes '("c" . c-ts))
  (add-to-list 'org-src-lang-modes '("c++" . c++-ts))
  (add-to-list 'org-src-lang-modes '("rust" . rust-ts))
  (add-to-list 'org-src-lang-modes '("go" . go-ts))
  (add-to-list 'org-src-lang-modes '("python" . python-ts))
  (add-to-list 'org-src-lang-modes '("js" . js-ts))
  (add-to-list 'org-src-lang-modes '("ts" . typescript-ts))
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("bash" . bash-ts))
  (add-to-list 'org-src-lang-modes '("zsh" . bash-ts))
  (add-to-list 'org-src-lang-modes '("sh" . bash-ts))
  (add-to-list 'org-src-lang-modes '("css" . css-ts))
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  (add-to-list 'org-src-lang-modes '("toml" . toml-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
  (add-to-list 'org-src-lang-modes '("dockerfile" . dockerfile-ts))
  ;; enable babel execution for languages in code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp t)
     (C . t)  ; for both C and C++
     (js . t)
     (dot . t)
     (haskell . t)
     (python . t)))

  (setq org-capture-templates
        `(("t" "gtd inbox" entry
           (file ,(gtd-file "inbox.org"))
           "* TODO %i%?")
          ("j" "journal" entry
           (file ,(journal-file "main.org"))
           "* %<%FT%T%z>\n\n%i%?"
           :empty-lines 1)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DOING(D)" "PLANNED(p)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("SOMEDAY" . "light sky blue")
          ("PLANNED" . "RoyalBlue1")
          ("WAITING" . "orchid")
          ("DOING" . "sandy brown")
          ("DONE" . "dark gray")
          ("CANCELLED" . "dark gray")))
  ;; apply faces to todo/done headline
  (setq org-fontify-done-headline t
        org-fontify-todo-headline t)
  (setq org-refile-targets
        `((,(gtd-file "actions.org") :maxlevel . 2)
          (,(gtd-file "archives.org") :maxlevel . 2)))
  ;; save buffer after capture or refile
  (defun save-after-capture-refile ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-buffer)))
  (advice-add 'org-capture-refile :after 'save-after-capture-refile))

(defun org-open (loc)
  "Open link at point based on LOC.
LOC can be `current' or `other'."
  (let* ((fn (cond
              ((eq loc 'current) #'find-file)
              ((eq loc 'other) #'find-file-other-window)
              (t (error "Wrong location"))))
         (setup-copy (copy-alist org-link-frame-setup))
         (org-link-frame-setup (push `(file . ,fn) setup-copy)))
    (org-open-at-point)))

(use-package org-agenda
  :defer t
  :config
  (defun my-org-agenda-prefix ()
    "Prefix string for org agenda items."
    (concat
     (s-truncate
      19
      (s-join "/" (remove nil `(,(org-get-category) ,(nth 1 (org-get-outline-path)))))
      "..")
     ":"))
  (setf (cdr (assoc 'todo org-agenda-prefix-format)) " %i %-20(my-org-agenda-prefix) ")
  (setq org-agenda-files
        `(,(gtd-file "inbox.org")
          ,(gtd-file "actions.org"))))


(use-package org-super-agenda
  :defer t
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        ;; only passed to next group if previous doesn't match
        '((:name "Inbox" :file-path "inbox\\.org\\'")
          (:todo "TODO")
          (:todo "WAITING")
          (:todo "DOING")
          (:todo "SOMEDAY")
          (:todo "PLANNED")))
  ;; disable the keymap on header
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(defun org-insert-category ()
  "Insert category property with current item."
  (interactive)
  (let ((item (org-entry-get nil "ITEM")))
    (org-set-property "CATEGORY" item)))

;; org-mode specific keybindings
(modaled-define-substate "org")
(modaled-define-keys
  :substates '("org")
  :bind
  `((("RET" "<return>") . ("org open" . ,(hx :eval (org-open 'current))))
    (("M-RET" "M-<return>") . ("org open (external)" . browse-url-at-point))
    ("' w" . ("org edit" . org-edit-special))
    ("' e" . ("org export" . org-export-dispatch))
    ("' i" . ("org insert" . (keymap)))
    ("' i T" . ("org insert template" . ,(hx :eval
                                             (modaled-set-state "insert")
                                             org-insert-structure-template)))
    ("' i i" . ("insert id" . org-id-get-create))
    ("' i c" . ("insert category" . org-insert-category))
    ("' i t" . ("insert tag" . org-set-tags-command))
    ("' i l" . ("insert denote link" . denote-link))
    ("' i d" . ("insert dblock" . (keymap)))
    ("' i d l" . ("denote link dblock" . denote-org-dblock-insert-links))
    ("' i d b" . ("denote backlink dblock" . denote-org-dblock-insert-backlinks))
    ("' i d f" . ("denote file dblock" . denote-org-dblock-insert-files))
    ("' d" . ("org dblock" . (keymap)))
    ("' d u" . ("org dblock update" . org-dblock-update))
    ("' l" . ("org toggle link display" . org-toggle-link-display))
    ("' c" . ("org capture" . org-capture))
    ("' t" . ("org todo" . ,(hx :region :eval org-todo)))
    ("' p" . ("org priority" . ,(hx :region :eval org-priority)))
    ("' <" . ("org promote" . ,(hx :region :eval org-do-promote)))
    ("' >" . ("org demote" . ,(hx :region :eval org-do-demote)))
    ("' J" . ("org promote subtree" . ,(hx :region :eval org-promote-subtree)))
    ("' :" . ("org demote subtree" . ,(hx :region :eval org-demote-subtree)))
    ("' L" . ("org promote subtree" . ,(hx :region :eval org-move-subtree-up)))
    ("' K" . ("org demote subtree" . ,(hx :region :eval org-move-subtree-down)))

    ;; gtd
    ("' g" . ("gtd" . (keymap)))
    ("' g r" . ("gtd refile" . ,(hx :region :eval org-refile gtd-save)))

    ;; denote
    ("' n" . ("denote" . (keymap)))
    ("' n n" . ("denote new" . denote))
    ("' n v" . ("denote visualise" . denote-explore-network))
    ("' n f" . ("denote find" . consult-denote-find))
    ("' n l" . ("denote links" . denote-find-link))
    ("' n b" . ("denote backlinks" . denote-find-backlink))
    ("' n s" . ("denote search" . consult-denote-grep))
    ("' n r" . ("denote rename" . denote-rename-file))))
;; enable org substate only for org-mode & not insert state
(modaled-enable-substate-on-state-change
  "org"
  :states '("normal" "select")
  :major '(org-mode))

;; org-agenda-mode specific keybindings
(modaled-define-substate "org-agenda")
(modaled-define-keys
  :substates '("org-agenda")
  :bind
  `(("r" . ("rebuild agenda view" . org-agenda-redo))
    ("' t" . ("agenda todo" . ,(hx :region :eval org-agenda-todo gtd-save)))
    ("' n" . ("agenda new gtd" . ,(hx :eval (org-capture nil "t"))))
    ("' p" . ("agenda priority" . ,(hx :region :eval org-agenda-priority gtd-save)))
    ("' r" . ("agenda refile" . ,(hx :region :eval org-agenda-refile gtd-save)))
    ("' i" . ("agenda insert" . (keymap)))
    ("' i t" . ("agenda insert tag" . org-agenda-set-tags))))
(modaled-enable-substate-on-state-change
  "org-agenda"
  :states '("normal" "select")
  :major '(org-agenda-mode))


;;; notes

(defvar notes-directory (ensure-dir "@NOTES_DIR@"))

(use-package denote
  :defer t
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :custom
  (denote-directory notes-directory)
  (denote-backlinks-show-context t)
  (denote-dired-directories (list notes-directory))
  (denote-dired-directories-include-subdirectories t)
  (denote-prompts '(title))
  (denote-save-buffers t)
  (denote-date-format "%FT%T%z")
  (denote-known-keywords '("life" "app" "game"))
  :custom-face
  (denote-faces-link ((t :foreground "turquoise" :underline t))))

(use-package denote-org
  :defer t)

(use-package denote-explore
  :defer t
  :custom
  (denote-explore-network-format 'd3.js)
  (denote-explore-network-d3-js "@D3_JS@"))

;; for searching
(use-package consult-denote
  :defer t
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep))

;;; org-present

(defun my/org-present-start ()
  "Set up org-present styles."
  (modaled-org-present-substate-mode 1)
  ;; Center text
  (visual-fill-column-mode 1)
  (display-line-numbers-mode -1)
  (org-display-inline-images)
  ;; Set a blank header line string to create blank space at the top
  (setq-local header-line-format " ")
  ;; Tweak font sizes
  ;; fixed-pitch means monospace font
  (setq-local face-remapping-alist
              '((default (:height 1.5) variable-pitch)
                (header-line (:height 4.0) variable-pitch)
                (org-document-title (:height 1.75) org-document-title variable-pitch)
                (org-code (:height 0.9) org-code fixed-pitch)
                (org-verbatim (:height 0.9) org-verbatim fixed-pitch)
                (org-block (:height 0.9) org-block fixed-pitch)
                (org-block-begin-line (:height 0.8) org-block-begin-line fixed-pitch))))

(defun my/org-present-end ()
  "Clean up org-present styles."
  (modaled-org-present-substate-mode -1)
  (visual-fill-column-mode -1)
  (display-line-numbers-mode 1)
  (org-remove-inline-images)
  ;; Clear the header line format by setting to `nil'
  (setq-local header-line-format nil)
  ;; Reset font
  (setq-local face-remapping-alist nil))

(use-package org-present
  :defer t
  :hook
  (org-present-mode . my/org-present-start)
  (org-present-mode-quit . my/org-present-end))

;; org-present-mode specific keybindings
(modaled-define-substate "org-present")
(modaled-define-keys
  :substates '("org-present")
  :bind
  `(("q" . ("quit org-present" . org-present-quit))
    ("<left>" . ("slide prev" . org-present-prev))
    ("<right>" . ("slide next" . org-present-next))
    ("S-<left>" . ("slide beginning" . org-present-beginning))
    ("S-<right>" . ("slide end" . org-present-end))))
(modaled-enable-substate-on-state-change
  "org-agenda"
  :states '("normal" "select")
  :minor '(org-present-mode))


(modaled-define-keys
  :states '("normal" "select" "major")
  :bind
  `(("SPC o" . ("org" . (keymap)))
    ;; org capture
    ("SPC o c" . ("org capture" . org-capture))
    ;; org present
    ("SPC o p" . ("org present" . ,(hx :eval (if (bound-and-true-p org-present-mode)
                                                 (org-present-quit)
                                               (org-present)))))
    ;; gtd (todo list)
    ("SPC o g" . ("gtd" . (keymap)))
    ("SPC o g l" . ("gtd list" . org-todo-list))
    ("SPC o g i" . ("gtd inbox" . ,(hx :eval (find-file (gtd-file "inbox.org")))))
    ("SPC o g a" . ("gtd actions" . ,(hx :eval (find-file (gtd-file "actions.org")))))
    ;; denote
    ("SPC o n" . ("denote" . (keymap)))
    ("SPC o n n" . ("denote new" . denote))
    ("SPC o n f" . ("denote find" . consult-denote-find))
    ;; journal
    ("SPC o j" . ("journal" . ,(hx :eval (find-file (journal-file "main.org")))))))

