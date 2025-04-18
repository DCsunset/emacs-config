:;; language related config

;;; editor config
;; mode for *scratch* buffer
(setq initial-major-mode 'text-mode)
;; Use setq-default because these variables are buffer-local
(setq-default major-mode 'text-mode  ; use text-mode by default instead of fundamental-mode
              indent-tabs-mode nil  ; use spaces for indentation
              standard-indent 2
              tab-width 2
              sh-indentation 2
              python-indent-offset 2
              css-indent-offset 2
              js-indent-level 2
              typescript-ts-mode-indent-offset 2
              go-ts-mode-indent-offset 2
              lua-indent-level 2
              rust-ts-mode-indent-offset 2
              ess-fancy-comments nil  ; indent comment line as normal code
              select-enable-clipboard nil  ; don't paste to clipboard by default
              interprogram-paste-function nil  ; don't copy clipboard to kill-ring
              interprogram-cut-function nil  ; don't copy kill-ring to clipboard
              show-trailing-whitespace t)

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode)
  ;; set first line as header automatically
  (csv-mode . csv-header-line))

(use-package hurl-mode
  :mode "\\.hurl\\'"
  :config
  (modaled-define-substate "hurl")
  (modaled-define-keys
    :substates '("hurl")
    :bind
    `(("' T" . ("hurl test all" . hurl-mode-test-request-file))
      ("' t" . ("hurl test single" . hurl-mode-test-request-single))
      ("' X" . ("hurl run all" . hurl-mode-send-request-file))
      ("' x" . ("hurl run single" . hurl-mode-send-request-single))))
  (modaled-enable-substate-on-state-change
    "hurl"
    :states '("normal" "select")
    :major '(hurl-mode)))

(use-package caddyfile-mode
  :mode "Caddyfile\\'"
  :hook
  (caddyfile-mode . (lambda ()
                      ; overwrite indent settings in caddyfile-mode
                      (setq-local tab-width 2
                                  indent-tabs-mode nil))))

(use-package beancount
  :mode
  ("\\.beancount\\'" . beancount-mode))

(use-package jtsx
  :mode
  ("\\.jsx\\'" . jtsx-jsx-mode)
  ("\\.tsx\\'" . jtsx-tsx-mode))

(use-package d2-mode
  :mode "\\.d2\\'"
  :config
  (modaled-define-substate "d2")
  (modaled-define-keys
    :substates '("d2")
    :bind
    `(("' c" . ("d2 compile" . d2-compile))))
  (modaled-enable-substate-on-state-change
    "d2"
    :states '("normal" "select")
    :major '(d2-mode)))

(use-package markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

(use-package nushell-ts-mode
  :mode "\\.nu\\'")

(use-package typst-ts-mode
  :mode "\\.typ\\'"
  :custom
  (typst-ts-mode-indent-offset 2)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (modaled-define-substate "typst")
  (modaled-define-keys
    :substates '("typst")
    :bind
    `(("' m" . ("typst menu" . typst-ts-tmenu))))
  (modaled-enable-substate-on-state-change
    "typst"
    :states '("normal" "select")
    :major '(typst-ts-mode)))

(use-package combobulate
  :defer t)

;; eglot LSP client
(use-package eglot
  :defer t
  :hook
  ((haskell-mode
    c-ts-mode
    c++-ts-mode
    rust-ts-mode
    go-ts-mode
    python-ts-mode
    html-mode
    js-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    jtsx-jsx-mode
    jtsx-tsx-mode
    bash-ts-mode
    nushell-ts-mode
    css-ts-mode
    json-ts-mode
    toml-ts-mode
    yaml-ts-mode
    dockerfile-ts-mode
    nix-mode
    lua-mode
    latex-mode
    typst-ts-mode) . eglot-ensure)
  :custom
  ; disable event buffer (hangs frequently in js/ts)
  (eglot-events-buffer-size 0)
  :config
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist")))
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server" "--configpath=@LUA_LS_CONFIG@"))))


;; tree-sitter (put at the end as some packages above may change auto-mode-alist)
;; remap major mode to ts major mode
(defvar ts-mode-remap-alist
  '((c-mode . c-ts-mode)
    (c++-mode . c++-ts-mode)
    (c-or-c++-mode . c-or-c++-ts-mode)
    ;; (javascript-mode . js-ts-mode)
    ;; (js-mode . js-ts-mode)
    (javascript-mode . jtsx-jsx-mode)
    (js-mode . jtsx-jsx-mode)
    (python-mode . python-ts-mode)
    (css-mode . css-ts-mode)
    (js-json-mode . json-ts-mode)
    (conf-toml-mode . toml-ts-mode)
    (sh-mode . bash-ts-mode)))

(defun remap-ts-mode (elem)
  "Remap mode in ELEM to ts mode."
  (let* ((orig (cdr elem))
         (new (cdr (assoc orig ts-mode-remap-alist))))
    (cons (car elem)
          (or new orig))))

;; replace modes in all mode alists
;; `major-mode-remap-alist' only affects `auto-mode-alist' so not using it.
(setq-default interpreter-mode-alist
              (mapcar #'remap-ts-mode interpreter-mode-alist))
(setq-default auto-mode-alist
              (mapcar #'remap-ts-mode auto-mode-alist))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("Containerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\`go.mod\\'" . go-mode-ts-mode))
(add-to-list 'auto-mode-alist '("\\.z?sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jtsx-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.[jt]?sx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
