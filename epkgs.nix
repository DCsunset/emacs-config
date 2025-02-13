{ extraEpkgs, epkgs }:

with epkgs; [
  ## common.el
  vlf
  dash
  nerd-icons
  visual-fill-column
  kkp
  extraEpkgs.modaled

  ## check.el
  sideline
  sideline-flymake
  flymake-cspell

  ## org.el
  org
  org-super-agenda
  org-present
  denote
  consult-denote
  valign
  extraEpkgs.org-moderncv

  # language.el
  extraEpkgs.hurl-mode
  json-mode  # required by hurl-mode
  extraEpkgs.combobulate
  extraEpkgs.typst-ts-mode
  csv-mode
  jtsx
  beancount
  nix-mode
  markdown-mode
  nushell-mode
  haskell-mode
  caddyfile-mode
  lua-mode
  d2-mode

  # tree-sitter for emacs 29+
  (treesit-grammars.with-grammars (grammars: with grammars; [
    tree-sitter-json
    tree-sitter-yaml
    tree-sitter-toml
    tree-sitter-html
    tree-sitter-css
    tree-sitter-markdown
    tree-sitter-make
    tree-sitter-dockerfile
    tree-sitter-python
    tree-sitter-bash
    tree-sitter-c
    tree-sitter-cpp
    tree-sitter-go
    tree-sitter-gomod
    tree-sitter-rust
    tree-sitter-javascript
    tree-sitter-typescript
    tree-sitter-tsx
    tree-sitter-nix
    tree-sitter-elisp
    tree-sitter-typst
  ]))
  # modeline.el
  shrink-path

  # tempo.el
  uuidgen

  # hx.el
  expand-region
  multiple-cursors
  default-text-scale
  popwin

  # dir.el
  dired-open-with
  dired-du
  dired-sidebar
  nerd-icons-dired
  projectile
  consult
  rg
  envrc

  # ai.el
  ellama

  # misc
  esup
  beframe
  undo-fu
  vundo
  minions
  rainbow-mode
  dashboard
  xclip
  modus-themes
  centaur-tabs
  nerd-icons-completion
  vertico
  marginalia
  orderless
  highlight
  vterm
  vterm-toggle
  diff-hl
  hl-todo
  blamer
  magit
  magit-todos
  company
  isearch-mb
]
