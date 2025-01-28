{
  dc-lib,
  emacsPackagesFor,
  emacs30,
  writeText,

  # Extra emacs packages
  extraEpkgs,

  # Vars for substitution in config
  # Supported vars:
  #   LUA_LS_CONIFG GTD_DIR NOTES_DIR OLLAMA_HOST
  #   IRC_SERVER IRC_NICK IRC_USER IRC_PASS_FILE
  configVars ? {}
}:

let
  # reference: home-manager
  emacsWithPackages = (emacsPackagesFor emacs30).emacsWithPackages;
  extraPackages = epkgs: let
    userPackages = (import ./epkgs.nix) { inherit extraEpkgs epkgs; };
    userConfig = epkgs.trivialBuild {
      pname = "emacs-user-config";
      version = "unstable";
      src = writeText "default.el"
        (dc-lib.substituteVars
          ({
            LUA_LS_CONIFG = "${./config/lsp-config}/lua-ls-config.lua";
          } // configVars)
          (dc-lib.readFiles [
            ./config/common.el
            ./config/hx.el
            ./config/modeline.el
            ./config/tempo.el
            ./config/check.el
            ./config/org.el
            ./config/language.el
            ./config/dir.el
            ./config/ai.el
            ./config/default.el
          ]));
      packageRequires = userPackages;
    };
  in userPackages ++ [ userConfig ];
in emacsWithPackages extraPackages
