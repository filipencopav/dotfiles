{ lib, pkgs, config, inputs, ... }:
let pkg =
  (pkgs.emacs30.override {
    withNativeCompilation = true;
    withPgtk = true;
    withTreeSitter = true;
  });
langs = config.my.features.langs;
in {
  options = {
    my.features.emacs.defaultEditor = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to configure {command}`emacsclient` as the default
        editor using the {env}`EDITOR` environment variable.
      '';
    };
  };

  services.emacs = {
    enable = true;
    package = pkg;
    client.enable = true;
    defaultEditor = config.my.features.emacs.defaultEditor;
  };

  programs.emacs = {
    enable = true;
    package = pkg;
    extraPackages = (epkgs: with epkgs; [
      tree-sitter
      tree-sitter-langs
      treesit-grammars.with-all-grammars
    ]);
  };

  home.packages = [
    pkgs.nixd
  ] ++ lib.optionals langs.rust.enable [
    (inputs.fenix.complete.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    pkgs.rust-analyzer
  ] ++ lib.optionals langs.go.enable [
    pkgs.gopls
  ] ++ lib.optionals langs.erlang.enable [
    pkgs.erlang-language-platform
  ] ++ lib.optionals langs.js.enable [
    pkgs.astro-language-server
    pkgs.vue-language-server
  ] ++ lib.optionals langs.java.enable [
    (pkgs.jdt-language-server.override (final: final // {
      jdk = pkgs.openjdk24;
    }))
  ];
}
