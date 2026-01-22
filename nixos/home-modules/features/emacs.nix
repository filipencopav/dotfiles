{ lib, pkgs, config, inputs, ... }:
let
  emacsPkg = (pkgs.emacs30.override {
    withNativeCompilation = true;
    withPgtk = true;
    withTreeSitter = true;
  });
  emacs = (pkgs.emacsPackagesFor emacsPkg).emacsWithPackages (
    epkgs: [
      (epkgs.treesit-grammars.with-grammars (p: with p; [
        tree-sitter-go
        tree-sitter-nix
        tree-sitter-java
        tree-sitter-typescript
        tree-sitter-vue
        tree-sitter-erlang
        tree-sitter-clojure
        tree-sitter-dockerfile
        tree-sitter-yaml
        tree-sitter-proto
        tree-sitter-typst
      ]))
    ]
  );
  jdk = config.my.features.langs.java.jdk;
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
    package = emacs;
    client.enable = true;
    defaultEditor = config.my.features.emacs.defaultEditor;
  };

  programs.emacs = {
    enable = true;
    package = emacs;
  };

  home.packages = [
    pkgs.nixd
    pkgs.clang
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
    (pkgs.jdt-language-server.override (final: final // { inherit jdk; }))
  ];
}
