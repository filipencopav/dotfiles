{ pkgs, config, ... }:
let jdk = config.my.features.langs.java.jdk;
in {
  home.packages = [
    (pkgs.clojure.overrideAttrs (old: {
      passthru = old.passthru // {
        jdk = pkgs.openjdk25;
      };
    }))
    pkgs.babashka
    pkgs.leiningen
  ];
}
