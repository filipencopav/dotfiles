{ pkgs, config, ... }:
let jdk = config.my.features.langs.java.jdk;
in {
  home.packages = [
    (pkgs.clojure.override {inherit jdk;})
    pkgs.babashka
    pkgs.leiningen
  ];
}
