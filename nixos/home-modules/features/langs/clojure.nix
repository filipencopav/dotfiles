{ pkgs, ... }:
{
  home.packages = [
    (pkgs.clojure.overrideAttrs (old: {
      passthru = old.passthru // {
        jdk = pkgs.openjdk24;
      };
    }))
    pkgs.babashka
    pkgs.leiningen
  ];
}
