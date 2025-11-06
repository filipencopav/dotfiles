{ pkgs, lib, config, ... }:
let jdk = config.my.features.langs.java.jdk;
in {
  options = {
    my.features.langs.java.jdk = lib.mkOption {
      default = pkgs.openjdk25;
      type = lib.types.package;
    };
  };
  config = {
    home.packages = [
      jdk
      pkgs.maven
      (pkgs.gradle.override {
        java = jdk;
      })
    ];

    home.sessionVariables = {
      _JAVA_OPTIONS = lib.concatStringsSep " " [
        "-Dawt.useSystemAAFontSettings=lcd" "-Djava.util.prefs.userRoot=${config.xdg.configHome}/java"
      ];
    };
  };
}
