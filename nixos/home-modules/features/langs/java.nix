{ pkgs, lib, config, ... }:
let jdk = config.my.features.langs.java.jdk;
in {
  options = {
    my.features.langs.java.jdk = lib.mkOption {
      default = pkgs.openjdk21;
      type = lib.types.package;
    };
  };

  home.packages = [
    jdk
    pkgs.maven
  ];

  home.sessionVariables = {
    _JAVA_OPTIONS = lib.concatStringsSep " " [
      "-Dawt.useSystemAAFontSettings=lcd" "-Djava.util.prefs.userRoot=${config.xdg.configHome}/java"
    ];
  };
}
