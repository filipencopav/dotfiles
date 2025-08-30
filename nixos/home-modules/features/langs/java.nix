{ pkgs, lib, config, ... }:
let jdk = pkgs.openjdk24;
in {
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
}
