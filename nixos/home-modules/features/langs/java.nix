{ pkgs, lib, config, ... }:
{
  home.packages = [
    pkgs.openjdk24
  ];

  home.sessionVariables = {
    _JAVA_OPTIONS = lib.concatStringsSep " " [
      "-Dawt.useSystemAAFontSettings=lcd" "-Djava.util.prefs.userRoot=${config.xdg.configHome}/java"
    ];
  };
}
