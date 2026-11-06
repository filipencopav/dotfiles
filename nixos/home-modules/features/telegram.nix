{ pkgs, ... }:
{
  home.packages = [
    pkgs._64gram
  ];
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/tg" = "io.github.tdesktop_x64.TDesktop.desktop";
      "x-scheme-handler/tonsite" = "io.github.tdesktop_x64.TDesktop.desktop";
    };
  };
}
