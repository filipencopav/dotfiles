{ pkgs, config, ... }:
let
  xdg = config.xdg;
in
{
  stylix.targets.gtk.enable = true;
  stylix.targets.gtk.fonts.enable = true;
  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.kora-icon-theme;
      name = "kora";
    };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };

    gtk4.theme = null;
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };

  home.file."${xdg.dataHome}/icons/capitaine-cursors-white".source =
    "${pkgs.capitaine-cursors}/share/icons/capitaine-cursors-white";
}
