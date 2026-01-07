{ pkgs, config, ... }:
let xdg = config.xdg;
in {
  stylix.targets.gtk.enable = true;
  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.kora-icon-theme.overrideAttrs (final: final // {
        dontWrapQtApps = true;
      });
      name = "kora";
    };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };

  home.file."${xdg.dataHome}/icons/capitaine-cursors-white".source =
    "${pkgs.capitaine-cursors}/share/icons/capitaine-cursors-white";
}
