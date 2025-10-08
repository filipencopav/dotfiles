{ config, pkgs, ... }:
{
  home.packages = config.stylix.fonts.packages;
  stylix = with pkgs; {
    autoEnable = false;

    base16Scheme = "${base16-schemes}/share/themes/tomorrow-night.yaml";
    enable = true;
    fonts = {
      serif = {
        package = ibm-plex;
        name = "IBM Plex Serif";
      };

      sansSerif = {
        package = ibm-plex;
        name = "IBM Plex Sans";
      };

      monospace = {
        package = nerd-fonts.fantasque-sans-mono;
        name = "FantasqueSansM Nerd Font";
      };

      emoji = {
        package = twemoji-color-font;
        name = "Twitter Color Emoji";
      };

      sizes = {
        applications = 13;
        terminal = 13;
        popups = 13;
      };
    };

    targets.vesktop.enable = true;
    targets.vencord.enable = true;
    targets.gtk.enable = true;
    targets.zathura.enable = true;
  };
}
