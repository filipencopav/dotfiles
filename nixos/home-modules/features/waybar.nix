{ pkgs, lib, ... }:
{
  stylix.targets.waybar.enable = true;
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    package = pkgs.waybar;
    settings.mainBar = {
      layer = "bottom";
      position = "bottom";
      modules-center = [
        "clock"
      ];
      modules-right = [
        "tray"
      ];
      clock = {
        format = "{:%A, %d-%m-%Y, %H:%M}";
      };
    };
    style = lib.mkAfter ''
      #clock {
        font-size: 1.1em;
        padding: 5px;
      }
      #tray {
        padding: 5px;
      }
    '';
  };
}
