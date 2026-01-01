{ pkgs, lib, config, ... }:
{
  options = {
    my.features.waybar.show-battery = lib.mkOption {
      type = lib.types.bool;
      description = "Whether to display battery";
      default = false;
    };
  };
  
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
      ] ++ lib.optionals config.my.features.waybar.show-battery [
        "battery"
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
