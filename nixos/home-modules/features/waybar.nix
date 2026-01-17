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
        "wireplumber"
        "clock"
      ] ++ lib.optionals config.my.features.waybar.show-battery [
        "battery"
      ];
      modules-right = [
        "tray"
      ];
      clock = {
        format = "[TIME :: {:%A, %d-%m-%Y, %H:%M}]";
      };
      battery = {
        format = "[BAT :: {capacity}%]";
      };
      wireplumber = {
        "format" = "[VOL :: {volume}%]";
        "max-volume" = 150;
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
