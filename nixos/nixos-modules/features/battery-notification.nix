{pkgs, lib, config, ...}:
let
  cfg = config.my-nixos.features.battery-notification;
in {
  options.my-nixos.features.battery-notification = {
    threshold = lib.options.mkOption {
      type = lib.types.int;
      default = 20;
      description = "Threshold below which low battery notifications will be sent";
    };
  };
  systemd.user.services."battery-notifications" = {
    enable = true;
    description = "Notify user if battery is below ${toString cfg.threshold}%";
    partOf = ["graphical-session.target"];
    wantedBy = ["graphical-session.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = pkgs.writeShellScript "battery-low-notification"
        ''
          if (( ${toString cfg.threshold} >= $(${pkgs.lib.getExe pkgs.acpi} -b | head -n 1 | ${pkgs.lib.getExe pkgs.ripgrep} -o "\d+%" | ${pkgs.lib.getExe pkgs.ripgrep} -o "\d+")));
          then ${pkgs.lib.getExe pkgs.pkgs.libnotify} --urgency=critical "low battery" "$(${pkgs.lib.getExe pkgs.acpi} -b | head -n 1 | ${pkgs.lib.getExe pkgs.ripgrep} -o "\d+%")";
          else echo; fi;
        '';
    };
  };
  systemd.user.timers."battery-notifications" = {
    wantedBy = ["timers.target"];
    timerConfig = {
      # Every Minute
      OnCalendar = "*-*-* *:*:00";
      Unit = "battery-notifications.service";
    };
  };
}
