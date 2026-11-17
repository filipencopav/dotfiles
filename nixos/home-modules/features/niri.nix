{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
let
  colors = config.lib.stylix.colors;
  cfg = config.my.features;
in
{
  imports = [
    inputs.niri.homeModules.niri
  ];

  options = {
    my.features.niri.show-battery = lib.mkOption {
      type = lib.types.bool;
      description = "Whether to display battery charge in status bar";
      default = false;
    };
  };

  home.packages = [
    pkgs.gnome-keyring
    pkgs.xwayland-satellite
  ];

  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gnome
      xdg-desktop-portal-gtk
    ];
    config = {
      niri = {
        default = [ "gtk" ];
        "org.freedesktop.impl.portal.Secret" = [
          "gnome-keyring"
        ];
        "org.freedesktop.impl.portal.ScreenCast" = [
          "gnome"
        ];
        "org.freedesktop.impl.portal.FileChooser" = [
          "gtk"
        ];
      };
    };
    configPackages = [
      config.programs.niri.package
    ];
  };

  services.polkit-gnome.enable = true;

  services.wpaperd = {
    enable = true;
    settings = {
      any = {
        path = "${config.xdg.dataHome}/backgrounds/shuffle";
        sorting = "random";
        duration = "10m";
        queue-size = 33;
        mode = "center";
      };
    };
  };

  stylix.targets.fuzzel.enable = true;
  programs.fuzzel = {
    enable = true;
    settings.colors.selection = lib.mkForce "${config.lib.stylix.colors.base02-hex}ff";
    settings.main = {
      font = lib.mkForce (
        lib.concatStringsSep ":" [
          config.stylix.fonts.monospace.name
          "size=12"
        ]
      );
      width = 50;
      layer = "overlay";
      vertical-pad = 20;
      horizontal-pad = 30;
      inner-pad = 10;
      x-margin = 0;
      y-margin = 0;
    };
    settings.border = {
      radius = 5;
    };
  };

  my.features.waybar.enable = true;
  my.features.waybar.show-battery = cfg.niri.show-battery;

  # TODO: window rules for borderless when single on workspace. how would that work? anyway
  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings.prefer-no-csd = true;
    settings.layout.empty-workspace-above-first = true;
    settings.input.keyboard = {
      xkb = {
        layout = "ro,ru";
        options = "grp:alt_space_toggle";
      };
      repeat-delay = 270;
      repeat-rate = 43;
    };

    # settings.xwayland-satellite = {
    #   enable = true;
    #   path = lib.getExe pkgs.xwayland-satellite;
    # };

    settings.spawn-at-startup = [ ];
    settings.environment = {
      QT_QPA_PLATFORM = "wayland";
      # For XWayland
      DISPLAY = ":0";
    };

    settings.layout = {
      default-column-width.proportion = 0.33333;
      gaps = 8;
      focus-ring = {
        width = 3;
        active.color = "#${colors.base08}";
      };
    };
    settings.window-rules = [
      {
        matches = [
          { app-id = "firefox"; }
          { app-id = "helium"; }
          { app-id = "org.inkscape.Inkscape"; }
          { app-id = "ONLYOFFICE"; }
          { app-id = "io.github.tdesktop_x64.TDesktop"; }
          { app-id = "vesktop"; }
        ];
        open-maximized = true;
      }
    ];
    settings.binds =
      lib.attrsets.optionalAttrs cfg.passfuzzel.enable {
        "Mod+P".action.spawn = [
          "${cfg.passfuzzel.package}/bin/passfuzzel"
        ];
        "Mod+P".hotkey-overlay.title = "Passwords menu";
      }
      // {
        "Mod+Shift+Slash".action.show-hotkey-overlay = { };

        # Suggested binds for running programs: terminal, app launcher, screen locker.
        "Mod+Return".action.spawn = "kitty";
        "Mod+Return".hotkey-overlay.title = "Open a Terminal: kitty";
        "Mod+Shift+E".action.spawn = [
          "emacsclient"
          "-c"
        ];
        "Mod+D".action.spawn = [ "fuzzel" ];
        "Mod+D".hotkey-overlay.title = "Run an Application: fuzzel";
        "Mod+S".action.toggle-overview = { };
        "Mod+S".repeat = false;

        "Mod+Ctrl+C".action.close-window = { };

        "Mod+J".action.focus-column-left = { };
        "Mod+K".action.focus-window-or-workspace-down = { };
        "Mod+I".action.focus-window-or-workspace-up = { };
        "Mod+L".action.focus-column-right = { };

        "Mod+Ctrl+J".action.move-column-left = { };
        "Mod+Ctrl+K".action.move-window-down-or-to-workspace-down = { };
        "Mod+Ctrl+I".action.move-window-up-or-to-workspace-up = { };
        "Mod+Ctrl+L".action.move-column-right = { };

        "Mod+bracketright".action.spawn = [
          "wpctl"
          "set-volume"
          "@DEFAULT_AUDIO_SINK@"
          "0.1+"
        ];
        "Mod+bracketleft".action.spawn = [
          "wpctl"
          "set-volume"
          "@DEFAULT_AUDIO_SINK@"
          "0.1-"
        ];

        "Mod+1".action.focus-workspace = 1;
        "Mod+2".action.focus-workspace = 2;
        "Mod+3".action.focus-workspace = 3;
        "Mod+4".action.focus-workspace = 4;
        "Mod+5".action.focus-workspace = 5;
        "Mod+6".action.focus-workspace = 6;
        "Mod+7".action.focus-workspace = 7;
        "Mod+8".action.focus-workspace = 8;
        "Mod+9".action.focus-workspace = 9;
        "Mod+Ctrl+1".action.move-column-to-workspace = 1;
        "Mod+Ctrl+2".action.move-column-to-workspace = 2;
        "Mod+Ctrl+3".action.move-column-to-workspace = 3;
        "Mod+Ctrl+4".action.move-column-to-workspace = 4;
        "Mod+Ctrl+5".action.move-column-to-workspace = 5;
        "Mod+Ctrl+6".action.move-column-to-workspace = 6;
        "Mod+Ctrl+7".action.move-column-to-workspace = 7;
        "Mod+Ctrl+8".action.move-column-to-workspace = 8;
        "Mod+Ctrl+9".action.move-column-to-workspace = 9;

        "Mod+R".action.switch-preset-column-width = { };
        # Mod+Shift+R { switch-preset-window-height; }
        # Mod+Ctrl+R { reset-window-height; }
        "Mod+Ctrl+Space".action.maximize-column = { };
        "Mod+Ctrl+F".action.fullscreen-window = { };

        # Toggles maximized column state
        "Mod+F".action.maximize-column = { };

        "Mod+C".action.center-window = { };

        "Mod+Q".action.toggle-window-floating = { };
        "Mod+Tab".action.switch-focus-between-floating-and-tiling = { };

        "Print".action.screenshot = { };
        "Ctrl+Print".action.screenshot-screen = { };
        "Alt+Print".action.screenshot-window = { };

        # The quit action will show a confirmation dialog to avoid accidental exits.
        "Mod+Ctrl+Q".action.quit = { };
      };
  };
}
