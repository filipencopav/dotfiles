{ inputs, config, pkgs, lib, ... }:
let
  colors = config.lib.stylix.colors;
  cfg = config.my.features;
in {
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
    settings.colors.selection = lib.mkForce
      "${config.lib.stylix.colors.base02-hex}ff";
    settings.main = {
      font = lib.mkForce (lib.concatStringsSep ":" [
        config.stylix.fonts.monospace.name
        "size=12"
      ]);
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

    settings.spawn-at-startup = [
    ];
    settings.environment = {
      # For XWayland
      DISPLAY = ":0";
    };

    settings.layout = {
      gaps = 8;
      focus-ring = {
        width = 3;
        active.color = "#${colors.base08}";
      };
    };
    settings.binds = lib.attrsets.optionalAttrs cfg.passfuzzel.enable {
      "Mod+P".action.spawn = [
        "${cfg.passfuzzel.package}/bin/passfuzzel"
      ];
      "Mod+P".hotkey-overlay.title = "Passwords menu";
    } // {
      "Mod+Shift+Slash".action.show-hotkey-overlay = {};

      # Suggested binds for running programs: terminal, app launcher, screen locker.
      "Mod+Return".action.spawn = "kitty";
      "Mod+Return".hotkey-overlay.title = "Open a Terminal: kitty";
      "Mod+Shift+E".action.spawn = ["emacsclient" "-c"];
      "Mod+D".action.spawn = ["fuzzel"];
      "Mod+D".hotkey-overlay.title = "Run an Application: fuzzel";
      # Super+Alt+L hotkey-overlay-title="Lock the Screen: swaylock" { spawn "swaylock"; }

      # You can also use a shell. Do this if you need pipes, multiple commands, etc.
      # Note: the entire command goes as a single argument in the end.
      # Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }

      # Example volume keys mappings for PipeWire & WirePlumber.
      # The allow-when-locked=true property makes them work even when the session is locked.
      # XF86AudioRaiseVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"; }
      # XF86AudioLowerVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"; }
      # XF86AudioMute        allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
      # XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }

      # Open/close the Overview: a zoomed-out view of workspaces and windows.
      # You can also move the mouse into the top-left hot corner,
      # or do a four-finger swipe up on a touchpad.
      "Mod+S".action.toggle-overview = {};
      "Mod+S".repeat = false;

      "Mod+Ctrl+C".action.close-window = {};

      "Mod+J".action.focus-column-left = {};
      "Mod+K".action.focus-window-or-workspace-down = {};
      "Mod+I".action.focus-window-or-workspace-up = {};
      "Mod+L".action.focus-column-right = {};

      "Mod+Ctrl+J".action.move-column-left = {};
      "Mod+Ctrl+K".action.move-window-down-or-to-workspace-down = {};
      "Mod+Ctrl+I".action.move-window-up-or-to-workspace-up = {};
      "Mod+Ctrl+L".action.move-column-right = {};

      # Alternative commands that move across workspaces when reaching
      # the first or last window in a column.
      # Mod+J     { focus-window-or-workspace-down; }
      # Mod+K     { focus-window-or-workspace-up; }
      # Mod+Ctrl+J     { move-window-down-or-to-workspace-down; }
      # Mod+Ctrl+K     { move-window-up-or-to-workspace-up; }

      # Mod+Home { focus-column-first; }
      # Mod+End  { focus-column-last; }
      # Mod+Ctrl+Home { move-column-to-first; }
      # Mod+Ctrl+End  { move-column-to-last; }

      # Mod+Shift+Left  { focus-monitor-left; }
      # Mod+Shift+Down  { focus-monitor-down; }
      # Mod+Shift+Up    { focus-monitor-up; }
      # Mod+Shift+Right { focus-monitor-right; }
      # Mod+Shift+H     { focus-monitor-left; }
      # Mod+Shift+J     { focus-monitor-down; }
      # Mod+Shift+K     { focus-monitor-up; }
      # Mod+Shift+L     { focus-monitor-right; }

      # Mod+Shift+Ctrl+Left  { move-column-to-monitor-left; }
      # Mod+Shift+Ctrl+Down  { move-column-to-monitor-down; }
      # Mod+Shift+Ctrl+Up    { move-column-to-monitor-up; }
      # Mod+Shift+Ctrl+Right { move-column-to-monitor-right; }
      # Mod+Shift+Ctrl+H     { move-column-to-monitor-left; }
      # Mod+Shift+Ctrl+J     { move-column-to-monitor-down; }
      # Mod+Shift+Ctrl+K     { move-column-to-monitor-up; }
      # Mod+Shift+Ctrl+L     { move-column-to-monitor-right; }

      # Alternatively, there are commands to move just a single window:
      # Mod+Shift+Ctrl+Left  { move-window-to-monitor-left; }
      # ...

      # And you can also move a whole workspace to another monitor:
      # Mod+Shift+Ctrl+Left  { move-workspace-to-monitor-left; }
      # ...

      # Mod+Page_Down      { focus-workspace-down; }
      # Mod+Page_Up        { focus-workspace-up; }
      # Mod+U              { focus-workspace-down; }
      # Mod+I              { focus-workspace-up; }
      # Mod+Ctrl+Page_Down { move-column-to-workspace-down; }
      # Mod+Ctrl+Page_Up   { move-column-to-workspace-up; }
      # Mod+Ctrl+U         { move-column-to-workspace-down; }
      # Mod+Ctrl+I         { move-column-to-workspace-up; }

      # Alternatively, there are commands to move just a single window:
      # Mod+Ctrl+Page_Down { move-window-to-workspace-down; }
      # ...

      # Mod+Shift+Page_Down { move-workspace-down; }
      # Mod+Shift+Page_Up   { move-workspace-up; }
      # Mod+Shift+U         { move-workspace-down; }
      # Mod+Shift+I         { move-workspace-up; }

      # You can bind mouse wheel scroll ticks using the following syntax.
      # These binds will change direction based on the natural-scroll setting.
      #
      # To avoid scrolling through workspaces really fast, you can use
      # the cooldown-ms property. The bind will be rate-limited to this value.
      # You can set a cooldown on any bind, but it's most useful for the wheel.
      # Mod+WheelScrollDown      cooldown-ms=150 { focus-workspace-down; }
      # Mod+WheelScrollUp        cooldown-ms=150 { focus-workspace-up; }
      # Mod+Ctrl+WheelScrollDown cooldown-ms=150 { move-column-to-workspace-down; }
      # Mod+Ctrl+WheelScrollUp   cooldown-ms=150 { move-column-to-workspace-up; }

      # Mod+WheelScrollRight      { focus-column-right; }
      # Mod+WheelScrollLeft       { focus-column-left; }
      # Mod+Ctrl+WheelScrollRight { move-column-right; }
      # Mod+Ctrl+WheelScrollLeft  { move-column-left; }

      # Usually scrolling up and down with Shift in applications results in
      # horizontal scrolling; these binds replicate that.
      # Mod+Shift+WheelScrollDown      { focus-column-right; }
      # Mod+Shift+WheelScrollUp        { focus-column-left; }
      # Mod+Ctrl+Shift+WheelScrollDown { move-column-right; }
      # Mod+Ctrl+Shift+WheelScrollUp   { move-column-left; }

      # Similarly, you can bind touchpad scroll "ticks".
      # Touchpad scrolling is continuous, so for these binds it is split into
      # discrete intervals.
      # These binds are also affected by touchpad's natural-scroll, so these
      # example binds are "inverted", since we have natural-scroll enabled for
      # touchpads by default.
      # Mod+TouchpadScrollDown { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02+"; }
      # Mod+TouchpadScrollUp   { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02-"; }

      # You can refer to workspaces by index. However, keep in mind that
      # niri is a dynamic workspace system, so these commands are kind of
      # "best effort". Trying to refer to a workspace index bigger than
      # the current workspace count will instead refer to the bottommost
      # (empty) workspace.
      #
      # For example, with 2 workspaces + 1 empty, indices 3, 4, 5 and so on
      # will all refer to the 3rd workspace.
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

      # Alternatively, there are commands to move just a single window:
      # Mod+Ctrl+1 { move-window-to-workspace 1; }

      # Switches focus between the current and the previous workspace.
      # Mod+Tab { focus-workspace-previous; }

      # The following binds move the focused window in and out of a column.
      # If the window is alone, they will consume it into the nearby column to the side.
      # If the window is already in a column, they will expel it out.
      # Mod+BracketLeft  { consume-or-expel-window-left; }
      # Mod+BracketRight { consume-or-expel-window-right; }

      # Consume one window from the right to the bottom of the focused column.
      # Mod+Comma  { consume-window-into-column; }
      # Expel the bottom window from the focused column to the right.
      # Mod+Period { expel-window-from-column; }

      "Mod+R".action.switch-preset-column-width = {};
      # Mod+Shift+R { switch-preset-window-height; }
      # Mod+Ctrl+R { reset-window-height; }
      "Mod+Ctrl+Space".action.maximize-column = {};
      "Mod+Ctrl+F".action.fullscreen-window = {};

      # Expand the focused column to space not taken up by other fully visible columns.
      # Makes the column "fill the rest of the space".
      "Mod+F".action.expand-column-to-available-width = {};

      "Mod+C".action.center-window = {};

      # Center all fully visible columns on screen.
      # Mod+Ctrl+C { center-visible-columns; }

      # Finer width adjustments.
      # This command can also:
      # * set width in pixels: "1000"
      # * adjust width in pixels: "-5" or "+5"
      # * set width as a percentage of screen width: "25%"
      # * adjust width as a percentage of screen width: "-10%" or "+10%"
      # Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
      # set-column-width "100" will make the column occupy 200 physical screen pixels.
      # Mod+Minus { set-column-width "-10%"; }
      # Mod+Equal { set-column-width "+10%"; }

      # Finer height adjustments when in column with other windows.
      # Mod+Shift+Minus { set-window-height "-10%"; }
      # Mod+Shift+Equal { set-window-height "+10%"; }

      # Move the focused window between the floating and the tiling layout.
      "Mod+Q".action.toggle-window-floating = {};
      "Mod+Tab".action.switch-focus-between-floating-and-tiling = {};

      # Toggle tabbed column display mode.
      # Windows in this column will appear as vertical tabs,
      # rather than stacked on top of each other.
      # Mod+W { toggle-column-tabbed-display; }

      # Actions to switch layouts.
      # Note: if you uncomment these, make sure you do NOT have
      # a matching layout switch hotkey configured in xkb options above.
      # Having both at once on the same hotkey will break the switching,
      # since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
      # Mod+Space       { switch-layout "next"; }
      # Mod+Shift+Space { switch-layout "prev"; }

      "Print".action.screenshot = {};
      "Ctrl+Print".action.screenshot-screen = {};
      "Alt+Print".action.screenshot-window = {};

      # The quit action will show a confirmation dialog to avoid accidental exits.
      "Mod+Ctrl+Q".action.quit = {};
    };
  };
}
