{ config, pkgs, util, lib, ... }:
let
  colors = config.colorScheme.palette;
  wrap-nixgl = util.wrap-nixgl-if config.my.features.nixgl.enable config;
in {
  wayland.windowManager.hyprland = {
    enable = false;
    package = wrap-nixgl pkgs.hyprland;

    settings = {
      "$mod" = "SUPER";

      monitor = [
        "WAYLAND-1, disable"
      ];

      general = {
        gaps_in = 5;
        gaps_out = 8;
        float_gaps = 8;
        layout = "master";

        snap = {
          enabled = true;
          window_gap = 8;
          monitor_gap = 26;
          respect_gaps = true;
        };
      };

      master = {
        new_on_top = true;
        orientation = "center";
        slave_count_for_center_master = 2;
        center_master_fallback = "right";
      };

      decoration = {
        dim_inactive = true;
        dim_strength = 0.1618;

        blur.enabled = false;
        shadow.enabled = false;

        rounding = 0;
        rounding_power = 4.0; # squircle
      };

      input = {
        kb_layout = "ro,ru";
        kb_options = "grp:alt_shift_toggle";
        repeat_delay = 270;
        repeat_rate = 43;

        sensitivity = 0.5;
        accel_profile = "flat";
        follow_mouse = 2; # focus will only change on click
        float_switch_override_focus = 0;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        focus_on_activate = true;
        middle_click_paste = false;
      };

      cursor = {
        no_warps = true;
      };

      ecosystem = {
        no_update_news = true;
        no_donation_nag = true;
      };

      workspace = [
        "w[tv1], gapsin:0, gapsout:0"
        "f[1], gapsout:0, gapsin:0"
      ];

      windowrule = [
        # disable borders and rounding on non-floating windows when they are the only
        # non-floaters around the workspace
        "bordersize 0, floating:0, onworkspace:w[tv1]"
        "bordersize 0, floating:0, onworkspace:f[1]"
        "rounding 0, floating:0, onworkspace:w[tv1]"
        "rounding 0, floating:0, onworkspace:f[1]"

        # workspace 4 - chromium. disable floating
        "tile, class:chromium"
        "workspace 4, class:chromium"
        "float, class:negative:(chromium), workspace:4"
        "fullscreen, content:game"

        "float, class:io\\.github\\.tdesktop_x64\\.TDesktop, title:([mM]edia ?[Vv]iewer)"
        "fullscreen, class:io\\.github\\.tdesktop_x64\\.TDesktop, title:([mM]edia ?[Vv]iewer)"

        "rounding 4, floating:1"
      ];

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      bind = let workspaces = (map (toString) [ 1 2 3 4 5 6 7 8 9 ]);
      in [
        ", Print, exec, grimblast copy area"
        "$mod, Return, exec, kitty"
        "$mod SHIFT, E, exec, emacsclient -c"
        "$mod SHIFT, C, killactive"
        "$mod SHIFT, F, fullscreen, 0"
        "$mod SHIFT, Space, fullscreen, 1"
        "$mod, Q, togglefloating"
        "$mod, Tab, cyclenext"
        "$mod, F, layoutmsg, cyclenext"
        "$mod, F, alterzorder, top"
        "$mod, D, layoutmsg, swapwithmaster"

        # snapping to different parts of screen
        "$mod SHIFT, L, moveactive, exact 50% 0"
        "$mod SHIFT, L, resizeactive, exact 50% 100%"
        "$mod SHIFT, J, moveactive, exact 0 0"
        "$mod SHIFT, J, resizeactive, exact 50% 100%"
        "$mod SHIFT, I, fullscreen, 1"

      ] ++ (map (number: "$mod, ${number}, workspace, ${number}") workspaces)
        ++ (map (number: "$mod_SHIFT, ${number}, movetoworkspacesilent, ${number}") workspaces)

      ;

      bindr = [
        "$mod, Super_L, exec, pkill wofi || wofi --show drun"
      ];

      binde = [
        # move floating windows with IJKL
        "$mod, J, moveactive, -20 0"
        "$mod, L, moveactive, 20 0"
        "$mod, I, moveactive, 0 -20"
        "$mod, K, moveactive, 0 20"

        # resize with SEUO
        "$mod, S, resizeactive,  0  -10%"
        "$mod, E, resizeactive,  0   10%"
        "$mod, O, resizeactive,  10%  0"
        "$mod, U, resizeactive, -10%  0"

        # compensate resizing
        "$mod, S, moveactive,  0  5%"
        "$mod, E, moveactive,  0 -5%"
        "$mod, O, moveactive, -5% 0"
        "$mod, U, moveactive,  5% 0"


        # "$mod, J I, moveactive, -15 -15"
        # "$mod, J K, moveactive, -15 15"
        # "$mod, L I, moveactive, 15 -15"
        # "$mod, L K, moveactive, 15 15"
      ];

      exec-once = [
        "eww daemon && eww open bar"
        "wpaperd"
      ];
      exec = [
        (with config.gtk; (lib.mkIf enable
          # TODO: make a package to auto convert XCursor themes to hyprcursor format.
          # Hypr team had absolutely no reason to invent a new format. https://xkcd.com/927/
          "hyprctl setcursor ${cursorTheme.name} ${toString cursorTheme.size}"))
      ];


      # ANIMATIONS
      bezier = [
        "easeOutExpo, 0.19, 1, 0.22, 1"
      ];

      animation = [
        "windows, 1, 4, easeOutExpo, popin"
        "windowsMove, 1, 6, easeOutExpo, popin"
        "windowsOut, 1, 4, easeOutExpo, gnomed"
        "fadeOut, 1, 3, easeOutExpo"

        "workspaces, 1, 3.5, easeOutExpo, slide"

        "fade, 0"
        "fadeDim, 1, 6, easeOutExpo"
      ];
    };
  };
}
