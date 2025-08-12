{ config, pkgs, util, lib, ... }:
let
  wrap-nixgl = util.wrap-nixgl-if config.my.features.nixgl.enable config;
in {
  stylix.targets.kitty.enable = true;
  programs.kitty = {
    enable = true;
    package = wrap-nixgl pkgs.kitty;
    settings = with config.colorScheme.palette; {
      wayland_titlebar_color = "background";

      confirm_os_window_close = 0;

      remember_window_size = "no";
      initial_window_width = "80c";
      initial_window_height = "36c";

      scrollback_lines = 10000;

      cursor_blink_interval = 0;

      clear_all_shortcuts = "yes";
    };

    keybindings = {
      "ctrl+alt+c" = "copy_to_clipboard";
      "ctrl+alt+v" = "paste_from_clipboard";
      "alt+i" = "scroll_line_up";
      "alt+k" = "scroll_line_down";
      "alt+page_up" = "scroll_page_up";
      "alt+page_down" = "scroll_page_down";
    };
  };
}
