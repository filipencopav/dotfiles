{ pkgs, config, ... }:
{
  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.configHome}/gnupg";
  };

  services.gpg-agent = {
    enable = true;
    pinentry = {
      package = pkgs.pinentry-gnome3;
      program = "pinentry-gnome3";
    };
  };
}
