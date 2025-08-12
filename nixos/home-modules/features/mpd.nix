{ ... }:
{
  services.mpd = {
    enable = true;
    # TODO: migrate mpd config to nix
  };

  home.sessionVariables = {
    MPC_FORMAT = "[%artist%[ \\\"%album%\\\"][ ##%track%] - ]%title%";
  };
}
