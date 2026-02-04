{ pkgs, ... }:
{
  virtualisation = {
    containers.enable = true;
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
      autoPrune.enable = true;
      extraPackages = [
        pkgs.podman-compose
      ];
    };
  };
}
