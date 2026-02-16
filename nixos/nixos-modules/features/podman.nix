{ pkgs, ... }:
{
  virtualisation = {
    containers.enable = true;
    containers.containersConf.settings = {
      engine."compose_warning_logs" = false;
      network.default_rootless_network_cmd = "slirp4netns";
    };
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
