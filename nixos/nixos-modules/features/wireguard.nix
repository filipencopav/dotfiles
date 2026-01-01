{ config, ... }:
{
  my-nixos.features.sops.enable = true;

  systemd.services.NetworkManager-ensure-profiles.after = [
    "NetworkManager.service"
    "sops-install-secrets.service"
  ];

  networking.networkmanager.ensureProfiles = {
    environmentFiles = [ config.sops.secrets.wg-env.path ]; 
    profiles.wg0 = {
      connection = {
        id = "wg0";
        type = "wireguard";
        interface-name = "wg0";
      };

      ipv4 = {
        address1 = "10.8.0.3/24";
        dns = "1.1.1.1;";
        dns-search = "~;";
        method = "manual";
      };

      ipv6 = {
        addr-gen-mode = "default";
        method = "disabled";
      };

      wireguard = {
        private-key = "$WG0_PRIVATE";
      };

      "wireguard-peer.gWYOPICmgrmkE2Z4/bCaN5TuYlEsR08JDSY4qDUZrGo=" = {
        endpoint = "87.249.50.238:51820";
        preshared-key = "$WG0_PRESHARED";
        preshared-key-flags = "0";
        allowed-ips = "::/0;0.0.0.0/0;";
        persistent-keepalive = 1;
      };
    };

    profiles.wg1 = {
      connection = {
        id = "wg1";
        type = "wireguard";
        interface-name = "wg1";
      };
      
      ipv4 = {
        address1 = "10.0.0.12/32";
        dns = "8.8.8.8;";
        dns-search = "~;";
        method = "manual";
      };

      ipv6 = {
        addr-gen-mode = "default";
        method = "disabled";
      };

      proxy = {};

      wireguard = {
        mtu = 1420;
        private-key = "$WG1_PRIVATE";
      };

      "wireguard-peer.qbhf7UK28hR6fa7qrDk48yyMGz9Bi2J7t8kLKKK+fA8=" = {
        allowed-ips = "10.0.0.0/24;192.168.27.0/24;";
        endpoint = "176.121.188.19:13231";
        persistent-keepalive = 25;
      };
    };
  };
}
