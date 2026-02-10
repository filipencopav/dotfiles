{ config, inputs, pkgs, ... }:
let
  secrets = inputs.secrets-folder;
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  environment.systemPackages = [
    pkgs.xray
  ];
  services.xray = {
    enable = true;
    settingsFile = config.sops.templates."xray.json".path;
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 443 ];
    allowedUDPPorts = [ 23 ];
    extraCommands = ''
      iptables -A INPUT -i eth0 -p tcp --dport 23 -j ACCEPT
        iptables -A INPUT -i eth0 -p udp --dport 23 -j ACCEPT
        iptables -A INPUT -i eth0 -p tcp --dport 443 -j ACCEPT
        iptables -A INPUT -i eth0 -p tcp --dport 9100 -j ACCEPT
    '';
  };

  sops.templates."xray.json" = {
    owner = "xray";
    content = builtins.toJSON {
      log = {
        loglevel = "info";
      };

      routing = {
        rules = [];
        domainStrategy = "AsIs";
      };

      inbounds = [
        {
          port = 443;
          protocol = "vless";
          tag = "vless_tls";
          settings = {
            clients = [
              {
                id = config.sops.placeholder."xray_user1_uuid";
                email = "user1@xray";
                flow = "xtls-rprx-vision";
              }
              {
                id = config.sops.placeholder."xray_user2_uuid";
                email = "user2@xray";
                flow = "xtls-rprx-vision";
              }
            ];
            decryption = "none";
          };
          streamSettings = {
            network = "tcp";
            security = "reality";
            realitySettings = {
              show = true;
              dest = config.sops.placeholder."xray_vless_domain" + ":443";
              xver = 0;
              serverNames = [ config.sops.placeholder."xray_vless_domain" ];
              privateKey = config.sops.placeholder."xray_private_key";
              minClientVer = "";
              maxClientVer = "";
              maxTimeDiff = 0;
              shortIds = [ "aa00" ];
            };
          };
          sniffing = {
            enabled = true;
            destOverride = ["http" "tls"];
          };
        }
      ];

      outbounds = [
        {
          protocol = "freedom";
          tag = "direct";
        }
        {
          protocol = "blackhole";
          tag = "block";
        }
      ];
    };
  };

  
  sops.age.keyFile = "/etc/sops/age.txt";

  sops.secrets."xray_user1_uuid" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_user2_uuid" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_server_address" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_vless_domain" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_public_key" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_private_key" = {
    owner = "xray";
    sopsFile = "${secrets}/xray.yaml";
  };
}
