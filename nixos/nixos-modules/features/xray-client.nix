{ pkgs, config, inputs, ... }:
let
  secrets = inputs.secrets-folder;
in {
  environment.systemPackages = [ pkgs.xray ];

  my-nixos.features.sops.enable = true;

  services.xray = {
    enable = true;
    settingsFile = config.sops.templates."xray-client.json".path;
  };

  # Grant CAP_NET_ADMIN to xray for creating TUN interface
  systemd.services.xray.serviceConfig = {
    CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
    AmbientCapabilities = [ "CAP_NET_ADMIN" ];
  };

  # Configure routing for Transparent Proxy
  systemd.services.xray.postStart = let
    p = pkgs.iproute2;
  in ''
    # Wait for tun0 to be created by Xray
    # (Xray creates it immediately on start if configured)
    ${p}/bin/ip addr add 198.18.0.1/15 dev tun0 || true
    ${p}/bin/ip link set dev tun0 up || true

    # Routing Rules
    # 1. Route Xray's own traffic (marked 255) to main table to bypass proxy
    ${p}/bin/ip rule add fwmark 255 lookup main priority 9000 || true
    
    # 2. Route SSH traffic (port 22) to main table to prevent lockout
    ${p}/bin/ip rule add sport 22 lookup main priority 9001 || true
    ${p}/bin/ip rule add dport 22 lookup main priority 9002 || true

    # 3. Route all other traffic to table 100
    ${p}/bin/ip rule add from all lookup 100 priority 9003 || true

    # 4. Configure table 100 to route via tun0
    ${p}/bin/ip route add default dev tun0 table 100 || true
  '';

  systemd.services.xray.preStop = let
    p = pkgs.iproute2;
  in ''
    ${p}/bin/ip rule del fwmark 255 lookup main 2>/dev/null || true
    ${p}/bin/ip rule del sport 22 lookup main 2>/dev/null || true
    ${p}/bin/ip rule del dport 22 lookup main 2>/dev/null || true
    ${p}/bin/ip rule del from all lookup 100 2>/dev/null || true
    ${p}/bin/ip route flush table 100 2>/dev/null || true
  '';

  sops.templates."xray-client.json" = {
    content = builtins.toJSON {
      log = {
        loglevel = "warning";
      };

      routing = {
        domainStrategy = "IPOnDemand";
        rules = [
          {
            type = "field";
            outboundTag = "direct";
            domain = ["geosite:ru"];
          }
          {
            type = "field";
            outboundTag = "direct";
            ip = ["geoip:ru" "geoip:private"];
          }
          {
            type = "field";
            outboundTag = "block";
            domain = ["geosite:category-ads-all"];
          }
        ];
      };
      
      inbounds = [
        {
          tag = "socks-in";
          protocol = "socks";
          listen = "127.0.0.1";
          port = 10808;
          settings = {
            upd = true;
          };
        }
        # 4.2 A few APPs are incompatible with socks protocol and need http protocol for forwarding, use the port below
        {
          "tag" = "http-in";
          "protocol" = "http";
          "listen" = "127.0.0.1"; # This is the address for local forwarding via http
          "port" = 10801; # This is the port for local forwarding via http
        }
      ];
      outbounds = [
        {
          protocol = "vless";
          tag = "proxy";
          settings = {
            vnext = [
              {
                address = config.sops.placeholder."xray_server_address";
                port = 443;
                users = [
                  {
                    id = config.sops.placeholder."xray_user1_uuid";
                    flow = "xtls-rprx-vision";
                    encryption = "none";
                    level = 0;
                  }
                ];
              }
            ];
          };
          streamSettings = {
            network = "tcp";
            security = "reality";
            realitySettings = {
              serverName = config.sops.placeholder."xray_vless_domain";
              publicKey = config.sops.placeholder."xray_public_key";
              fingerprint = "chrome";
              shortId = "aa00";
              spiderX = "/";
            };
            sockopt = {
              mark = 255; # Mark outbound traffic to bypass proxy
            };
          };
        }
        {
          protocol = "freedom";
          tag = "direct";
          streamSettings = {
            sockopt = {
              mark = 255; # Mark direct traffic too, just in case
            };
          };
        }
        {
          protocol = "blackhole";
          tag = "block";
        }
      ];
    };
  };

  sops.secrets."xray_user1_uuid" = {
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_server_address" = {
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_vless_domain" = {
    sopsFile = "${secrets}/xray.yaml";
  };
  sops.secrets."xray_public_key" = {
    sopsFile = "${secrets}/xray.yaml";
  };
}
