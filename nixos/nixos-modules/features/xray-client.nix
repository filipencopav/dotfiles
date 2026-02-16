{ pkgs, config, inputs, ... }:
let
  secrets = inputs.secrets-folder;
  xray-fw-mark = 255;
  xray-interface-name = "tun0";
in {
  environment.systemPackages = [ pkgs.xray ];

  my-nixos.features.sops.enable = true;

  sops.templates."post_start.env".content = ''
    XRAY_VPS_ADDRESS=${config.sops.placeholder."xray_server_address"}
  '';
  systemd.services.xray = {
    serviceConfig.EnvironmentFile = config.sops.templates."post_start.env".path;
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    postStart = let
      ip = "${pkgs.iproute2}/bin/ip";
      awk = "${pkgs.gawk}/bin/awk";
      head = "${pkgs.coreutils}/bin/head";
    in ''
      while ! ${ip} link show ${xray-interface-name} >/dev/null 2>&1; do sleep 0.1; done
  
      # (e.g., "default via 192.168.1.1 dev eth0...")
      default_route_line=$(${ip} route show default | head -n1)
      gateway=$(echo "$default_route_line" | ${awk} '{print $3}')
      interface=$(echo "$default_route_line" | ${awk} '{print $5}')
      local_cidr=$(${ip} route show dev "$interface" scope link | ${head} -n1 | ${awk} '{print $1}')
  
      ${ip} route add default dev ${xray-interface-name} || true
      ${ip} route del default via "$gateway" || true
      ${ip} route add $XRAY_VPS_ADDRESS via "$gateway" dev "$interface" || true
      ${ip} route add "$local_cidr" via "$gateway" dev "$interface" || true
    '';
  };


  networking.firewall = {
    checkReversePath = false;
    trustedInterfaces = [ xray-interface-name ];
  };

  services.xray = {
    enable = true;
    settingsFile = config.sops.templates."xray-client.json".path;
  };
  sops.templates."xray-client.json".content = builtins.toJSON {
    log = {
      loglevel = "warning";
    };

    routing = {
      domainStrategy = "IPOnDemand";
      rules = [
        {
          type = "field";
          outboundTag = "direct";
          domain = [
            "geosite:ru"
            "geosite:steam"
          ];
        }
        {
          type = "field";
          outboundTag = "direct";
          ip = [
            "geoip:ru"
            "geoip:private"
          ];
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
        tag = "tun-in";
        protocol = "tun";
        settings = {
          name = "${xray-interface-name}";
          mtu = 1500;
        };
        sniffing = {
          enabled = true;
          destOverride = ["http" "tls"];
          routeOnly = true;
        };
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
            show = true;
            serverName = config.sops.placeholder."xray_vless_domain";
            publicKey = config.sops.placeholder."xray_public_key";
            fingerprint = "chrome";
            shortId = "aa00";
          };
          sockopt.mark = xray-fw-mark;
        };
      }
      {
        protocol = "freedom";
        tag = "direct";
        streamSettings.sockopt.mark = xray-fw-mark;
      }
      {
        protocol = "blackhole";
        tag = "block";
      }
    ];
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
