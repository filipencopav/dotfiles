{
  config,
  inputs,
  ...
}:
let
  secrets-dir = inputs.secrets-folder;
  xray-interface-name = "tun0";
  sing-interface-name = "sing-tun";
in
{
  my-nixos.features.sops.enable = true;

  networking.firewall = {
    checkReversePath = false;
    trustedInterfaces = [
      xray-interface-name
      sing-interface-name
    ];
  };

  services.sing-box = {
    enable = true;
    settings = {
      log.level = "warning";
      dns.servers = [
        {
          tag = "google";
          type = "tls";
          server = "8.8.8.8";
        }
      ];
      inbounds = [
        {
          type = "tun";
          tag = "tun-in";
          interface_name = sing-interface-name;
          auto_route = true;
          auto_redirect = true;
          address = [
            "172.19.0.1/30"
            "fdfe:dcba:9876::1/126"
          ];
        }
      ];
      outbounds = [
        {
          type = "vless";
          tag = "proxy";
          server._secret = config.sops.secrets."xray_server_address".path;
          server_port = 443;
          uuid._secret = config.sops.secrets."xray_user1_uuid".path;
          flow = "xtls-rprx-vision";
          tls = {
            enabled = true;
            server_name._secret = config.sops.secrets."xray_vless_domain".path;
            utls = {
              enabled = true;
              fingerprint = "chrome";
            };
            reality = {
              enabled = true;
              public_key._secret = config.sops.secrets."xray_public_key".path;
              short_id = "aa00";
            };
          };
          packet_encoding = "xudp";
        }
        {
          type = "direct";
          tag = "direct";
        }
      ];
      route.auto_detect_interface = true;
      route.rules = [
        {
          type = "logical";
          mode = "or";
          action = "hijack-dns";
          rules = [
            { protocol = "dns"; }
            { port = 53; }
          ];
        }
        {
          action = "sniff";
        }
        {
          ip_is_private = true;
          action = "bypass";
        }
        {
          rule_set = [ "geoip-ru" ];
          outbound = "reject";
        }
      ];
      route.rule_set = [
        {
          type = "remote";
          tag = "geoip-ru";
          format = "binary";
          url = "https://github.com/SagerNet/sing-geoip/raw/rule-set/geoip-ru.srs";
          download_detour = "proxy";
          # Use this when sing-box version gets to 1.14
          # http_client.dial.detour = "proxy";
        }
      ];
      experimental.cache_file.enabled = true;
    };
  };

  sops.secrets."xray_user1_uuid" = {
    sopsFile = "${secrets-dir}/xray.yaml";
  };
  sops.secrets."xray_server_address" = {
    sopsFile = "${secrets-dir}/xray.yaml";
  };
  sops.secrets."xray_vless_domain" = {
    sopsFile = "${secrets-dir}/xray.yaml";
  };
  sops.secrets."xray_public_key" = {
    sopsFile = "${secrets-dir}/xray.yaml";
  };
}
