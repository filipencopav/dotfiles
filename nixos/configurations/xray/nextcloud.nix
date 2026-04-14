{
  config,
  inputs,
  pkgs,
  ...
}:
let
  lib = pkgs.lib;
  secrets = inputs.secrets-folder;
in
{
  sops.secrets.nextcloud_pass = {
    owner = "nextcloud";
    sopsFile = "${secrets}/xray.yaml";
  };

  services.nextcloud = {
    enable = true;
    # specify either a domain you own or localhost
    hostName = "localhost";
    autoUpdateApps.enable = true;
    https = true;
    config = {
      # only specify dbtype if using postgresql db
      dbtype = "pgsql";
      dbname = "nextcloud";
      dbuser = "nextcloud";
      # default directory for postgresql, ensures automatic setup of db
      dbhost = "/run/postgresql";
      adminuser = "admin";
      # specified using agenix, provide path to file as alternative
      adminpassFile = config.sops.secrets.nextcloud_pass.path;
      # error thrown unless specified
      defaultPhoneRegion = "AU";
    };
    settings.trusted_proxies = [ "127.0.0.1" ];
  };

  services = {
    postgresql = {
      enable = true;
      ensureDatabases = [ "nextcloud" ];
      ensureUsers = [
        {
          name = "nextcloud";
          ensureDBOwnership = true;
        }
      ];
    };
  };

  # ensure postgresql db is started with nextcloud
  systemd = {
    services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
  };

  sops.secrets.domainCert = {
    key = "";
    format = "binary";
    sopsFile = "${secrets}/xray-domain.crt.enc";
  };
  sops.secrets.domainCertKey = {
    key = "";
    format = "binary";
    sopsFile = "${secrets}/xray-domain.key.enc";
  };

  sops.templates."nginx.conf" = {
    owner = "nginx";
    content = ''
      user nginx;
      worker_processes auto;

      error_log /var/log/nginx/error.log notice;
      pid /var/run/nginx.pid;

      events {
          worker_connections 1024;
      }

      http {
          log_format main '[$time_local] $proxy_protocol_addr "$http_referer" "$http_user_agent"';
          access_log /var/log/nginx/access.log main;

          map $http_upgrade $connection_upgrade {
             default upgrade;
             ""      close;
          }

          map $proxy_protocol_addr $proxy_forwarded_elem {
              ~^[0-9.]+$        "for=$proxy_protocol_addr";
              ~^[0-9A-Fa-f:.]+$ "for=\"[$proxy_protocol_addr]\"";
              default           "for=unknown";
          }

          map $http_forwarded $proxy_add_forwarded {
              "~^(,[ \\t]*)*([!#$%&'*+.^_`|~0-9A-Za-z-]+=([!#$%&'*+.^_`|~0-9A-Za-z-]+|\"([\\t \\x21\\x23-\\x5B\\x5D-\\x7E\\x80-\\xFF]|\\\\[\\t \\x21-\\x7E\\x80-\\xFF])*\"))?(;([!#$%&'*+.^_`|~0-9A-Za-z-]+=([!#$%&'*+.^_`|~0-9A-Za-z-]+|\"([\\t \\x21\\x23-\\x5B\\x5D-\\x7E\\x80-\\xFF]|\\\\[\\t \\x21-\\x7E\\x80-\\xFF])*\"))?)*([ \\t]*,([ \\t]*([!#$%&'*+.^_`|~0-9A-Za-z-]+=([!#$%&'*+.^_`|~0-9A-Za-z-]+|\"([\\t \\x21\\x23-\\x5B\\x5D-\\x7E\\x80-\\xFF]|\\\\[\\t \\x21-\\x7E\\x80-\\xFF])*\"))?(;([!#$%&'*+.^_`|~0-9A-Za-z-]+=([!#$%&'*+.^_`|~0-9A-Za-z-]+|\"([\\t \\x21\\x23-\\x5B\\x5D-\\x7E\\x80-\\xFF]|\\\\[\\t \\x21-\\x7E\\x80-\\xFF])*\"))?)*)?)*$" "$http_forwarded, $proxy_forwarded_elem";
              default "$proxy_forwarded_elem";
          }

          server {
              listen 80;
              listen [::]:80;
              return 301 https://$host$request_uri;
          }

          server {
              listen                  127.0.0.1:8001 ssl default_server;

              ssl_reject_handshake    on;

              ssl_protocols           TLSv1.2 TLSv1.3;

              ssl_session_timeout     1h;
              ssl_session_cache       shared:SSL:10m;
          }

          server {
              listen                     127.0.0.1:8001 ssl proxy_protocol;
              http2                      on; # This directive appeared in version 1.25.1. Otherwise use it like this. "listen 127.0.0.1:8001 ssl http2 proxy_protocol;"

              set_real_ip_from           127.0.0.1;
              real_ip_header             proxy_protocol;

              server_name                ${config.sops.placeholder."xray_vless_domain"};

              ssl_certificate            ${config.sops.placeholder.domainCert};
              ssl_certificate_key        ${config.sops.placeholder.domainCertKey};

              ssl_protocols              TLSv1.2 TLSv1.3;
              ssl_ciphers                TLS13_AES_128_GCM_SHA256:TLS13_AES_256_GCM_SHA384:TLS13_CHACHA20_POLY1305_SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305;
              ssl_prefer_server_ciphers  on;

              ssl_stapling               on;
              ssl_stapling_verify        on;
              resolver                   1.1.1.1 valid=60s;
              resolver_timeout           2s;

              location / {
                  set $website ${config.sops.placeholder."xray_vless_domain"};
                  proxy_pass                            http://127.0.0.1:8080;

                  proxy_set_header Host                 $proxy_host;

                  proxy_http_version                    1.1;
                  proxy_cache_bypass                    $http_upgrade;

                  proxy_ssl_server_name                 on;

                  proxy_set_header Upgrade              $http_upgrade;
                  proxy_set_header Connection           $connection_upgrade;
                  proxy_set_header X-Real-IP            $proxy_protocol_addr;
                  proxy_set_header Forwarded            $proxy_add_forwarded;
                  proxy_set_header X-Forwarded-For      $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Proto    $scheme;
                  proxy_set_header X-Forwarded-Host     $host;
                  proxy_set_header X-Forwarded-Port     $server_port;

                  proxy_connect_timeout                 60s;
                  proxy_send_timeout                    60s;
                  proxy_read_timeout                    60s;
              }
          }
      }
    '';
  };

  systemd.services.nginx =
    let
      nginxCfg = config.services.nginx;
      nginxConfigPath = config.sops.templates."nginx.conf".path;
      execCommand = "${nginxCfg.package}/bin/nginx -c '${nginxConfigPath}'";
    in
    {
      preStart = lib.mkForce ''
        ${nginxCfg.preStart}
        ${execCommand} -t
      '';
      wants = lib.mkForce [
        "xray.service"
        "network.target"
      ];
      serviceConfig = {
        ExecStart = lib.mkForce execCommand;
        ExecReload = lib.mkForce [
          "${execCommand} -t"
        ];
      };
    };

  services.nginx = {
    enable = true;
    # to have it load the file from /etc/nginx/nginx.conf
    enableReload = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };
}
