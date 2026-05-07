{
  config,
  inputs,
  pkgs,
  ...
}:
let
  secrets = inputs.secrets-folder;
  lib = pkgs.lib;
  HAPROXY_PORT = 8081;
  NGINX_PORT = 8003;
in
{
  sops.secrets.nextcloud_pass = {
    owner = "nextcloud";
    sopsFile = "${secrets}/xray.yaml";
  };

  services.nextcloud = {
    enable = true;
    hostName = "localhost";
    database.createLocally = true;
    autoUpdateApps.enable = true;
    config = {
      # only specify dbtype if using postgresql db
      dbtype = "sqlite";
      adminuser = "admin";
      adminpassFile = config.sops.secrets.nextcloud_pass.path;
    };
    settings.trusted_proxies = [ "127.0.0.1" ];
  };

  sops.templates."trusted_domains.config.php" = {
    owner = "nextcloud";
    content = ''
      <?php
      $CONFIG = [
        'trusted_domains' => [
          'localhost', '${config.sops.placeholder."xray_vless_domain"}' ]];
    '';
  };
  systemd.tmpfiles.rules =
    let
      datadir = config.services.nextcloud.home;
    in
    [
      "L+ ${datadir}/config/trusted_domains.config.php - - - - ${
        config.sops.templates."trusted_domains.config.php".path
      }"
    ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."${config.services.nextcloud.hostName}".listen = [
      {
        addr = "127.0.0.1";
        port = NGINX_PORT;
        ssl = lib.mkForce false;
      }
    ];
  };

  sops.templates."haproxy_cert.pem" = {
    owner = "haproxy";
    content = lib.concatStrings (
      lib.map lib.strings.trim [
        config.sops.placeholder.domain-cert
        config.sops.placeholder.intermediate-cert
        config.sops.placeholder.domain-cert-key
      ]
    );
  };

  systemd.services.haproxy.after = [ "nginx.service" ];
  services.haproxy = {
    enable = true;
    config = ''
      global
          log /dev/log local0
          ssl-default-bind-options ssl-min-ver TLSv1.2
          maxconn 4096

      defaults
          mode http
          log global
          option httplog
          option dontlognull
          timeout connect 5s
          timeout client 30s
          timeout server 30s

      frontend http_in
          bind *:80
          http-request redirect scheme https code 301
          option httplog

      # HTTPS listener with TLS termination
      frontend https_in
          bind *:${toString HAPROXY_PORT} ssl crt ${
            config.sops.templates."haproxy_cert.pem".path
          } alpn h2,http/1.1 accept-proxy
          http-request set-header X-Forwarded-Proto https
          http-request set-header X-Forwarded-Host %[req.hdr(Host)]
          http-request set-header X-Forwarded-Port 443
          http-request set-header X-Real-IP %[src]
          default_backend app_server

      # Forward to backend HTTP service
      backend app_server
          http-request set-header Host localhost
          server backend1 127.0.0.1:${toString NGINX_PORT} check
      #
    '';
  };

  sops.secrets.domain-cert = {
    owner = "haproxy";
    key = "";
    format = "binary";
    sopsFile = "${secrets}/xray-domain.crt.enc";
  };
  sops.secrets.intermediate-cert = {
    owner = "haproxy";
    key = "";
    format = "binary";
    sopsFile = "${secrets}/xray-intermediate.crt.enc";
  };
  sops.secrets.domain-cert-key = {
    owner = "haproxy";
    key = "";
    format = "binary";
    sopsFile = "${secrets}/xray-domain.key.enc";
  };
}
