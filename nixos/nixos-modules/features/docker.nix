{ lib, config, ... }:
let
  cfg = config.my-nixos.features.docker;
in {
  options = {
    my-nixos.features.docker = {
      rootless = lib.mkOption {
        default = false;
        example = true;
        description = ''Whether to enable rootless mode for docker.
          Also sets the socket variable $DOCKER_HOST to its appropriate value
        '';
        type = lib.types.bool;
      };
      enable-user-systemd-socket = lib.mkOption {
        default = false;
        example = true;
        description = ''Whether to enable user systemd socket for docker.'';
        type = lib.types.bool;
      };
    };
  };

  virtualisation.docker = {
    enable = true;

    storageDriver = "btrfs";

    daemon.settings = {
      experimental = true;
      userland-proxy = false;
    };
    autoPrune.enable = true;
    
    rootless = {
      enable = cfg.rootless;
      setSocketVariable = true;
      daemon.settings = {
        experimental = true;
        userland-proxy = false;
      };
    };
  };
  systemd.user.sockets.docker.wantedBy =
    lib.mkIf cfg.enable-user-systemd-socket [ "default.target" ];
}
