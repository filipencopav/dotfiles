{ pkgs, config, inputs, ... }:
let
  secrets = inputs.secrets-folder;
  users = [ "user1" "user2" "user3" ];
  mk-client-config = (
    user: {
      id = config.sops.placeholder."xray_${user}_uuid";
      email = "${user}@xray";
      flow = "xtls-rprx-vision";
    });
  client-configs = pkgs.lib.map
    mk-client-config
    users;

  mk-secret-config-kv = (
    user: {
      name = "xray_${user}_uuid";
      value = {
        owner = "xray";
        sopsFile = "${secrets}/xray.yaml";
      };
    });
  secret-configs = pkgs.lib.listToAttrs
    (pkgs.lib.map mk-secret-config-kv
      users);

  user-links = pkgs.writeShellScriptBin "user-links"
    (pkgs.lib.concatMapStringsSep "\n"
      (user: let s = config.sops.secrets; in ''
        UUID=$(cat ${s."xray_${user}_uuid".path})
        ADDR=$(cat ${s."xray_server_address".path})
        PBK=$(cat ${s."xray_public_key".path})
        DOMAIN=$(cat ${s."xray_vless_domain".path})
        echo "vless://$UUID@$ADDR:443?security=reality&encryption=none&pbk=$PBK&headerType=none&fp=chrome&type=tcp&flow=xtls-rprx-vision&sni=$DOMAIN&sid=aa00#pb-${user}"
        echo
      '')
    users);
in {
  imports = [
    ./disko.nix
    ./hardware-configuration.nix
    inputs.sops-nix.nixosModules.sops
  ];

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 443 ];
  };

  environment.systemPackages = [
    pkgs.xray
    user-links
  ];
  services.xray = {
    enable = true;
    settingsFile = config.sops.templates."xray.json".path;
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
            clients = client-configs;
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

  sops.secrets = secret-configs // {
    "xray_server_address" = {
      owner = "xray";
      sopsFile = "${secrets}/xray.yaml";
    };
    "xray_vless_domain" = {
      owner = "xray";
      sopsFile = "${secrets}/xray.yaml";
    };
    "xray_public_key" = {
      owner = "xray";
      sopsFile = "${secrets}/xray.yaml";
    };
    "xray_private_key" = {
      owner = "xray";
      sopsFile = "${secrets}/xray.yaml";
    };
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };
  
  security.sudo.wheelNeedsPassword = false;

  networking.hostName = "xray";
  networking.networkmanager.enable = true;
  
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };

  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQNrhBwMxwuM56z43LNcLVogFD7dlNA7H7H1Tt2HXe6 pavel"
      ];
    };
    xray = {
      isNormalUser = true; 
      shell = pkgs.bash; 
      description = "nixos-xray user"; 
      extraGroups = [ 
        "networkmanager" 
        "wheel" 
      ]; 
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQNrhBwMxwuM56z43LNcLVogFD7dlNA7H7H1Tt2HXe6 pavel"
      ];
    };
  };

  system.stateVersion = "25.11";
}
