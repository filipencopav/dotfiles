{ inputs, lib, config, ...}:
let
  secrets = inputs.secrets-folder;
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  systemd.services.sops-install-secrets.after
    = lib.mkIf (config.my-nixos.features.keys-usb.enable && config.sops.useSystemdActivation) [
    "systemd-udevd.service"
  ];
  sops = {
    age.keyFile = "/mnt/sops-secrets/age-key.txt";
    useSystemdActivation = true;

    secrets = {
      "wg-env" = {
        key = "";
        format = "dotenv";
        sopsFile = "${secrets}/wg.env";
      };
    };
  };
}
