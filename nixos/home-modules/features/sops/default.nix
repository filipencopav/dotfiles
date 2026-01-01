{pkgs, inputs, lib, config, ...}:
let
  secrets = inputs.secrets-folder;
  fillSecrets =
    file: format: dir: keys: lib.attrsets.mergeAttrsList (
      map
        (key: {
          "${file}/${key}" = {
            inherit key;
            format = format;
            sopsFile = "${secrets}/${file}.${format}";
            path = "${dir}/${key}";
          };})
        keys);
in {
  imports = [
    inputs.sops-nix.homeManagerModules.sops
  ];

  home.packages = [
    pkgs.rage
    pkgs.sops
  ];

  sops = let HOME = config.home.homeDirectory;
  in {
    age.keyFile = "/mnt/sops-secrets/age-key.txt";

    secrets = {
      passwords = {
        key = "";
        format = "yaml";
        sopsFile = "${secrets}/passwords.yaml";
      };
    } // fillSecrets "ssh" "yaml" "${HOME}/.ssh" [
      "id_ed25519"
      "id_ed25519.pub"
      "vision_server"
      "vision_server.pub"
    ];
  };
}
