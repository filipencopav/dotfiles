{pkgs, inputs, lib, config, ...}:
let
  secrets = inputs.secrets-folder;
  fillSecrets =
    file: format: dir: keys: lib.attrsets.mergeAttrsList (
      builtins.map
        (key: {
          "${file}/${key}" = {
            inherit key;
            format = format;
            sopsFile = "${secrets}/${file}.${format}";
            path = "${dir}/${key}";
          };})
        keys);
in
{
  home.packages = [
    pkgs.rage
    pkgs.sops
  ];
  sops = let in {
    age.keyFile = "/mnt/sops-secrets/age-key.txt";

    secrets = {
      passwords = {
        key = "";
        format = "yaml";
        sopsFile = "${secrets}/passwords.yaml";
      };
    } // fillSecrets "ssh" "yaml" "${config.home.homeDirectory}/.ssh" [
      "github_filipencopav"
      "id_ed25519"
      "twc"
      "vision_server"
    ];
  };
}
