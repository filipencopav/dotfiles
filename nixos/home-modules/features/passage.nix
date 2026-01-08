{pkgs, config, ...}:
let
  pkg = pkgs.passage;
in
{
  home.packages = [
    pkg
  ];

  home.sessionVariables = {
    PASSAGE_DIR = "${config.xdg.dataHome}/passage";
    PASSAGE_IDENTITIES_FILE = config.sops.age.keyFile;
  };

  my.features.passfuzzel.enable = true;
}
