{ config, ... }:
let
    homedir = config.home.homeDirectory;
in
{
  xdg = {
    enable = true;
    userDirs = {
      enable = true;

      desktop     = "${homedir}/dskt";
      download    = "${homedir}/dwn";
      templates   = "${homedir}/tmpl";
      publicShare = "${homedir}/pub";
      documents   = "${homedir}/dox";
      music       = "${homedir}/mus";
      pictures    = "${homedir}/pix";
      videos      = "${homedir}/vids";
    };
  };
}
