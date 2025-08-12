{ util, config, lib, ... }:
{
  imports =
    []
    ++ (util.gen-modules-in-dir config [ "my-nixos" "features" ] ./features);
  my-nixos.features = {
    nix.enable = lib.mkDefault true;
    keyd.enable = lib.mkDefault true;
    pipewire.enable = lib.mkDefault true;
    sudo.enable = lib.mkDefault true;
    polkit.enable = lib.mkDefault true;
  };
}
