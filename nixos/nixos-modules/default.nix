{ pkgs, util, config, lib, ... }:
let
  pass-pkgs = (module: module-args: module ({inherit pkgs;} // module-args));
  features = (
    map pass-pkgs
      (util.gen-modules-in-dir
        config
        [ "my-nixos" "features" ]
        ./features));
in {
  imports =
    []
    ++ features;

  config = {
    my-nixos.features = {
      nix.enable = lib.mkDefault true;
      keyd.enable = lib.mkDefault true;
      pipewire.enable = lib.mkDefault true;
      sudo.enable = lib.mkDefault true;
      polkit.enable = lib.mkDefault true;
      memory.enable = lib.mkDefault true;
    };
  };
}
