{ pkgs, util, config, ... }:
let
  wrap-nixgl = util.wrap-nixgl-if config.my.features.nixgl.enable config;
in {
  home.packages = [
    (wrap-nixgl pkgs.steam)
  ];
}
