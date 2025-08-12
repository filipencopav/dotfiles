{ config, util, pkgs, ... }:
let
  wrap-nixgl = util.wrap-nixgl-if config.my.features.nixgl.enable config;
in
{
  home.packages = [
    (wrap-nixgl pkgs.ungoogled-chromium)
  ];
}
