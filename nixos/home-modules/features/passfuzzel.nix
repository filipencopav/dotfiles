{pkgs, lib, config, inputs, ...}:
let
  cfg = config.my.features.passfuzzel;
  derivations = (import inputs.derivs {});
  pkg = pkgs.callPackage derivations.passfuzzel {};
in
{
  options.my.features.passfuzzel = {
    package = lib.mkPackageOption derivations "passfuzzel" {
      example = "derivations.passfuzzel { pass = pkgs.passage; }";
    };
  };

  my.features.passfuzzel.package = lib.mkDefault pkg;

  home.packages = [
    cfg.package
  ];
}
