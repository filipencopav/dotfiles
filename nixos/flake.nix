{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    niri.url = "github:sodiboo/niri-flake";

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
  let
    util = import ./util inputs;
  in {
    # NOTE: 'nixos' is the default hostname
    nixosConfigurations.desktop =
      util.mk-system
        "x86_64-linux"
        ./configurations/desktop/configuration.nix;

    homeConfigurations."pavel@desktop" =
      util.mk-home
        "x86_64-linux"
        ./configurations/desktop/home.nix;

    nixosModules.default = ./nixos-modules;
    home-modules.default = ./home-modules;
  };
}
