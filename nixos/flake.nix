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

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    secrets-folder = {
      url = "path:./secrets";
      flake = false;
    };

    derivs = {
      url = "path:./derivations";
      flake = false;
    };

    code-cursor-nix = {
      url = "github:jacopone/code-cursor-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
  let
    lib = inputs.nixpkgs.lib;
    forAllSystems = lib.genAttrs lib.systems.flakeExposed;
    util = import ./util inputs;
  in {
    nixosConfigurations.laptop =
      util.mk-system
        "x86_64-linux"
        ./configurations/laptop/configuration.nix;
    homeConfigurations."pavel@laptop" =
      util.mk-home
        "x86_64-linux"
        ./configurations/laptop/home.nix;

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

    devShells = forAllSystems (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.just ];
          };
        }
    );
  };
}
