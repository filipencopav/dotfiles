inputs @ { nixpkgs, home-manager, ... }:
let
  outputs = inputs.self.outputs;
  util = import ./default.nix inputs;
  lib = nixpkgs.lib;


  assoc-in = set: where: val: (
    lib.attrsets.updateManyAttrsByPath
      [{path = where; update = _: val;}]
      set);

  get-module-name = module-path: (
    builtins.unsafeDiscardStringContext
      (builtins.head
        (builtins.split "\\." (baseNameOf module-path))));

  get-files-in =
    dir-path: (map (filename: "${dir-path}/${filename}")
      (builtins.attrNames (builtins.readDir dir-path)));

  # `wrap-modules` imports all modules from `paths` and executes an arbitrary
  # function `wrapper` for each module config, having its inputs be the module name + the
  # module itself.
  #
  # `paths` = list of path
  # `wrapper` = function { module, moduleName, ... } -> moduleAttributeSet
  wrap-modules =
    paths: wrapper: let
    in (
      map (module-path: module-args: (
        wrapper {
          moduleName = get-module-name module-path;
          module = (import module-path) module-args;
        }))
        paths);

  # `wrap-modules-in-directory` imports all modules in `dir` and executes an arbitrary
  # function `wrapper` for each module config, having its inputs be the module name + the
  # module itself.
  #
  # `dir` = path
  # `wrapper` = function { module, moduleName, ... } -> moduleAttributeSet
  wrap-modules-in-directory =
    dir: wrap-modules (get-files-in dir);

  # `gen-modules` imports all modules from `paths` and ensures they have the module options,
  # config and imports properly set up. If the module has a `config` attribute, uses that and
  # adds an enable option to it automatically. If the module has `options` and/or `imports`,
  # uses them as well. If the module has no `config` attribute, then all its attributes
  # (except for `imports` and `options`) will be put inside the newly created `config`
  # attribute
  #
  # `paths` = list of path
  #
  # Example:
  # Suppose we have a module in `./simple1.nix`:
  # ```nix
  #   { lib, config, ... }: {
  #     options = { some.ns.simple1.text = lib.mkOption { ... } };
  #     home.file."simplerc".text = config.some.ns.simple1.text;
  #   }
  # ```
  #
  # With `(gen-modules config "some.ns" [ ./simple1.nix ])`
  # this module would be effectively transformed into:
  # ```nix
  #   { lib, config, ... }: {
  #     options = {
  #       some.ns.simple1.enable = lib.mkEnableOption "Enables simple feature";
  #       some.ns.simple1.text = lib.mkOption { ... };
  #     };
  #     config = lib.mkIf config.some.ns.simple1.enable {
  #       home.file."simplerc".text = config.some.ns.simple1.text;
  #     };
  #     imports = [];
  #   }
  # ```
  gen-modules =
    config: prefix: paths: (
      wrap-modules paths (
        { moduleName,
          module ? {},
          ...
        }: let
          cfg     = module.config  or (removeAttrs module ["options" "imports"]);
          options = module.options or {};
          imports = module.imports or [];
          enable-attr-path =  prefix ++ [moduleName "enable"];
        in {
          config =
            lib.mkIf (lib.attrsets.attrByPath enable-attr-path false config)
              cfg;
          options =
            (assoc-in options enable-attr-path
              (lib.mkEnableOption "Enables ${moduleName} feature"));
          imports = imports;
        }));

  gen-modules-in-dir = config: prefix: dir: gen-modules config prefix (get-files-in dir);

  overlay-module = {
    nixpkgs.overlays = import ./../overlays {inherit inputs;};
  };
in {
  inherit get-module-name get-files-in gen-modules gen-modules-in-dir wrap-modules-in-directory;

  wrap-nixgl-if =
    is-enabled: config: value: (
      if is-enabled
      then config.lib.nixGL.wrap value
      else value);

  mk-system = system: config: let
  in nixpkgs.lib.nixosSystem {
    specialArgs = { inherit inputs outputs util; };
    modules = [
      config
      outputs.nixosModules.default
      overlay-module
    ];
  };

  mk-home = system: config: let
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
  in home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = { inherit inputs system pkgs util; };
    modules = [
      config
      outputs.home-modules.default
      overlay-module
    ];
  };
}
