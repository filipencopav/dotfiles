{ pkgs, inputs, config, ... }:
let
  features = config.my-nixos.features;
in {
  imports = [
    inputs.stylix.nixosModules.stylix
  ];
  stylix = with pkgs; {
    enable = true;
    autoEnable = false;

    base16Scheme = "${base16-schemes}/share/themes/tomorrow-night.yaml";
    fonts = {
      serif = {
        package = ibm-plex;
        name = "IBM Plex Serif";
      };
    
      sansSerif = {
        package = ibm-plex;
        name = "IBM Plex Sans";
      };
    
      monospace = {
        package = nerd-fonts.fantasque-sans-mono;
        name = "FantasqueSansM Nerd Font";
      };
    
      emoji = {
        package = twemoji-color-font;
        name = "Twitter Color Emoji";
      };
    
      sizes = {
        applications = 11;
        terminal = 11;
        popups = 11;
      };
    };

    targets.regreet.enable = features.regreet.enable;
  };
}
