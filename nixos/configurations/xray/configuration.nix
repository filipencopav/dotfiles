{ pkgs, ... }:
{
  imports = [
    ./disko.nix
    ./hardware-configuration.nix
    ./xray.nix
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };
  
  security.sudo.wheelNeedsPassword = false;

  networking.networkmanager.enable = true;
  networking.hostName = "xray";
  
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };

  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQNrhBwMxwuM56z43LNcLVogFD7dlNA7H7H1Tt2HXe6 pavel"
      ];
    };
    xray = {
      isNormalUser = true; 
      shell = pkgs.bash; 
      description = "nixos-xray user"; 
      extraGroups = [ 
        "networkmanager" 
        "wheel" 
      ]; 
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQNrhBwMxwuM56z43LNcLVogFD7dlNA7H7H1Tt2HXe6 pavel"
      ];
    };
  };

  system.stateVersion = "25.11";
}
