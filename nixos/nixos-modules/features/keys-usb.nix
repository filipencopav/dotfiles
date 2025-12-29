{pkgs, ...}:
{
  systemd.tmpfiles.rules = [ "d /mnt/sops-secrets 0755 root root" ];
  
  fileSystems."/mnt/sops-secrets" = {
    device = "/dev/disk/by-label/sops-secrets";
    fsType = "ext4";
    options = [ "nofail" "x-systemd.automount" ];
  };

  services.udev.enable = true;
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="block", ENV{ID_FS_LABEL}=="sops-secrets", \
    RUN+="${pkgs.systemd}/bin/systemctl start mnt-sops\\x2dsecrets.automount"
  '';
}
