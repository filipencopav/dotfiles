{ pkgs, ... }:
{
  home.packages = [
    pkgs.mg
  ];

  home.file.".mg".text = ''
    make-backup-files 0
    column-number-mode 1
    auto-execute *.c c-mode
    auto-execute *.h c-mode
  '';
}
