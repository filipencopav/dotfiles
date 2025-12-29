{ inputs, system, ...}:
{
  home.packages = [
    inputs.code-cursor-nix.packages."${system}".cursor
  ];
}
