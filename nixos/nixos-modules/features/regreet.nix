{ ... }:
{
  services.displayManager.regreet = {
    enable = true;
    cageArgs = [
      "-s"
      "-m"
      "last"
    ];
  };
}
