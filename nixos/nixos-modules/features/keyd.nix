{ ... }:
{
  services.keyd.enable = true;
  services.keyd.keyboards = {
    default = {
      ids = [ "*" ];
      settings = {
        main = {
          capslock = "esc";
        };
      };
    };
  };
}
