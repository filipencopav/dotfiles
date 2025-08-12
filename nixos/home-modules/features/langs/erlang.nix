{ pkgs, ... }:
{
  home.packages = [
    (pkgs.beam28Packages.erlang.override {
      odbcSupport = true;
      odbcPackages = [ pkgs.unixODBC ];
      javacSupport = false;
    })
    pkgs.erlang-language-platform.override
  ];

  home.sessionVariables = {
  };
}
