{ pkgs, config, ... }:
let
  GOPATH = "${config.xdg.dataHome}/go";
  GOBIN = "${GOPATH}/bin";
in
{
  home.packages = [
    pkgs.go
    pkgs.go-tools
    pkgs.golangci-lint
    pkgs.delve
  ];

  home.sessionPath = [
    "${GOPATH}/bin"
  ];

  home.sessionVariables = {
    inherit GOPATH GOBIN;
  };
}
