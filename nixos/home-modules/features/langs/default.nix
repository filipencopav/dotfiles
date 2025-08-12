{ config, util, lib, ... }:
{
  imports = (util.gen-modules config [ "my" "features" "langs" ] [
    ./erlang.nix
    ./lisp.nix
    ./rust.nix
    ./clojure.nix
    ./java.nix
    ./go.nix
    ./js.nix
  ]);
}
