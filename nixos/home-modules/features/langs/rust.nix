{ config, ... }:
let
  xdg = config.xdg;
in {
  # placeholder
  home.sessionVariables = {
    CARGO_HOME = "${xdg.dataHome}/cargo";
    RUSTUP_HOME = "${xdg.dataHome}/rustup";
  };
  # doubt: PATH=${PATH}:${CARGO_HOME}/bin
}
