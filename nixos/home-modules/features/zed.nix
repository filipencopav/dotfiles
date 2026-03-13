{ pkgs, lib, ... }:
let
  cursor-pkg = pkgs.cursor-cli;
  cursor-agent-with-model = name: {
    type = "custom";
    command = "${lib.getExe cursor-pkg}";
    args = [
      "--model"
      name
      "acp"
    ];
    env = { };
  };
in
{
  programs.zed-editor = {
    enable = true;
    extensions = [
      "nix"
      "clojure"
    ];
    extraPackages = [ cursor-pkg ];

    userSettings = {
      agent_servers = {
        "cursor (kimi k2.5)" = cursor-agent-with-model "kimi-k2.5";
        "cursor (sonnet)" = cursor-agent-with-model "sonnet-4.6";
        "cursor (opus)" = cursor-agent-with-model "opus-4.6";
        "cursor (custom)" = {
          type = "custom";
          command = "${lib.getExe cursor-pkg}";
          "args" = [ "acp" ];
          "env" = { };
        };
      };
    };
  };
}
