{ lib, config, ... }:
{
  stylix.targets.starship.enable = true;
  programs.starship = {
    enable = true;
    settings = {
      format = lib.concatStrings [
        "$directory"
        "$git_branch"
        "$git_status"
        "$nix_shell"
        "$line_break$status; "
      ];
      command_timeout = 1000;
      directory = {
        style = "bold bg:yellow fg:black";
        format = "[\\[$path\\]]($style)";
        truncation_length = 3;
        truncation_symbol = "…/";

        substitutions = lib.mkIf
          config.xdg.userDirs.enable (with config.xdg.userDirs; {
            ${documents} = "󰈙 ";
            ${download} = " ";
            ${music} = "󰝚 ";
            ${pictures} = " ";
          });
      };

      git_branch = {
        symbol = "";
        style = "fg:cyan";
        format = "[ $symbol $branch]($style)";
      };
      git_status = {
        ahead = "($count)↑";
        behind = "($count)↓";
        untracked = "?";
        modified = "*";
        deleted = "-";
        style = "fg:cyan";
        format = lib.concatStrings [
          "([ ($ahead_behind )"
          "("
          "$stashed"
          "$untracked"
          "$modified"
          "$staged"
          "$renamed"
          "$typechanged"
          "$deleted"
          "$conflicted"
          ")"
          "]($style))"
        ];
      };

      line_break.disabled = false;

      character = {
        disabled = false;
        format = "$symbol";
        success_symbol = "[0](bold fg:cyan)";
        error_symbol = "[$status](bold fg:red)";
        vimcmd_symbol = "[](bold fg:green)";
        vimcmd_replace_one_symbol = "[](bold fg:purple)";
        vimcmd_replace_symbol = "[](bold fg:purple)";
        vimcmd_visual_symbol = "[](bold fg:yellow)";
      };

      status = {
        disabled = false;
        style = "bold fg:cyan";
        symbol = "";
        success_symbol = "0";
        format = "[($common_meaning )$int]($style)";
        map_symbol = false;
      };

      nix_shell = {
        style = "bold fg:blue";
        symbol = "[ ❄️]($style)";
        format = "[$symbol $state( «$name»)]($style)";
      };
    };
  };
}
