{ config, ... }:
{
  programs.bash = {
    enable = true;

    # Note, if you use NixOS or nix-darwin and do not have Bash completion
    # enabled in the system configuration, then make sure to add
    #
    # environment.pathsToLink = [ "/share/bash-completion" ];
    # to your system configuration to get completion for system packages.
    enableCompletion = true;

    historyControl = ["erasedups" "ignorespace"];
    historyIgnore  = ["clear" "ls" "history"];
    historyFile    = "${config.xdg.stateHome}/bash/history";


    bashrcExtra = ''
      complete -cf doas

      # this is needed because home.sessionVariables aren't put into .bashrc, so we source # them from .profile
      [[ -f ~/.profile ]] && . ~/.profile
    '';

    # this is needed because home.sessionVariables don't get put into .profile by default
    sessionVariables = config.home.sessionVariables;
  };

  home.shell.enableBashIntegration = true;
}
