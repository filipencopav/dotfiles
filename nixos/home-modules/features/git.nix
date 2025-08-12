{ lib, ... }:
{
  programs.git = {
    enable = true;
    userName = "Pavel Filipenco";
	userEmail = "filipencop@mail.ru";

    # See https://github.com/dandavison/delta
    delta.enable = true;

    extraConfig = {
      diff.algorithm = "histogram";
      init.defaultBranch = "main";
      pull.rebase = true;
    };

    ignores = [
      "*~"
    ];
  };
}
