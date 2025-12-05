{ ... }:
{
  programs.git = {
    enable = true;
    lfs.enable = true;

    settings = {
      user.name = "Pavel Filipenco";
      user.email = "filipencop@mail.ru";

      diff.algorithm = "histogram";
      init.defaultBranch = "main";
      pull.rebase = true;
    };

    ignores = [
      "*~"
    ];
  };

  # See https://github.com/dandavison/delta
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };
}
