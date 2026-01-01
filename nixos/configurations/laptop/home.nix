{ inputs, config, pkgs, util, ... }:
let
  user = "pavel";
  homedir = "/home/${user}";
in {
  imports = [
    inputs.stylix.homeModules.stylix
  ];

  my.features = {
    # programs
    niri.enable = true;
    niri.show-battery = true;
    kitty.enable = true;
    git.enable = true;
    nixgl.enable = false;
    mako.enable = true;
    bash.enable = true;
    starship.enable = true;
    emacs.enable = true;
    emacs.defaultEditor = true;
    gpg.enable = true;
    pass.enable = true;
    steam.enable = true;
    chromium.enable = true;
    sops.enable = true;

    langs = {
      clojure.enable = true;
      java.enable = true;
      lisp.enable = true;
    };

    stylix.enable = true;

    user-dirs.enable = true;

    # styles
    gtk.enable = true;
  };

  # allow home-manager to install and manage itself
  programs.home-manager.enable = true;

  # session
  home = {
    stateVersion = "25.11";

    username = user;
    homeDirectory = homedir;


    sessionPath = [
      "${homedir}/.local/bin"
    ];

    sessionVariables = {
      LC_ALL = "C.UTF-8";
      LESSHISTFILE = "-";
    };

    shellAliases = {
      dotconf = ''git --git-dir="$HOME/dotfiles/" --work-tree="$HOME"'';

      # youtube downloader alias
      youtube-dl = "yt-dlp -o '%(title)s.%(ext)s'";
      audio-dl = "yt-dlp -ic -x -f bestaudio/best";

      # QOL basic system navigation commands
      rm = "rm -Iv";
      mkdir = "mkdir -pv";
      grep = "grep --color=auto";
      ls = "ls -hN --color=auto --group-directories-first";
      ll = "ls --color=auto -l -a";
      ssh = "TERM=xterm ssh";
      # TODO: remove when on nixOS
      update-grub = "grub-mkconfig -o /boot/grub/grub.cfg";

      wget = ''wget --hsts-file="$XDG_DATA_HOME/wget-hsts"'';
    };

    packages = with pkgs; [
      ardour
      audacity
      vesktop
      gcolor3
      htop
      imv
      inkscape
      mpv
      obs-studio
      openttd
      ripgrep
      _64gram
      transmission_4-gtk
      yt-dlp
      unzip
      zip
      wl-clipboard
      typst

      code-cursor

      zathura
    ];
  };
}
