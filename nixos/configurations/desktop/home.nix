{ inputs, config, pkgs, util, ... }:
let
  user = "pavel";
  homedir = "/home/${user}";
  wrap-nixgl = util.wrap-nixgl-if config.my.features.nixgl.enable config;
in {
  imports = [
    inputs.stylix.homeModules.stylix
  ];

  # non-nixOS usage
  # targets.genericLinux.enable = true;

  my.features = {
    # programs
    niri.enable = true;
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

    langs = {
      clojure.enable = true;
      java.enable = true;
      js.enable = true;
      go.enable = true;
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
    stateVersion = "25.05";

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
      e = "$EDITOR";
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
      # TODO: regroup packages to appropriate bundles, if necessary
      tldr
      android-tools
      (wrap-nixgl ardour) # TODO: figure out audio
      # (wrap-nixgl audacity)
      clang
      (wrap-nixgl vesktop)
      gcolor3
      htop
      (wrap-nixgl imv)
      (wrap-nixgl inkscape)
      # (wrap-nixgl lutris)
      (wrap-nixgl mpv)
      (wrap-nixgl obs-studio)
      (wrap-nixgl openttd)
      ripgrep
      shellcheck
      (wrap-nixgl _64gram)
      # (wrap-nixgl telegram-desktop)
      (wrap-nixgl thunderbird-bin)
      (wrap-nixgl transmission_4-gtk)
      yt-dlp
      zip
      wl-clipboard
      typst

      (wrap-nixgl zathura) # TODO: enable djvu and pdf-mupdf
    ];
  };
}
