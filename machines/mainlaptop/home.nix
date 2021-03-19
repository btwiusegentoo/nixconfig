# This file is generated from "README.org"
{ config, pkgs, lib, ... }:
let

  haskell-env = pkgs.unstable.haskellPackages.ghcWithHoogle (
    hp: with hp; [
      xmonad
      xmonad-contrib
      xmonad-extras
      apply-refact
      haskell-language-server
      brittany
      cabal-install
      hlint
      xmobar
    ]
  );

in
{
  home.packages = with pkgs; [
    haskell-env
    trash-cli
    tree
    bc
    gitAndTools.diff-so-fancy
    gitAndTools.delta
    unstable.ripgrep
    unstable.fd
    unstable.fzf
    killall
    neofetch
    pfetch
    nodePackages.node2nix
    cabal2nix
    nix-index
    niv
    rnix-lsp
    scrot
    feh
    tty-clock
    appimage-run
    lightlocker
    circleci-cli
    fontforge
    palenight-gtk-theme
    unstable.pop-icon-theme
    unstable.hicolor-icon-theme # fallback gtk theme
    unstable.pandoc
    unstable.nitrogen
    unstable.chafa
    unstable.ueberzug
    # nixpkgs
    nixfmt
    nixpkgs-fmt
    nixpkgs-review
    nix-prefetch-git
    nix-prefetch-github
    pypi2nix
    # rust
    unstable.cargo
    # tui apps
    cava
    unstable.vifm-full
    vifmimg
    htop
    unstable.powertop
    unstable.bottom
    ncpamixer
    unstable.mps-youtube
    unstable.lazygit
    # gui apps
    master.discord
    tdesktop
    pavucontrol
    spotify
    sxiv
    unstable.steam
    unstable.lutris
    unstable.pulseeffects-legacy
    # node packages
    nodePackages.gitmoji-cli
    # dependencies
    unstable.universal-ctags
    libnotify
    xsel # used by xmonad emoji prompt
    unstable.youtube-dl
    unstable.neovim-remote
    unstable.direnv
    unstable.imagemagick
    unstable.xdotool
    unstable.devour
    unstable.xorg.xwininfo
    unstable.zstd
    unstable.editorconfig-core-c
    unstable.sqlite
    # misc
    glxinfo
    xclip
    file
    catimg
    steam-run
    # joke command
    cmatrix
    figlet
    fortune
    asciiquarium
  ];

  services.lorri.enable = true;
  services.keynav.enable = true;

  # programs {{{
  programs = {
    home-manager.enable = true;
    neovim = (import ../../modules/editors/neovim-stable.nix) { inherit pkgs; };
  };
  #}}}

  home.keyboard = {
    layout = "us";
    variant = "dvorak";
  };
  xsession.profileExtra = "xrandr --output LVDS1 --scale-from 1920x1080 --panning 1920x1080";

  # home files{{{
  home.file = {

    # xmobar{{{
    ".xmonad/xmobar.hs".source = ../../haskell/xmobarlaptop.hs;
    ".xmonad/xmobar.hs".onChange = ''
      if [[ -v DISPLAY ]] ; then
          echo "Recompiling xmobar"
          xmobar -r ~/.xmonad/xmobar.hs &
          sleep 2
          disown
          killall xmobar
          echo "Restarting"
          xmonad --restart
      fi
    ''; # }}}

    # tmux theme
    ".palenight-tmux".source = ../../.palenight-tmux;

    # neovim dashboard logo
    "Pictures/neovimlogo.png".source = (pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/neovim/neovim.github.io/master/logos/neovim-logo-flat.png";
      sha256 = "1vl7mi87wisqhd9zigg24y2ksjyyjk6225bvw9qk4s6gfv62w4jm";
    });

    # emacs-anywhere
    ".emacs_anywhere".source = (pkgs.fetchFromGitHub {
      owner = "btwiusegentoo";
      repo = "emacs-anywhere";
      rev = "e739c8bf1930eeb68ecaf312a885f5788942154c";
      sha256 = "0k0iay6pd01k2nyjm2n648n8ja36mgarvb9677ky2d758xkzgckv";
    });

    # doom emacs
    # ".emacs.d/init.el".text = ''
    #     (load "default.el")
    # '';
    ".doom.d/init.el".source = ../../doom.d/init.el;
    ".doom.d/config.el".source = ../../doom.d/config.el;
    ".doom.d/packages.el".source = ../../doom.d/packages.el;

    ".ghc/ghci.conf".text = ''
      :set prompt "%s 𝛌: "
    '';

  };
  #}}}

  # xsession{{{
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata Ice";
      size = 16;
    };

  }; #}}}

  # xresources config{{{
  xresources.properties = {
    "Xft.dpi" = 96;
  };
  # }}}

  # Home Manager config{{{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "x230";
  home.homeDirectory = "/home/x230";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";

  #}}}

  # local env variables{{{
  home.sessionVariables = {
    "XDG_CONFIG_HOME" = "$HOME/.config";
    "MANPAGER" = "sh -c 'col -bx | bat -l man -p'";
    "COLORTERM" = "truecolor";
    "EDITOR" = "emacsclient -c";
  }; # }}}

}
