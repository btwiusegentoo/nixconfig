# Don't forget to setup swapfile and use cachix just in case something have to compile.
# sometime haskell package starts compiling

{ config, pkgs, lib, ... }:

let
    unstable = pkgs.unstable;
    master = pkgs.master;

    # Haskell packages{{{
    haskell-env = unstable.haskellPackages.ghcWithPackages (
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
    # }}}

    # doom emacs
    doom-emacs = unstable.callPackage (builtins.fetchTarball {
        url = "https://github.com/vlaci/nix-doom-emacs/archive/develop.tar.gz";
    }) {
        doomPrivateDir = ../../doom.d;
    };

    # import variables
    username = (import ../../uservars.nix).username;

in
{

    # Packages to install{{{
    home.packages = with pkgs; [
        trash-cli
        unstable.fd
        gitAndTools.diff-so-fancy
        ripgrep
        unstable.fzf
        binutils
        killall
        neofetch
        nodePackages.node2nix
        cabal2nix
        unstable.mosh
        nix-index
        niv
        haskell-env
        rnix-lsp
        scrot
        feh
        lightlocker
        circleci-cli
        fontforge
        palenight-gtk-theme
        unstable.pandoc
        unstable.chafa
        # nixpkgs
        nixfmt
        nixpkgs-fmt
        nixpkgs-review
        nix-prefetch-git
        nix-prefetch-github
        pypi2nix
        # rust
        unstable.cargo
        # lua
        unstable.luajit
        unstable.luajitPackages.lua-lsp
        # tui apps
        cava
        unstable.vifm-full
        vifmimg
        htop
        unstable.ytop
        ncpamixer
        unstable.mps-youtube
        unstable.lazygit
        # node packages
        nodePackages.gitmoji-cli
        # dependencies
        libnotify
        xsel # used by xmonad emoji prompt
        unstable.youtube-dl
        unstable.neovim-remote
        unstable.direnv
        unstable.imagemagick
        # misc
        file
        catimg
        # joke command
        cowsay
        cmatrix
        espeak
        figlet
        fortune
        asciiquarium
    ];
    #}}}

    # services {{{
    services = {

        lorri.enable = true;

    };
    #}}}

    # programs {{{
    programs = {

        home-manager.enable = true;

        neovim = (import ../../modules/editors/neovim-stable.nix) { inherit pkgs; }; # don't compile

        git = (import ../../modules/terminal/git.nix) { inherit pkgs; };
        fish = (import ../../modules/terminal/fish.nix) { inherit pkgs; };
        tmux = (import ../../modules/terminal/tmux.nix) { inherit pkgs; };
        bat = (import ../../modules/terminal/bat.nix) { inherit pkgs; };
        starship = (import ../../modules/terminal/starship.nix) { inherit pkgs; };
        lsd = (import ../../modules/terminal/lsd.nix);
        fzf = (import ../../modules/terminal/fzf.nix);
    };
    #}}}

    # home files{{{
    home.file = {

        # tmux theme
        ".palenight-tmux".source = ../../.palenight-tmux;

        # emojis
        "textfiles/UnicodeData.txt".source = ../../textfiles/UnicodeData.txt;

        # neofetch ascii
        "textfiles/neofetchascii.txt".source = ../../textfiles/neofetchascii.txt;

        # neovim dashboard logo
        "Pictures/neovimlogo.png".source = (pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/neovim/neovim.github.io/master/logos/neovim-logo-flat.png";
            sha256 = "1vl7mi87wisqhd9zigg24y2ksjyyjk6225bvw9qk4s6gfv62w4jm";
        });

        ".ghc/ghci.conf".text = ''
            :set prompt "%s λ: "
        '';

    };
    #}}}

    # .config files{{{
    xdg.configFile = {

        "nvim/coc-settings.json".source = ../../configs/coc-settings.json;
        "neofetch/config.conf".source = ../../configs/neofetch.conf;
        "ncpamixer.conf".source = ../../configs/ncpamixer.conf;

        # nixpkgs{{{
        "nixpkgs/config.nix".text = ''
            { allowUnfree = true; } 
        '';
        # }}}

        "vifm/vifmrc".source = ../../configs/vifmrc.vim;

    };
    # }}}

    xsession.enable = false;


    # Home Manager config{{{
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "${username}";
    home.homeDirectory = "/home/${username}";

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
    }; # }}}

    nixpkgs.config = import ../../configs/nixpkgs-config.nix;

    nixpkgs.overlays = import ../../overlays/all-overlays.nix { inherit pkgs; };

}
# vim:ft=nix fdm=marker sw=4:
