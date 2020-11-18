# Don't forget to setup swapfile and use cachix just in case something have to compile.
# sometime haskell package starts compiling

{ config, pkgs, lib, ... }:

let

    # Haskell packages{{{
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
    # }}}

in
{

    imports = [
        # import xmonad nix expression
        ../../modules/common/xmonad.nix
    ];

    # Packages to install{{{
    home.packages = with pkgs; [
        trash-cli
        tree
        bc
        gitAndTools.diff-so-fancy
        unstable.ripgrep
        unstable.fd
        unstable.fzf
        unstable.mosh
        binutils
        killall
        neofetch
        pfetch
        nodePackages.node2nix
        cabal2nix
        nix-index
        niv
        haskell-env
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
        # lua
        unstable.luajit
        unstable.luajitPackages.lua-lsp
        # tui apps
        cava
        unstable.vifm-full
        vifmimg
        htop
        unstable.bottom
        ncpamixer
        unstable.mps-youtube
        unstable.lazygit
        # gui apps
        gimp
        krita
        kdenlive
        olive-editor
        blender
        master.discord
        tdesktop
        pavucontrol
        spotify
        sxiv
        unstable.retroarch
        unstable.steam
        unstable.virt-manager
        unstable.pulseeffects
        # node packages
        nodePackages.gitmoji-cli
        # dependencies
        ffmpeg-full
        unstable.ffmpegthumbnailer
        frei0r
        unstable.universal-ctags
        libnotify
        xsel # used by xmonad emoji prompt
        unstable.youtube-dl
        unstable.neovim-remote
        unstable.direnv
        unstable.imagemagick
        unstable.zstd
        unstable.editorconfig-core-c
        unstable.sqlite
        # misc
        glxinfo
        xclip
        qt5.qttools
        xkb-switch
        unstable.libinput-gestures
        unstable.xdotool
        unstable.xorg.xwininfo
        unstable.xfontsel
        unstable.xorg.libXext
        unstable.steam-run
        xorg.xev
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

        picom = (import ../../modules/services/picom.nix) { inherit pkgs; };
        dunst = (import ../../modules/services/dunst.nix);
        emacs = (import ../../modules/services/emacsdaemon.nix);
        gpg-agent = (import ../../modules/services/gpg-agent.nix);
        keynav.enable = true;
        lorri.enable = true;

    };
    #}}}

    # programs {{{
    programs = {

        home-manager.enable = true;

        neovim = (import ../../modules/editors/neovim.nix) { inherit pkgs; };
        emacs = {
            enable = true;
            package = pkgs.emacsGccPgtk;
            extraPackages = (epkgs: [ epkgs.vterm ]);
        };
        alacritty = (import ../../modules/terminal/alacritty.nix);
        git = (import ../../modules/terminal/gitemacs.nix) { inherit pkgs; };
        fish = (import ../../modules/terminal/fish.nix) { inherit pkgs; };
        tmux = (import ../../modules/terminal/tmux.nix) { inherit pkgs; };
        bat = (import ../../modules/terminal/bat.nix) { inherit pkgs; };
        starship = (import ../../modules/terminal/starship.nix) { inherit pkgs; };
        lsd = (import ../../modules/terminal/lsd.nix);
        fzf = (import ../../modules/terminal/fzf.nix);
        gpg.enable = true;

        mpv = (import ../../modules/gui/mpv.nix);
        qutebrowser = (import ../../modules/gui/qutebrowseremacs.nix);
        firefox = (import ../../modules/gui/firefox.nix) { inherit pkgs; };
        zathura = (import ../../modules/gui/zathura.nix);
    };
    #}}}

    # Set your keyboard layout. I recommend Dvorak.
    home.keyboard = {
        layout = "us";
        variant = "dvorak";
    };
    #Scale to 1080p
    xsession.profileExtra = "xrandr --output DVI-D-0 --scale-from 2560x1440 --panning 2560x1440";

    # home files{{{
    home.file = {

        # xmobar{{{
        ".xmonad/xmobar.hs".source = ../../haskell/xmobar.hs;
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

        # neofetch ascii
        "textfiles/neofetchascii.txt".source = ../../textfiles/neofetchascii.txt;

        # neovim dashboard logo
        "Pictures/neovimlogo.png".source = (pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/neovim/neovim.github.io/master/logos/neovim-logo-flat.png";
            sha256 = "1vl7mi87wisqhd9zigg24y2ksjyyjk6225bvw9qk4s6gfv62w4jm";
        });

        # doom emacs
        # ".emacs.d/init.el".text = ''
        #     (load "default.el")
        # '';
        ".doom.d/init.el".source = ../../doom.d/init.el;
        ".doom.d/config.el".source = ../../doom.d/config.el;
        ".doom.d/packages.el".source = ../../doom.d/packages.el;

        ".ghc/ghci.conf".text = ''
            :set prompt "%s λ: "
        '';

    };
    #}}}

    # xsession{{{
    xsession = {
        enable = true;
        scriptPath = ".hm-xsession";

        pointerCursor = {
            package = pkgs.bibata-cursors;
            name = "bibata-cursors";
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
    home.username = "btw";
    home.homeDirectory = "/home/btw";

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

    xdg = import ../../modules/common/xdg.nix { inherit pkgs; };

}
    # vim:ft=nix fdm=marker sw=4:
