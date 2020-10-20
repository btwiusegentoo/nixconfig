{ pkgs }:

let
    nixpkgs-unstable =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "84d74ae9c9cbed73274b8e4e00be14688ffc93fe";
            sha256 = "0ww70kl08rpcsxb9xdx8m48vz41dpss4hh3vvsmswll35l158x0v";
        };

    nixpkgsmaster =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "b0a8a175e57d45ead90903e6218ec68ae482be91";
            sha256 = "1k7sg15bxplpk2cswzgpwg9jf6z24y5303hmr1r4ww6xvcjnb90h";
        };

    qtpkgsrev =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "c71518e75bf067fb639d44264fdd8cf80f53d75a";
            sha256 = "0hwa79prsqgvfwd3ah54nl0wh73q215z7np4k6y0pd6zr3m17vxs";
        };

    nurrev =
        pkgs.fetchFromGitHub {
            owner = "nix-community";
            repo = "NUR";
            rev = "4f1bda8aee8e3e6fbd161cd5dba86ac9866e3fce";
            sha256 = "0yxms4k7mrlnnf4vi7pqf2hd08dm6wc1ycf5vh0iqhqn7wv3b1s1";
        };

in
self: super: with pkgs; {
    # required globally
    unstable = import nixpkgs-unstable {
        config = import ../configs/nixpkgs-config.nix;
        overlays =  [
                    (import ./emacs.nix)
                    ];
    };
    master = import nixpkgsmaster {
        config = import ../configs/nixpkgs-config.nix;
    };
    qtpkgs = import qtpkgsrev {
        config = import ../configs/nixpkgs-config.nix;
    };
    nur = import nurrev {
        inherit pkgs;
    };
    # configuration.nix
    lua = unstable.lua;
    neovim = unstable.neovim.override {
        viAlias = true;
        vimAlias = true;
    };
    neovim-unwrapped = unstable.neovim-unwrapped.overrideAttrs (oldattrs: {
        ## use neovim nightly
        version = "master";
        src = builtins.fetchGit {
            url = "https://github.com/neovim/neovim";
            rev = "f26df8bb66158baacb79c79822babaf137607cd6";
        };
        patches = oldattrs.patches ++ [ ../patches/nvim_fix_terminal_colors.patch ];
    });
    picomfork = unstable.picom.overrideAttrs (oldattrs: {
        version = "unstable-2020-10-01";
        src = fetchFromGitHub {
            owner = "jonaburg";
            repo = "picom";
            rev = "3ecf9e24441bd16faa39d8ee23471560fbf395b5";
            sha256 = "1r74vlfnv388ynw112bd0nz833i80xjhzkk2hpb66ni67vhgr5hk";
        };
    });
    neofetch = unstable.neofetch.overrideAttrs (oldattrs: {
        patches = [ ../patches/escapesequence.patch ];
    });
    nerdfonts = unstable.nerdfonts;
    doas = unstable.doas;
    alacritty = unstable.alacritty;
    fish = unstable.fish;
    youtube-dl = unstable.youtube-dl;
    mpv = unstable.mpv;
    fzf = unstable.fzf;
    qutebrowser = qtpkgs.qutebrowser;
    qtwebengine = qtpkgs.qtwebengine;
}
