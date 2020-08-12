{ pkgs }:

let
    nixpkgs-unstable =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "8e2b14aceb1d40c7e8b84c03a7c78955359872bb";
            sha256 = "0zzjpd9smr7rxzrdf6raw9kbj42fbvafxb5bz36lcxgv290pgsm8";
        };

    nixpkgsmaster =
        pkgs.fetchFromGitHub {
            owner = "nixos";
            repo = "nixpkgs";
            rev = "776476c4a3441db864103230f396bb90be9dc057";
            sha256 = "1vr0zivph849g8xwcc71fda815shv37a12qdc0ywlm56jrl3b6fq";
        };

    qtpkgsrev =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "c71518e75bf067fb639d44264fdd8cf80f53d75a";
            sha256 = "0hwa79prsqgvfwd3ah54nl0wh73q215z7np4k6y0pd6zr3m17vxs";
        };

in
self: super: with pkgs; {
    # required globally
    unstable = import nixpkgs-unstable {
        config = import ../nixpkgs/config.nix;
    };
    master = import nixpkgsmaster {
        config = import ../nixpkgs/config.nix;
    };
    qtpkgs = import qtpkgsrev {
        config = import ../nixpkgs/config.nix;
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
    picom = unstable.picom.overrideAttrs (oldattrs: {
        version = "unstable-2020-08-04";
        src = fetchFromGitHub {
            owner = "ibhagwan";
            repo = "picom";
            rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
            sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
        };
    });
    neofetch = unstable.neofetch.overrideAttrs (oldattrs: {
        patches = [ ../patches/escapesequence.patch ];
    });
    nerdfonts = unstable.nerdfonts;
    doas = unstable.doas;
    # home.nix
    kitty = unstable.kitty;
    fish = unstable.fish;
    youtube-dl = unstable.youtube-dl;
    mpv = unstable.mpv;
    fzf = unstable.fzf;
    qutebrowser = qtpkgs.qutebrowser;
    qtwebengine = qtpkgs.qtwebengine;
}
