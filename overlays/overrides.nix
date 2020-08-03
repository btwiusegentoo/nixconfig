{ pkgs }:

let
    nixpkgs-unstable =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "3541e8b4fbf01731580b2f26a3de41200213e6f1";
            sha256 = "19l6w4nlczydy50fnfcsd7s86j0z1andi9x0xw7fihjjzs9l9pna";
        };

    qutebrowserworkingrev =
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
    qtbpkgs = import qutebrowserworkingrev {
        config = import ../nixpkgs/config.nix;
    };
    # configuration.nix
    lua = unstable.lua;
    neovim = unstable.neovim.override {
    viAlias = true;
    vimAlias = true;
    };
    neovim-unwrapped = unstable.neovim-unwrapped.overrideAttrs (oldattrs: {
        version = "master";
        src = builtins.fetchGit {
            url = "https://github.com/neovim/neovim.git";
            rev = "54ff1cdd76518d88d5e530afc56a9b8c2b7633be";
        };
        patches = oldattrs.patches ++ [ ../patches/nvim_fix_terminal_colors.patch ];
    });
    nerdfonts = unstable.nerdfonts;
    doas = unstable.doas;
    # home.nix
    kitty = unstable.kitty;
    fish = unstable.fish;
    youtube-dl = unstable.youtube-dl;
    mpv = unstable.mpv;
    fzf = unstable.fzf;
    qutebrowser = qtbpkgs.qutebrowser;
}
