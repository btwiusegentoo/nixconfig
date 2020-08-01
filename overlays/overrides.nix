{ pkgs }:

let
    nixpkgs-unstable =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "b69c61eb39852b11c6d00cf54fc055777c2da35e";
            sha256 = "1mhi84w6w35rbg1vxgm60rj0xy3q06sf41lg3x5dgy049fsc9b9x";
        };

in
self: super: with pkgs; {
    # required globally
    unstable = import nixpkgs-unstable {
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
            url = https://github.com/neovim/neovim.git;
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
}
