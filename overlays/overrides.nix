{ pkgs }:

let
    nixpkgs-unstable =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "7ad5e816faba3f808e2a8815b14d0f023a4e2160";
            sha256 = "1vqqpcnybwl10h5br5nm8n0l33qfzj3rxhahf3pfjfvrc8iyk762";
        };

    nixpkgsmaster =
        pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            rev = "c127653b72574199463a73a56e1809223eaec0df";
            sha256 = "0wld8ig9xdyqhgv6ck4739jgb55pxkw2m4b12qly5ly2fpy9fj9j";
        };

    nurpin =
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
        # overlays =  [
        #             (import ./.nix)
        #             ];
    };
    master = import nixpkgsmaster {
        config = import ../configs/nixpkgs-config.nix;
    };
    nur = import nurpin {
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
}
