{ pkgs, unstable, master }:

self: super: with pkgs; {
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
    fish = unstable.fish;
    youtube-dl = unstable.youtube-dl;
    mpv = unstable.mpv;
    fzf = unstable.fzf;
}
