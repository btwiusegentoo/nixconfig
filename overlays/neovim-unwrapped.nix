final: prev:
{
    neovim-unwrapped = prev.neovim-unwrapped.overrideAttrs (old: rec {
        ## use neovim nightly
        version = "master";
        src = builtins.fetchGit {
            url = "https://github.com/neovim/neovim";
            rev = "f26df8bb66158baacb79c79822babaf137607cd6";
        };
        patches = old.patches ++ [ ../patches/nvim_fix_terminal_colors.patch ];
    });
}
