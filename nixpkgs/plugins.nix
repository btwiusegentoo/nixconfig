{ pkgs, fetchgit }:

let
  buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix;
in {

# use nix-prefetch-git to get rev/sha256.

    "material-vim" = buildVimPlugin {
        name = "material-vim";
        src = fetchgit {
            "url" = "https://github.com/kaicataldo/material.vim";
            "rev" = "ec9c95a4985da74930fafc2e52a758015e676e01";
            "sha256" = "0y9mvdmcalprpqjvn0m0d9msnsrnzggg33n0p64pri0lgk94h87v";
        };
        dependencies = [];
    };

    "nvim-colorizer-lua" = buildVimPlugin {
        name = "nvim-colorizer-lua";
        src = fetchgit {
            "url" = "https://github.com/norcalli/nvim-colorizer.lua";
            "rev" = "36c610a9717cc9ec426a07c8e6bf3b3abcb139d6";
            "sha256" = "0gvqdfkqf6k9q46r0vcc3nqa6w45gsvp8j4kya1bvi24vhifg2p9";
        };
        dependencies = ["lua"];
    };

    "vim-python-pep8-indent" = buildVimPlugin {
        name = "vim-python-pep8-indent";
        src = fetchgit {
            "url" = "https://github.com/Vimjas/vim-python-pep8-indent";
            "rev" = "60ba5e11a61618c0344e2db190210145083c91f8";
            "sha256" = "1blyhkykfnf4pgfq9hn9l8pq0iqdvig9m4zd8qq9aa9rlm8f0kzh";
        };
        dependencies = [];
    };

    "vim-fish" = buildVimPlugin {
        name = "vim-fish";
        src = fetchgit {
            "url" = "https://github.com/blankname/vim-fish";
            "rev" = "8b20c1768913c7c4610a0b42c3705849c80f9195";
            "sha256" = "0sgq3m7xngsjmc2i3x089v3znvl5zxxqsf2ia9hvhadpf63x3fd8";
        };
        dependencies = [];
    };

    "vim-cpp-modern" = buildVimPlugin {
        name = "vim-cpp-modern";
        src = fetchgit {
            "url" = "https://github.com/bfrg/vim-cpp-modern";
            "rev" = "a0cdbae1e6acbbe67d8501070a99be8fdf12fd71";
            "sha256" = "0nmivz9m5d1g3gvx8rwlx8g4ppzvwfi0k07q5ji3gwxcn192a19h";
        };
        dependencies = [];
    };

    "coc-clangd" = buildVimPlugin {
        name = "coc-clangd";
        src = fetchgit {
            "url" = "https://github.com/clangd/coc-clangd";
            "rev" = "3876334c671cf56a77b53451f14fbff7819e94b8";
            "sha256" = "0sq1d68gsmrmps5abn54hs62a432d2ampi97wgn6kc4pjh0pxmxw";
        };
        dependencies = [];
    };

    "coc-discord-neovim" = buildVimPlugin {
        name = "coc-discord-neovim";
        src = fetchgit {
            "url" = "https://github.com/BubbatheVTOG/coc-discord-neovim";
            "rev" = "f081407e18b3458c1529eba7a8ae7ec82647fe8c";
            "sha256" = "17qpqf0rlq6q1070wdh5cm7sv90g4pj19jffxldq20gmg20y691y";
        };
        dependencies = [];
    };

}

# vim: foldmethod=marker shiftwidth=4:
