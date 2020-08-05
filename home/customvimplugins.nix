{ pkgs, fetchFromGitHub }:

let
  buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix;
in {

# use nix-prefetch-github/fetchFromGitHub.
# specify rev to use custom branch.

    "nvim-colorizer-lua" = buildVimPlugin {
        name = "nvim-colorizer-lua";
        src = fetchFromGitHub {
            owner = "norcalli";
            repo = "nvim-colorizer.lua";
            rev = "36c610a9717cc9ec426a07c8e6bf3b3abcb139d6";
            sha256 = "0gvqdfkqf6k9q46r0vcc3nqa6w45gsvp8j4kya1bvi24vhifg2p9";
        };
        dependencies = ["lua"];
    };

    "vim-python-pep8-indent" = buildVimPlugin {
        name = "vim-python-pep8-indent";
        src = fetchFromGitHub {
            owner = "Vimjas";
            repo = "vim-python-pep8-indent";
            rev = "60ba5e11a61618c0344e2db190210145083c91f8";
            sha256 = "1blyhkykfnf4pgfq9hn9l8pq0iqdvig9m4zd8qq9aa9rlm8f0kzh";
        };
        dependencies = [];
    };

    "vim-fish" = buildVimPlugin {
        name = "vim-fish";
        src = fetchFromGitHub {
            owner = "blankname";
            repo = "vim-fish";
            rev = "8b20c1768913c7c4610a0b42c3705849c80f9195";
            sha256 = "0sgq3m7xngsjmc2i3x089v3znvl5zxxqsf2ia9hvhadpf63x3fd8";
        };
        dependencies = [];
    };

    "vim-cpp-modern" = buildVimPlugin {
        name = "vim-cpp-modern";
        src = fetchFromGitHub {
            owner = "bfrg";
            repo = "vim-cpp-modern";
            rev = "a0cdbae1e6acbbe67d8501070a99be8fdf12fd71";
            sha256 = "0nmivz9m5d1g3gvx8rwlx8g4ppzvwfi0k07q5ji3gwxcn192a19h";
        };
        dependencies = [];
    };

    "coc-clangd" = buildVimPlugin {
        name = "coc-clangd";
        src = fetchFromGitHub {
            owner = "clangd";
            repo = "coc-clangd";
            rev = "6db291b6eb42dfb5d5755223f7ccdeeb06b2575c";
            sha256 = "1d7rxmi03p2pw5kvq1ja92fkfi09iv60jslhia38jdszs4rpdgn7";
        };
        dependencies = [];
    };

    "coc-discord-neovim" = buildVimPlugin {
        name = "coc-discord-neovim";
        src = fetchFromGitHub {
            owner = "BubbatheVTOG";
            repo = "coc-discord-neovim";
            rev = "3a3d6128c0ce7c17c4bf5346f6e0f7610f83c61c";
            sha256 = "1b675pf7g2ndg4x1asni843542syqhdssrnl8m1qxv9h1ss2bsh5";
        };
        dependencies = [];
    };

    "ghcid" = buildVimPlugin {
        name = "ghcid";
        src = fetchFromGitHub {
            owner = "ndmitchell";
            repo = "ghcid";
            rev = "e54c1ebcec8bf4313ef04a1c5f47ecdbb6d11db3";
            sha256 = "1bs07jjj3pgwdr81w8piph6wz73n0gwj3imbnd2za0jqxbshyzry";
        };
    };

    "indenthaskell" = buildVimPlugin {
        name = "indenthaskell";
        src = fetchFromGitHub {
            owner = "vim-scripts";
            repo = "indenthaskell.vim";
            rev = "17380713774ea4f3ca5da1de455126fa1cce82f7";
            sha256 = "1cs9qkn40fk3c8a9kvbdm3d6izf944awiagpmllkvlb4ci9m6lk7";
        };
    };

    "lightline-onedark" = buildVimPlugin {
        name = "lightline-onedark";
        src = fetchFromGitHub {
            owner = "hallzy";
            repo = "lightline-onedark";
            rev = "b3e9d9c35a347e25fb84c51c73fdb084cec4fb96";
            sha256 = "0xc14cyx1v7rk1w739fq0zrkjma1ykj3sl519mf8vmpzy4fb4als";
        };
    };

    "tmuxline" = buildVimPlugin {
        name = "tmuxline";
        src = fetchFromGitHub {
            owner = "edkolev";
            repo = "tmuxline.vim";
            rev = "7001ab359f2bc699b7000c297a0d9e9a897b70cf";
            sha256 = "13d87zxpdzryal5dkircc0sm88mwwq7f5n4j3jn9f09fmg9siifb";
        };
    };

    "vim-deviconsfork" = buildVimPlugin {
        name = "vim-deviconsfork";
        src = fetchFromGitHub {
            owner = "btwiusegentoo";
            repo = "vim-devicons";
            rev = "4b41518c3481f0354d4bb44a1df90ef35e39e2f0";
            sha256 = "0v6dzhw92hmb5bpvyqj0zbsnljrf1qvf442g3adxk37n40dg8rl5";
        };
    };

    "dashboard-nvim" = buildVimPlugin {
        name = "dashboard-nvim";
        src = fetchFromGitHub {
            owner = "hardcoreplayers";
            repo = "dashboard-nvim";
            rev = "a2cea5fedbb4ca3f17874cf5725bd89ede5f70b0";
            sha256 = "1nr85x06nblr022ld8ji18gs1ixc4d4dmni0z5vbqb0rm62d1wsv";
        };
    };

    "coc-explorer" = buildVimPlugin {
        name = "coc-explorer";
        src = fetchFromGitHub {
            owner =  "weirongxu";
            repo =  "coc-explorer";
            rev =  "40e0b6787ae3d4c0f90dd0ce8374953522d2b11b";
            sha256 =  "1m279l7glmdi50q1iamchzl1ll39pc374aa2l1mk1b3jbrxhyb3s";
        };
    };

    "indent-blankline" = buildVimPlugin {
        name = "indent-blankline";
        src = fetchFromGitHub {
            owner = "lukas-reineke";
            repo = "indent-blankline.nvim";
            rev = "a4c147215e3419d8170c96a2a22d995e89c1ea34";
            sha256 = "1fx7513c118g3ap70h01pn12c404bz2dws45z8544nrpfr1zvm5h";
        };
    };

    "sexy-scroller" = buildVimPlugin {
        name = "sexy-scroller";
        src = fetchFromGitHub {
            owner = "joeytwiddle";
            repo = "sexy_scroller.vim";
            rev = "0edd9935c36d627ade050ce7c33532e1b455e486";
            sha256 = "1qr8dvdpzlcva8nmk9ci4063y27gkakf7mldam4dl8m78xh7jdjz";
        };
    };

    "lazygit-nvim" = buildVimPlugin {
        name = "lazygit-nvim";
        src = fetchFromGitHub {
            owner = "kdheepak";
            repo = "lazygit.nvim";
            rev = "5b1e22d967ace8caa074c8edd6ce16b02d9bfb95";
            sha256 = "1vjv8v3prmr59arms59md7i4yg8c0a0hwlknk61sml3925dqndar";
        };
    };
}

# vim: foldmethod=marker shiftwidth=4:
