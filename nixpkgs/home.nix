# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
# nix-channel --add https://github.com/rycee/home-manager/archive/release-20.03.tar.gz home-manager
# nix-channel --update
# It's better to setup swapfile and use cachix.

{ config, pkgs, lib, ... }:

let
    plugins = pkgs.callPackage ./plugins.nix {};
    unstable = import <nixpkgs-unstable> {};
    # Haskell packages{{{
    haskell-env = unstable.haskellPackages.ghcWithHoogle ( hp: with hp; [
        xmonad
        xmonad-contrib
        xmonad-extras
        apply-refact
        ghcide
        stylish-haskell
        cabal-install
        hlint
        xmobar
    ]);
# }}}

in
{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Packages to install{{{
    home.packages = with pkgs; [
        trash-cli
        tree
        lsd
        bc
        gitAndTools.diff-so-fancy
        fish
        binutils
        killall
        git
        python
        neofetch
        pfetch
        nodePackages.node2nix
        binutils
        glibc
        haskell-env
        rnix-lsp
        scrot
        feh
        tty-clock
        appimage-run
        lightlocker
        # tui apps
        cava
        unstable.nnn
        htop
        ncpamixer
        # gui apps
        qutebrowser
        gimp
        krita
        kdenlive
        blender
        discord
        unstable.tdesktop
        pavucontrol
        mpv
        # dependencies
        ffmpeg-full
        frei0r
        # misc
        glxinfo
        xclip
        qt5.qttools
        xkb-switch
        unstable.libinput-gestures
        unstable.xdotool
        xorg.xev
        file
        catimg
        # joke command
        cowsay cmatrix espeak figlet fortune
    ];
    #}}}

    #keyboard layout{{{
    home.keyboard = {
        layout = "us";
        variant = "dvorak";
    };#}}}

    services = { #{{{
        picom = {
            enable = true;
            fade = true;
            fadeDelta = 5;
            backend = "glx";
        };
    };
    #}}}

    #programs{{{
    programs = {


        #git{{{
        git = {
            enable = true;
            userName = "btwiusegentoo";
            userEmail = "66811008+btwiusegentoo@users.noreply.github.com";
        };
        #}}}

        #fish{{{
        fish = {
            enable = true;

            #alias{{{
            shellAbbrs = {
                #Common commands
                "c" = "clear";
                "s" = "lsd";
                "sa" = "lsd -aF";
                "ss" = "lsd -alF";
                "v" = "nvim";
                "suv" = "sudoedit";
                "diff" = "diff-so-fancy";
                "cat" = "bat";
                "find" = "fd";
                "top" = "htop";
                "untar" = "tar -xvzf";
                "ncpa" = "ncpamixer";
                #Git
                "g" = "git";
                "ga" = "git add";
                "gall" = "git add --all";
                "gc" = "git clone";
                "gco" = "git commit";
                "gp" = "git push";
                "gb" = "git branch";
                "gd" = "git diff";
                "gdst" = "git diff --staged";
                "gst" = "git status";
                "gch" = "git checkout";
                "gf" = "git fetch";
                #neovim is the best
                "code" = "nvim";
                "emacs" = "nvim";
                "vi" = "nvim";
                "vim" = "nvim";
                "atom" = "nvim";
                #config files alias
                "chome" = "nvim ~/mygit/nixconfig/nixpkgs/home.nix";
                "cnix" = "nvim ~/mygit/nixconfig/configuration.nix";
                "cmonad" = "nvim ~/mygit/nixconfig/xmonad/xmonad.hs";
                "cmobar" = "nvim ~/mygit/nixconfig/xmonad/xmobar.hs";
                #screenshot
                "scrotclipsel" = "scrot -s ~/tmp.png && xclip -selection clipboard -t image/png -i ~/tmp.png && rm ~/tmp.png";
                "scrotclip" = "scrot ~/tmp.png && xclip -selection clipboard -t image/png -i ~/tmp.png && rm ~/tmp.png";
                # nnn(filemanager)
                "n" = "nnn -a";
                "nh" = "nnn -a -H";
                "nnnplugins" = "curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh";
                # nixos
                "homesw" = "home-manager switch";
                "nore" = "sudo nixos-rebuild switch";
            };
# }}}

            #plugins{{{
            plugins =
                [
                    {
                        name = "z";
                        src = pkgs.fetchFromGitHub {
                            owner = "jethrokuan";
                            repo = "z";
                            rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
                            sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
                        };
                    }

                    ## It have bug that messes up color of agnoster-fish that I use.
                    ## You can fix by installing once, set theme and uninstall by commenting out this.
                    #{
                        #name = "base16-fish";
                        #src = pkgs.fetchFromGitHub {
                            #owner = "tomyun";
                            #repo = "base16-fish";
                            #rev = "675d53a0dd1aed0fc5927f26a900f5347d446459";
                            #sha256 = "0lp1s9hg682jwzqn1lgj5mrq5alqn9sqw75gjphmiwmciv147kii";
                        #};
                    #}

                    {
                        name = "fish-ssh-agent";
                        src = pkgs.fetchFromGitHub {
                            owner = "danhper";
                            repo = "fish-ssh-agent";
                            rev = "ce90d80aa9549c626f9c5fc5a964536de015a192";
                            sha256 = "03zj5g7dxkhqpp9lijxxlnyx4cc7nqpapj5iqfv7swavyximicyi";
                        };
                    }
                ];
            # }}}

            # config{{{
            interactiveShellInit =
            ''
                fish_vi_key_bindings
                set fish_greeting

                set -U fish_color_autosuggestion 676e95
                set -U fish_color_cancel -r
                set -U fish_color_command green #white
                set -U fish_color_comment 32374D
                set -U fish_color_cwd green
                set -U fish_color_cwd_root red
                set -U fish_color_end brblack #blue
                set -U fish_color_error red
                set -U fish_color_escape yellow #green
                set -U fish_color_history_current --bold
                set -U fish_color_host normal
                set -U fish_color_match --background=brblue
                set -U fish_color_normal normal
                set -U fish_color_operator blue #green
                set -U fish_color_param 8796B0
                set -U fish_color_quote yellow #brblack
                set -U fish_color_redirection cyan
                set -U fish_color_search_match bryellow --background=32374D
                set -U fish_color_selection white --bold --background=32374D
                set -U fish_color_status red
                set -U fish_color_user brgreen
                set -U fish_color_valid_path --underline
                set -U fish_pager_color_completion normal
                set -U fish_pager_color_description yellow --dim
                set -U fish_pager_color_prefix white --bold #--underline
                set -U fish_pager_color_progress brwhite --background=cyan

            '';# }}}

        };
        #}}}

        #neovim{{{ 
        neovim = {
            enable = true;
            viAlias = true;
            vimAlias = true;
            withPython3 = true;

            configure.plug.plugins = with unstable.vimPlugins // plugins; [# {{{
                coc-nvim
                #onedark-vim
                #lightline-onedark
                material-vim #kaicataldo/material.vim
                nvim-colorizer-lua #norcalli/nvim-colorizer.lua
                indentLine
                ale
                vista-vim
                vim-easymotion
                vim-fugitive
                nerdcommenter
                lightline-vim
                lightline-ale
                lightline-bufferline
                nerdtree
                #vim-devicons
                vim-deviconsfork
                nerdtree-git-plugin
                semshi
                vim-python-pep8-indent
                vim-fish
                cosco-vim
                vim-cpp-modern
                vim-nix
                coc-json
                coc-css
                coc-snippets
                coc-clangd
                coc-prettier
                coc-html
                coc-pairs
                indenthaskell
                vim-stylishask
                haskell-vim
                vim-tmux-navigator
                tmuxline
            ]; #}}}

            configure.customRC = '' "{{{
                let g:coc_global_extensions = ['coc-python', 'coc-syntax', 'coc-emoji', 'coc-discord-neovim']
                set modelines=5
                syntax enable
                filetype plugin on
                set nu
                set nornu
                set foldmethod=manual
                set pumblend=10
                set wildmenu
                augroup saveloadfolds
                    autocmd!
                    autocmd BufWinLeave * mkview
                    autocmd BufWinEnter * silent! loadview
                augroup END
                set showtabline=2
                set noshowmode
                set cmdheight=1
                set ignorecase
                set smartcase
                set wrapscan
                set incsearch
                set inccommand=split
                "indent
                set tabstop=4
                set shiftwidth=4
                set softtabstop=0
                set expandtab
                set smarttab
                set shiftround
                set cursorline
                let g:nix_recommended_style=0

                let g:auto_comma_or_semicolon = 1
                let g:material_theme_style = 'palenight'
                let g:material_terminal_italics = 1
                let g:onedark_terminal_italics = 1
                let g:onedark_termcolors = 256
                set background=dark
                colorscheme material
                set termguicolors
                lua require'colorizer'.setup()
                "haskell
                let g:stylishask_on_save = 1
                let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
                let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
                let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
                let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
                let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
                let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
                let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

                " NERDTree
                let NERDTreeShowHidden=1

                "cosco.vim
                let g:cosco_ignore_comment_lines = 1
                let g:cosco_filetype_whitelist = ['php', 'javascript']

                "lightline
                let g:lightline = {
                \ 'colorscheme': 'material_vim',
                \ 'active': {
                \   'left': [ [ 'mode', 'paste' ],
                \             [ 'cocstatus', 'currentfunction', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
                \ },
                \ 'component_function': {
                \   'gitbranch': 'fugitive#head',
                \   'cocstatus': 'coc#status',
                \   'currentfunction': 'CocCurrentFunction',
                \   'filetype': 'MyFiletype',
                \   'fileformat': 'MyFileformat',
                \ },
                \ 'tabline': {
                \   'left': [ ['buffers'] ],
                \   'right': [ ['close'] ]
                \ },
                \ 'component_expand': {
                \   'buffers': 'lightline#bufferline#buffers'
                \ },
                \ 'component_type': {
                \   'buffers': 'tabsel'
                \ },
                \ }

                let g:lightline#bufferline#enable_devicons = 1
                let g:lightline#bufferline#unicode_symbols = 1

                if !has('gui_running')
                    set t_Co=256
                endif

                function! MyFiletype()
                    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ""
                endfunction

                function! MyFileformat()
                    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ""
                endfunction

                "Keybinds
                let mapleader = "\<Space>"
                nnoremap <Leader>w :w<CR>
                "move window with ctrlhjkl
                nnoremap <C-h> <C-w>h
                nnoremap <C-j> <C-w>j
                nnoremap <C-k> <C-w>k
                nnoremap <C-l> <C-w>l
                nmap <silent> <Leader>n :<C-u>Vista!!<CR>
                map <C-n> :NERDTreeToggle<CR>
                "move lines
                nnoremap <A-j> :m .+1<CR>==
                nnoremap <A-k> :m .-2<CR>==
                inoremap <A-j> <Esc>:m .+1<CR>==gi
                inoremap <A-k> <Esc>:m .-2<CR>==gi
                vnoremap <A-j> :m '>+1<CR>gv=gv
                vnoremap <A-k> :m '<-2<CR>gv=gv
                "easymotion
                let g:EasyMotion_do_mapping = 0
                nmap <leader>f <Plug>(easymotion-overwin-f2)
                let g:EasyMotion_smartcase = 1
                map <Leader>j <Plug>(easymotion-j)
                map <Leader>k <Plug>(easymotion-k)
                "copy to clipboard
                vnoremap <leader>y "+y
                nnoremap <leader>Y "+yg_
                nnoremap <leader>y "+y
                nnoremap <leader>yy "+yy
                "paste from clipboard
                nnoremap <leader>p "+p
                nnoremap <leader>P "+P
                vnoremap <leader>p "+p
                vnoremap <leader>P "+P
                "move buffers with Shift JK like qutebrowser
                nnoremap <S-j> :bnext<CR>
                nnoremap <S-k> :bprevious<CR>

                " buffers
                " new buffer
                nnoremap <leader>T :enew<cr>
                " Close the current buffer and move to the previous one
                nnoremap <leader>bq :bp <BAR> bd #<CR>
                " Show all open buffers and their status
                nnoremap <leader>bl :ls<CR>

                "Coc
                set hidden
                set nobackup
                set nowritebackup
                set cmdheight=2
                set updatetime=300
                set shortmess+=c
                set signcolumn=yes
                inoremap <silent><expr> <c-space> coc#refresh()
                inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
                nnoremap <leader><S-f>  :call CocAction('format')<CR>          " leader shift f
                nmap <leader>qf <Plug>(coc-fix-current)
                nmap <silent> [g <Plug>(coc-diagnostic-prev)
                nmap <silent> ]g <Plug>(coc-diagnostic-next)
                nmap <silent> gd <Plug>(coc-definition)
                nmap <silent> gy <Plug>(coc-type-definition)
                nmap <silent> gi <Plug>(coc-implementation)
                nmap <silent> gr <Plug>(coc-references)
                nnoremap <C-K> :call <SID>show_documentation()<CR>
                function! s:show_documentation()
                    if (index(['vim','help'], &filetype) >= 0)
                        execute 'h '.expand('<cword>')
                    else
                        call CocAction('doHover')
                    endif
                endfunction
                autocmd CursorHold * silent call CocActionAsync('highlight')

                augroup mygroup
                    autocmd!
                    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
                    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
                augroup end

                xmap if <Plug>(coc-funcobj-i)
                xmap af <Plug>(coc-funcobj-a)
                omap if <Plug>(coc-funcobj-i)
                omap af <Plug>(coc-funcobj-a)
                nmap <silent> <C-d> <Plug>(coc-range-select)
                xmap <silent> <C-d> <Plug>(coc-range-select)
                command! -nargs=0 Format :call CocAction('format')
                command! -nargs=? Fold :call CocAction('fold', <f-args>)
                command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')
                inoremap <silent><expr> <TAB>
                    \ pumvisible() ? "\<C-n>" :
                    \ <SID>check_back_space() ? "\<TAB>" :
                    \ coc#refresh()
                inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
                function! s:check_back_space() abort
                    let col = col('.') - 1
                    return !col || getline('.')[col -1] =~# '\s'
                endfunction
            '';# }}}

        };
        #}}}

    #kitty{{{
    kitty = {
        enable = true;
        settings = {
            font_size = 9;
            disable_ligatures = "never";    
            window_padding_width = 4;
            background_opacity = "0.9";
            allow_remote_control = "yes";
        };

        font.name = "Monoid Nerd Font Mono";

        extraConfig = ''
        # Palenight Colorscheme{{{
        # https://github.com/citizen428/kitty-palenight
        foreground           #959dcb
        background           #292d3e
        selection_foreground #eceef0
        selection_background #607c8b
        url_color            #82aaff
        # black
        color0   #434759
        color8   #434758
        # red
        color1   #f07178
        color9   #ff8b92
        # green
        color2   #c3e88d
        color10  #ddffa7
        # yellow
        color3   #ffcb6b
        color11  #ffe585
        # blue
        color4  #82aaff
        color12 #9cc4ff
        # magenta
        color5   #c792ea
        color13  #e1acff
        # cyan
        color6   #89ddff
        color14  #a3f7ff
        # white
        color7   #d0d0d0
        color15  #fefefe
        #}}}

        symbol_map U+E000-U+FFFF Monoid Nerd Font
        # symbol_map above Fixes Nerd Font glyph small size issue.

        # keymaps
        map alt+h neighboring_window left 
        map alt+j neighboring_window down
        map alt+k neighboring_window up
        map alt+l neighboring_window right

        map alt+shift+h move_window left 
        map alt+shift+j move_window down
        map alt+shift+k move_window up
        map alt+shift+l move_window right
        '';
    };
    # }}}

    # tmux{{{
    tmux = {
        enable = true;
        plugins = with pkgs; [
            tmuxPlugins.vim-tmux-navigator        
            tmuxPlugins.prefix-highlight
        ];
        keyMode = "vi";
        extraConfig = ''
            set-option -g default-shell /home/btw/.nix-profile/bin/fish
            set-option -g default-terminal "screen-256color"
            set -ga terminal-overrides ",*256col*:Tc"
            set-option -g prefix C-Space
            bind C-Space send-prefix
            bind s split-window -h
            bind v split-window -v
            source-file ./.palenight-tmux
        '';
    };# }}}

    # bat{{{
    bat = {
        enable = true;
        config = {
            theme = "palenight";
            style = "numbers,changes,header";
            italic-text = "always";
        };

        themes = {
            palenight = builtins.readFile (pkgs.fetchgit {
                url = "https://github.com/equinusocio/material-theme";
                rev = "614b7e8bc7369c32e852297d42253643ebf90d55";
                sha256 = "1gjfisksvqa2d08na0yln7yxny4i16wrmvlfnwllbqrgwh26v94g";
            } + "/schemes/Material-Theme-Palenight.tmTheme");
        };
    };

    # }}}

    # mpv{{{
    mpv = {
        enable = true;
        config = {
            volume = 50;
            ytdl-format = "bestvideo+bestaudio";
        };
        bindings = {
            h = "seek -10";
            j = "add volume -2";
            k = "add volume 2";
            l = "seek 10";
            "Ctrl+l" = "ab-loob";
        };
    };
    # }}}

    # qutebrowser{{{
    qutebrowser = {
        enable = true;
        #searchEngines = {
            #DEFAULT = "https://google.com/search?q={}";
        #};

        # settings{{{
        settings = {
            url.start_pages = ["https://btwiusegentoo.github.io/start.html"];
            url.default_page = "https://btwiusegentoo.github.io/start.html";
            scrolling = {
                smooth = true;
                bar = "always";
            };

            fonts = {
                default_family = "SFNS Diplay";
                web.family.fixed = "Monoid Nerd Font Mono";
                completion = {
                    category = "9pt Monoid Nerd Font";
                    entry = "9pt Monoid Nerd Font";
                };
                contextmenu = "9pt Monoid Nerd Font";
                debug_console = "9pt Monoid Nerd Font";
                default_size = "9pt";
                downloads = "9pt Monoid Nerd Font";
                hints = "9pt Monoid Nerd Font";
                keyhint = "9pt Monoid Nerd Font";
                messages = {
                    error = "9pt Monoid Nerd Font";
                    info = "9pt Monoid Nerd Font";
                    warning = "9pt Monoid Nerd Font";
                };
                prompts = "9pt Monoid Nerd Font";
                statusbar = "9pt Monoid Nerd Font";
                #tabs.selected = "9pt Monoid Nerd Font";
                #tabs.unselected = "9pt Monoid Nerd Font";
                tabs = "9pt Monoid Nerd Font";
            };

            # colors{{{
            # base16-qutebrowser (https://github.com/theova/base16-qutebrowser)
            # Base16 qutebrowser template by theova
            # Material Palenight scheme by Nate Peterson
            colors = {
                completion = {
                    fg = "#959DCB";
                    odd.bg = "#292D3E";
                    even.bg = "#292D3E";
                    category = {
                        fg = "#FFCB6B";
                        bg = "#292D3E";
                        border.top = "#292D3E";
                        border.bottom = "#292D3E";
                    };
                    item = {
                        selected.fg = "#444267";
                        selected.bg = "#FFCB6B";
                        selected.border.top = "#FFCB6B";
                        selected.border.bottom = "#FFCB6B";
                        selected.match.fg = "#F07178";
                    };
                    match.fg = "#C3E88D";
                    scrollbar = {
                        fg = "#959DCB";
                        bg = "#292D3E";
                    };
                };
                contextmenu = {
                    menu = {
                        bg = "#292D3E";
                        fg =  "#959DCB";
                    };
                    selected = {
                        bg = "#FFCB6B";
                        fg = "#444267";
                    };
                };
                downloads = {
                    bar.bg = "#292D3E";
                    start = {
                        fg = "#292D3E";
                        bg = "#82AAFF";
                    };
                    stop = {
                        fg = "#292D3E";
                        bg = "#89DDFF";
                    };
                    error.fg = "#F07178";
                };
                hints = {
                    fg = "#292D3E";
                    bg = "#FFCB6B";
                    match.fg = "#959DCB";
                };
                keyhint = {
                    fg = "#959DCB";
                    suffix.fg = "#959DCB";
                    bg = "#292D3E";
                };
                messages = {
                    error = {
                        fg = "#292D3E";
                        bg = "#F07178";
                        border = "#F07178";
                    };
                    warning = {
                        fg = "#292D3E";
                        bg = "#C792EA";
                        border = "#C792EA";
                    };
                    info = {
                        fg = "#959DCB";
                        bg = "#292D3E";
                        border = "#292D3E";
                    };
                };
                prompts = {
                    fg = "#959DCB";
                    border = "#292D3E";
                    bg = "#292D3E";
                    selected.bg = "#FFCB6B";
                };
                statusbar = {
                    normal = {
                        fg = "#C3E88D";
                        bg = "#292D3E";
                    };
                    insert = {
                        fg = "#292D3E";
                        bg = "#82AAFF";
                    };
                    passthrough = {
                        fg = "#292D3E";
                        bg = "#89DDFF";
                    };
                    private = {
                        fg = "#292D3E";
                        bg = "#676E95";
                    };
                    command = {
                        fg = "#959DCB";
                        bg = "#292D3E";
                        private = {
                            fg = "#959DCB";
                            bg = "#292D3E";
                        };
                    };
                    caret = {
                        fg = "#292D3E";
                        bg = "#C792EA";
                        selection = {
                            fg = "#292D3E";
                            bg = "#82AAFF";
                        };
                    };
                    progress.bg = "#82AAFF";
                    url = {
                        fg = "#959DCB";
                        error.fg = "#F07178";
                        hover.fg = "#959DCB";
                        success = {
                            http.fg = "#89DDFF";
                            https.fg = "#C3E88D";
                        };
                        warn.fg = "#C792EA";
                    };
                };
                tabs = {
                    bar.bg = "#292D3E";
                    indicator = {
                        start = "#82AAFF";
                        stop = "#89DDFF";
                        error = "#F07178";
                    };
                    odd = {
                        fg = "#959DCB";
                        bg = "#292D3E";
                    };
                    even = {
                        fg = "#959DCB";
                        bg = "#292D3E";
                    };
                    pinned = {
                        even = {
                            bg = "#292D3E";
                            fg = "#959DCB";
                        };
                        odd = {
                            bg = "#292D3E";
                            fg = "#959DCB";
                        };
                        selected = {
                            even = {
                                bg = "#292D3E";
                                fg = "#959DCB";
                            };
                            odd = {
                                bg = "#292D3E";
                                fg = "#959DCB";
                            };
                        };
                    };
                    selected = {
                        odd = {
                            fg = "#FFFFFF";
                            bg = "#959DCB";
                        };
                        even = {
                            fg = "#FFFFFF";
                            bg = "#959DCB";
                        };
                    };
                };
        };# }}}
    };
    # }}}

    extraConfig = ''
        config.bind('yd', 'spawn mpv {url}')
        config.bind('yf', 'hint links spawn mpv --force-window yes {hint-url}')
        c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}'}
    '';

    };
    # }}}


    };
    #}}}

    # generate dotfiles{{{
    home.file = {

    # xmobar{{{
    ".xmonad/xmobar.hs".source = ../xmonad/xmobar.hs;
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
    '';# }}}

    # tmux theme
    ".palenight-tmux".source = ../.palenight-tmux;

    # scripts{{{
    #  example to make executable
    #".scripts/pymodoro.py".source = pkgs.writeScript "pymodoro.py" (builtins.readFile ( pkgs.fetchurl {
        #url = "https://raw.githubusercontent.com/dattanchu/pymodoro/master/pymodoro/pymodoro.py";
        #sha256 = "076gd0kkc3mn1rkw1hmhxf9iiyl0qz4rs5mjlaqpby3ww14dp1mn";
    #} ) );
    # }}}

    #Coc{{{
    ".config/nvim/coc-settings.json".text = ''
    {

        "html.autoClosingTags": true,
        "html.format.enable": true,
        "html.format.indentInnerHtml": true,

        "suggest.completionItemKindLabels": {
        "function": "\uf794",
        "method": "\uf6a6",
        "variable": "\uf71b",
        "constant": "\uf8ff",
        "struct": "\ufb44",
        "class": "\uf0e8",
        "interface": "\ufa52",
        "text": "\ue612",
        "enum": "\uf435",
        "enumMember": "\uf02b",
        "module": "\uf668",
        "color": "\ue22b",
        "property": "\ufab6",
        "field": "\uf93d",
        "unit": "\uf475",
        "file": "\uf471",
        "value": "\uf8a3",
        "event": "\ufacd",
        "folder": "\uf115",
        "keyword": "\uf893",
        "snippet": "\uf64d",
        "operator": "\uf915",
        "reference": "\uf87a",
        "typeParameter": "\uf278",
        "default": "\uf29c"
        },
        "languageserver": {
            "haskell": {
            "command": "ghcide",
            "args": [
                "--lsp"
            ],
            "rootPatterns": [
                ".stack.yaml",
                ".hie-bios",
                "BUILD.bazel",
                "cabal.config",
                "package.yaml"
            ],
            "filetypes": [
                "hs",
                "lhs",
                "haskell"
            ],
            "settings": {
                "languageServerHaskell": {
                    "hlintOn": true,
                    "maxNumberOfProblems": 10,
                    "completionSnippetsOn": true
                    }
            }
            },
            "nix": {
                "command": "rnix-lsp",
                "filetypes": [
                    "nix"
                ]
            }
        }
    }
    '';
        ##haskell-language-server
        #"languageserver": {
        #"haskell": {
            #"command": "haskell-language-server-wrapper",
            #"args": ["--lsp"],
            #"rootPatterns": [
            #"*.cabal",
            #"stack.yaml",
            #"cabal.project",
            #"package.yaml"
            #],
            #"filetypes": [
            #"hs",
            #"lhs",
            #"haskell"
            #],
            #"initializationOptions": {
            #"languageServerHaskell": {
                    #}
                #}
            #}
        #}

    # }}}

    #neofetch{{{
    ".config/neofetch/config.conf".text = ''
        print_info() {

            prin "$(color 4)────────────────────────────────────────────" 
            info "OS" distro
            info "Uptime" uptime
            info "Packages" packages
            info "Shell" shell
            info "DE" de
            info "Terminal" term
            info "CPU" cpu
            info "Memory" memory
            prin "$(color 4)────────────────────────────────────────────"
            info cols
        }
        title_fqdn="off"
        kernel_shorthand="on"
        os_arch="on"
        uptime_shorthand="tiny"
        memory_percent="off"
        package_managers="on"
        shell_path="off"
        shell_version="on"
        speed_type="bios_limit"
        speed_shorthand="off"
        cpu_brand="on"
        cpu_speed="on"
        cpu_cores="logical"
        cpu_temp="off"
        refresh_rate="off"
        de_version="off"
        colors=(distro)
        bold="on"
        underline_enabled="on"
        underline_char="-"
        separator=" "
        block_range=(0 15)
        color_blocks="on"
        block_width=3
        block_height=1
        col_offset="auto"
        image_backend="ascii"
        image_source="auto"
        ascii_distro="nixos"
        ascii_colors=(4 6)
        ascii_bold="off"
        gap=3
        stdout="off"
    '';
# }}}

    # fontconfig{{{
    ".config/fontconfig/conf.d/10-prefer-emoji.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
            <match>
                <edit name="family" mode="prepend">
                    <string> Apple Color Emoji</string>
                </edit>
            </match>
        </fontconfig>
    '';

    ".config/fontconfig/conf.d/65-nonlatin.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
            <alias>
                <family>serif</family>
                <prefer>
                    <family>Noto Sans CJK JP</family>
                </prefer>
            </alias>
            <alias>
                <family>sans-serif</family>
                <prefer>
                    <family>Noto Sans CJK JP</family>
                </prefer>
            </alias>
            <alias>
                <family>monospace</family>
                <prefer>
                    <family>Noto Sans CJK JP</family>
                </prefer>
            </alias>
        </fontconfig>
    '';# }}}

    # ncpamixer{{{
    ".config/ncpamixer.conf".text = ''
        "theme" = "c0r73x"

        # c0r73x theme {
        "theme.c0r73x.static_bar"             = false
        "theme.c0r73x.default_indicator"      = "■ "
        "theme.c0r73x.bar_style.bg"           = "■"
        "theme.c0r73x.bar_style.fg"           = "■"
        "theme.c0r73x.bar_style.indicator"    = "■"
        "theme.c0r73x.bar_style.top"          = "" 
        "theme.c0r73x.bar_style.bottom"       = "" 
        "theme.c0r73x.bar_low.front"          = 0
        "theme.c0r73x.bar_low.back"           = -1
        "theme.c0r73x.bar_mid.front"          = 0
        "theme.c0r73x.bar_mid.back"           = -1
        "theme.c0r73x.bar_high.front"         = 0
        "theme.c0r73x.bar_high.back"          = -1
        "theme.c0r73x.volume_low"             = 6
        "theme.c0r73x.volume_mid"             = 6
        "theme.c0r73x.volume_high"            = 6
        "theme.c0r73x.volume_peak"            = 1
        "theme.c0r73x.volume_indicator"       = 15
        "theme.c0r73x.selected"               = 6
        "theme.c0r73x.default"                = -1
        "theme.c0r73x.border"                 = -1
        "theme.c0r73x.dropdown.selected_text" = 0
        "theme.c0r73x.dropdown.selected"      = 6
        "theme.c0r73x.dropdown.unselected"    = -1
        # }

        # maybe this uses keycode from here
        # https://blogs.longwin.com.tw/lifetype/key_codes.html
        # something releated to javascript? idk
        # Keybinds {
        "keycode.99"    = "switch"         # tab
        "keycode.13"   = "select"          # enter
        "keycode.27"   = "quit"            # escape
        "keycode.9"   = "dropdown"         # c
        "keycode.113"  = "quit"            # q
        "keycode.109"  = "mute"            # m
        "keycode.100"  = "set_default"     # d
        "keycode.108"  = "volume_up"       # l
        "keycode.104"  = "volume_down"     # h
        "keycode.107"  = "move_up"         # k
        "keycode.106"  = "move_down"       # j
        "keycode.74"   = "tab_next"        # J
        "keycode.75"   = "tab_prev"        # K
        "keycode.265"  = "tab_playback"    # f1
        "keycode.266"  = "tab_recording"   # f2
        "keycode.267"  = "tab_output"      # f3
        "keycode.268"  = "tab_input"       # f4
        "keycode.269"  = "tab_config"      # f5
        "keycode.f.80" = "tab_playback"    # f1 VT100
        "keycode.f.81" = "tab_recording"   # f2 VT100
        "keycode.f.82" = "tab_output"      # f3 VT100
        "keycode.f.83" = "tab_input"       # f4 VT100
        "keycode.f.84" = "tab_config"      # f5 VT100
        "keycode.71"   = "move_last"       # G
        "keycode.103"  = "move_first"      # g
        "keycode.48"   = "set_volume_0"    # 0
        "keycode.49"   = "set_volume_10"   # 1
        "keycode.50"   = "set_volume_20"   # 2
        "keycode.51"   = "set_volume_30"   # 3
        "keycode.52"   = "set_volume_40"   # 4
        "keycode.53"   = "set_volume_50"   # 5
        "keycode.54"   = "set_volume_60"   # 6
        "keycode.55"   = "set_volume_70"   # 7
        "keycode.56"   = "set_volume_80"   # 8
        "keycode.57"   = "set_volume_90"   # 9
        # }
    '';# }}}

    };
    #}}}

    #xsession{{{
    xsession = {
        enable = true;
        scriptPath = ".hm-xsession";
        profileExtra = "xrandr --output DVI-D-0 --scale 1.33333333333333x1.33333333333333 --panning 2560x1440 ";

        #xmonad{{{
        windowManager.xmonad = {
            enable = true;
            enableContribAndExtras = true;
            config = ../xmonad/xmonad.hs;
            haskellPackages =
                unstable.haskell.packages.ghc882;
            extraPackages = haskellPackages: [
                haskellPackages.xmonad-contrib
                haskellPackages.xmonad-extras
                haskellPackages.xmonad
            ];
        };
# }}}

    pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
        size = 16;
    };


    };/*}}}*/

    # xresources config{{{
    xresources.properties = {
    };
# }}}

    #Home Manager config{{{
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

    nixpkgs.config.allowUnfree = true;
    #}}}

    # local env variables{{{
    home.sessionVariables = {
        "XDG_CONFIG_HOME" = "$HOME/.config";
        "MANPAGER" = "sh -c 'col -bx | bat -l man -p'";
        "NNN_PLUG" = "p:preview-tui";
    };# }}}

    nixpkgs.overlays = [ (import ../overlays/packages.nix) ];

    # override modules.{{{
    nixpkgs.config.packageOverrides = pkgs: {
        kitty = unstable.kitty;
    };
# }}}

}
# vim:ft=nix foldmethod=marker shiftwidth=4:
