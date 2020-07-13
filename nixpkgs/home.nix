# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
# nix-channel --add https://github.com/rycee/home-manager/archive/release-20.03.tar.gz home-manager
# nix-channel --update
# It's better to setup swapfile and use cachix.

{ config, pkgs, lib, ... }:

let
    plugins = pkgs.callPackage ./plugins.nix {};
    unstable = import <nixpkgs-unstable> {};
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

in
{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Packages to install{{{
    home.packages = with pkgs; [
        feh
        dmenu
        htop
        ranger
        cava
        xclip
        kitty
        lsd
        bat
        bc
        gitAndTools.diff-so-fancy
        fish
        binutils
        killall
        discord
        unstable.tdesktop
        git
        pavucontrol
        python
        neofetch
        pfetch
        unstable.qutebrowser
        unstable.qt5.qtwebengine
        nodePackages.node2nix
        mpv
        binutils
        glxinfo
        glibc
        haskell-env
        unstable.picom
        unstable.libinput-gestures
        unstable.xdotool
        scrot
        #misc
        cowsay cmatrix espeak figlet
    ];
    #}}}

    #keyboard layout{{{
    home.keyboard = {
        layout = "us";
        variant = "dvorak";
    };#}}}

    #services{{{
    #services.picom = {
        #enable = true;
        #blur = true;
    #};
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
                #Git
                "g" = "git";
                "ga" = "git add";
                "gall" = "git add --all";
                "gc" = "git clone";
                "gco" = "git commit";
                "gp" = "git push";
                "gb" = "git branch";
                "gd" = "git diff";
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
                "scrotclip" = "scrot -s ~/tmp.png && xclip -selection clipboard -t image/png -i ~/tmp.png && rm ~/tmp.png";
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

            interactiveShellInit =
            ''
                fish_vi_key_bindings
                set fish_greeting
            '';

        };
        #}}}

        #neovim{{{ 
        neovim = {
            enable = true;
            viAlias = true;
            vimAlias = true;
            withPython3 = true;

            configure.plug.plugins = with pkgs.vimPlugins // plugins; [# {{{
                coc-nvim
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
                nerdtree
                vim-devicons
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
            ]; #}}}

            configure.customRC = '' "{{{
                let g:coc_global_extensions = ['coc-python', 'coc-syntax', 'coc-emoji', 'coc-discord-neovim']
                set modelines=5
                syntax enable
                filetype plugin on
                set number relativenumber
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
                set tabstop=4
                set shiftwidth=4
                set softtabstop=0
                set expandtab
                set smarttab
                set shiftround
                let g:auto_comma_or_semicolon = 1
                let g:material_theme_style = 'palenight'
                let g:material_terminal_italics = 1
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

                "cosco.vim
                let g:cosco_ignore_comment_lines = 1
                let g:cosco_filetype_whitelist = ['php', 'javascript']

                "lightline
                let g:lightline = {
                \ 'colorscheme': 'material_vim',
                \ 'active': {
                \   'left': [ [ 'mode', 'paste' ],
                \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
                \ },
                \ 'component_function': {
                \   'cocstatus': 'coc#status',
                \   'currentfunction': 'CocCurrentFunction'
                \ },
                \ }

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
                "move tab with Shift JK like qutebrowser
                nnoremap <S-j> gT
                nnoremap <S-k> gt

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
            background_opacity = "1";
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
        cursor #fefefe
        symbol_map U+E000-U+FFFF Monoid Nerd Font
        # symbol_map above Fixes Nerd Font glyph small size issue.
        '';
    };
    # }}}

    };
    #}}}

    # generate dotfiles{{{
    home.file = {

    #".xmonad/xmonad.hs".source = ../xmonad/xmonad.hs;
    ".xmonad/xmobar.hs".source = ../xmonad/xmobar.hs;

    # qutebrowser{{{
    ".config/qutebrowser/config.py".text = 
    ''
        c.url.start_pages = ["https://btwiusegentoo.github.io/start.html"]
        c.url.default_page = "https://btwiusegentoo.github.io/start.html"
        c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}'}
        c.scrolling.smooth = True
        c.scrolling.bar = 'always'

        # Fonts
        c.fonts.default_family="SFNS Diplay"
        c.fonts.web.family.fixed="Monoid Nerd Font Mono"
        c.fonts.completion.category="9pt Monoid Nerd Font Mono"
        c.fonts.completion.entry="9pt Monoid Nerd Font Mono"
        c.fonts.contextmenu="9pt Monoid Nerd Font Mono"
        c.fonts.debug_console="9pt Monoid Nerd Font Mono"
        c.fonts.default_size="9pt"
        c.fonts.downloads="9pt Monoid Nerd Font Mono"
        c.fonts.hints="9pt Monoid Nerd Font Mono"
        c.fonts.keyhint="9pt Monoid Nerd Font Mono"
        c.fonts.messages.error="9pt Monoid Nerd Font Mono"
        c.fonts.messages.info="9pt Monoid Nerd Font Mono"
        c.fonts.messages.warning="9pt Monoid Nerd Font Mono"
        c.fonts.prompts="9pt Monoid Nerd Font Mono"
        c.fonts.statusbar="9pt Monoid Nerd Font Mono"
        c.fonts.tabs.selected="9pt Monoid Nerd Font Mono"
        c.fonts.tabs.unselected="9pt Monoid Nerd Font Mono"

        # mpv youtube
        config.bind('yd', 'spawn mpv {url}')
        config.bind('yf', 'hint links spawn mpv --force-window yes {hint-url}')

        # base16-qutebrowser (https://github.com/theova/base16-qutebrowser)
        # Base16 qutebrowser template by theova
        # Material Palenight scheme by Nate Peterson

        base00 = "#292D3E"
        base01 = "#444267"
        base02 = "#32374D"
        base03 = "#676E95"
        base04 = "#8796B0"
        base05 = "#959DCB"
        base06 = "#959DCB"
        base07 = "#FFFFFF"
        base08 = "#F07178"
        base09 = "#F78C6C"
        base0A = "#FFCB6B"
        base0B = "#C3E88D"
        base0C = "#89DDFF"
        base0D = "#82AAFF"
        base0E = "#C792EA"
        base0F = "#FF5370"

        # set qutebrowser colors

        # Text color of the completion widget. May be a single color to use for
        # all columns or a list of three colors, one for each column.
        c.colors.completion.fg = base05

        # Background color of the completion widget for odd rows.
        c.colors.completion.odd.bg = base00

        # Background color of the completion widget for even rows.
        c.colors.completion.even.bg = base00

        # Foreground color of completion widget category headers.
        c.colors.completion.category.fg = base0A

        # Background color of the completion widget category headers.
        c.colors.completion.category.bg = base00

        # Top border color of the completion widget category headers.
        c.colors.completion.category.border.top = base00

        # Bottom border color of the completion widget category headers.
        c.colors.completion.category.border.bottom = base00

        # Foreground color of the selected completion item.
        c.colors.completion.item.selected.fg = base01

        # Background color of the selected completion item.
        c.colors.completion.item.selected.bg = base0A

        # Top border color of the selected completion item.
        c.colors.completion.item.selected.border.top = base0A

        # Bottom border color of the selected completion item.
        c.colors.completion.item.selected.border.bottom = base0A

        # Foreground color of the matched text in the selected completion item.
        c.colors.completion.item.selected.match.fg = base08

        # Foreground color of the matched text in the completion.
        c.colors.completion.match.fg = base0B

        # Color of the scrollbar handle in the completion view.
        c.colors.completion.scrollbar.fg = base05

        # Color of the scrollbar in the completion view.
        c.colors.completion.scrollbar.bg = base00

        # Background color of the context menu. If set to null, the Qt default is used.
        c.colors.contextmenu.menu.bg = base00

        # Foreground color of the context menu. If set to null, the Qt default is used.
        c.colors.contextmenu.menu.fg =  base05

        # Background color of the context menu’s selected item. If set to null, the Qt default is used.
        c.colors.contextmenu.selected.bg = base0A

        #Foreground color of the context menu’s selected item. If set to null, the Qt default is used.
        c.colors.contextmenu.selected.fg = base01

        # Background color for the download bar.
        c.colors.downloads.bar.bg = base00

        # Color gradient start for download text.
        c.colors.downloads.start.fg = base00

        # Color gradient start for download backgrounds.
        c.colors.downloads.start.bg = base0D

        # Color gradient end for download text.
        c.colors.downloads.stop.fg = base00

        # Color gradient stop for download backgrounds.
        c.colors.downloads.stop.bg = base0C

        # Foreground color for downloads with errors.
        c.colors.downloads.error.fg = base08

        # Font color for hints.
        c.colors.hints.fg = base00

        # Background color for hints. Note that you can use a `rgba(...)` value
        # for transparency.
        c.colors.hints.bg = base0A

        # Font color for the matched part of hints.
        c.colors.hints.match.fg = base05

        # Text color for the keyhint widget.
        c.colors.keyhint.fg = base05

        # Highlight color for keys to complete the current keychain.
        c.colors.keyhint.suffix.fg = base05

        # Background color of the keyhint widget.
        c.colors.keyhint.bg = base00

        # Foreground color of an error message.
        c.colors.messages.error.fg = base00

        # Background color of an error message.
        c.colors.messages.error.bg = base08

        # Border color of an error message.
        c.colors.messages.error.border = base08

        # Foreground color of a warning message.
        c.colors.messages.warning.fg = base00

        # Background color of a warning message.
        c.colors.messages.warning.bg = base0E

        # Border color of a warning message.
        c.colors.messages.warning.border = base0E

        # Foreground color of an info message.
        c.colors.messages.info.fg = base05

        # Background color of an info message.
        c.colors.messages.info.bg = base00

        # Border color of an info message.
        c.colors.messages.info.border = base00

        # Foreground color for prompts.
        c.colors.prompts.fg = base05

        # Border used around UI elements in prompts.
        c.colors.prompts.border = base00

        # Background color for prompts.
        c.colors.prompts.bg = base00

        # Background color for the selected item in filename prompts.
        c.colors.prompts.selected.bg = base0A

        # Foreground color of the statusbar.
        c.colors.statusbar.normal.fg = base0B

        # Background color of the statusbar.
        c.colors.statusbar.normal.bg = base00

        # Foreground color of the statusbar in insert mode.
        c.colors.statusbar.insert.fg = base00

        # Background color of the statusbar in insert mode.
        c.colors.statusbar.insert.bg = base0D

        # Foreground color of the statusbar in passthrough mode.
        c.colors.statusbar.passthrough.fg = base00

        # Background color of the statusbar in passthrough mode.
        c.colors.statusbar.passthrough.bg = base0C

        # Foreground color of the statusbar in private browsing mode.
        c.colors.statusbar.private.fg = base00

        # Background color of the statusbar in private browsing mode.
        c.colors.statusbar.private.bg = base03

        # Foreground color of the statusbar in command mode.
        c.colors.statusbar.command.fg = base05

        # Background color of the statusbar in command mode.
        c.colors.statusbar.command.bg = base00

        # Foreground color of the statusbar in private browsing + command mode.
        c.colors.statusbar.command.private.fg = base05

        # Background color of the statusbar in private browsing + command mode.
        c.colors.statusbar.command.private.bg = base00

        # Foreground color of the statusbar in caret mode.
        c.colors.statusbar.caret.fg = base00

        # Background color of the statusbar in caret mode.
        c.colors.statusbar.caret.bg = base0E

        # Foreground color of the statusbar in caret mode with a selection.
        c.colors.statusbar.caret.selection.fg = base00

        # Background color of the statusbar in caret mode with a selection.
        c.colors.statusbar.caret.selection.bg = base0D

        # Background color of the progress bar.
        c.colors.statusbar.progress.bg = base0D

        # Default foreground color of the URL in the statusbar.
        c.colors.statusbar.url.fg = base05

        # Foreground color of the URL in the statusbar on error.
        c.colors.statusbar.url.error.fg = base08

        # Foreground color of the URL in the statusbar for hovered links.
        c.colors.statusbar.url.hover.fg = base05

        # Foreground color of the URL in the statusbar on successful load
        # (http).
        c.colors.statusbar.url.success.http.fg = base0C

        # Foreground color of the URL in the statusbar on successful load
        # (https).
        c.colors.statusbar.url.success.https.fg = base0B

        # Foreground color of the URL in the statusbar when there's a warning.
        c.colors.statusbar.url.warn.fg = base0E

        # Background color of the tab bar.
        c.colors.tabs.bar.bg = base00

        # Color gradient start for the tab indicator.
        c.colors.tabs.indicator.start = base0D

        # Color gradient end for the tab indicator.
        c.colors.tabs.indicator.stop = base0C

        # Color for the tab indicator on errors.
        c.colors.tabs.indicator.error = base08

        # Foreground color of unselected odd tabs.
        c.colors.tabs.odd.fg = base05

        # Background color of unselected odd tabs.
        c.colors.tabs.odd.bg = base00

        # Foreground color of unselected even tabs.
        c.colors.tabs.even.fg = base05

        # Background color of unselected even tabs.
        c.colors.tabs.even.bg = base00

        # Background color of pinned unselected even tabs.
        c.colors.tabs.pinned.even.bg = base00

        # Foreground color of pinned unselected even tabs.
        c.colors.tabs.pinned.even.fg = base05

        # Background color of pinned unselected odd tabs.
        c.colors.tabs.pinned.odd.bg = base00

        # Foreground color of pinned unselected odd tabs.
        c.colors.tabs.pinned.odd.fg = base05

        # Background color of pinned selected even tabs.
        c.colors.tabs.pinned.selected.even.bg = base00

        # Foreground color of pinned selected even tabs.
        c.colors.tabs.pinned.selected.even.fg = base05

        # Background color of pinned selected odd tabs.
        c.colors.tabs.pinned.selected.odd.bg = base00

        # Foreground color of pinned selected odd tabs.
        c.colors.tabs.pinned.selected.odd.fg = base05

        # Foreground color of selected odd tabs.
        c.colors.tabs.selected.odd.fg = base07

        # Background color of selected odd tabs.
        c.colors.tabs.selected.odd.bg = base06

        # Foreground color of selected even tabs.
        c.colors.tabs.selected.even.fg = base07

        # Background color of selected even tabs.
        c.colors.tabs.selected.even.bg = base06

        # Background color for webpages if unset (or empty to use the theme's
        # color).
        # c.colors.webpage.bg = base00

    ''; /*}}}*/

    #mpv{{{
    ".config/mpv/input.conf".text = ''
        h seek -10
        j add volume -2
        k add volume 2
        l seek 10
        Ctrl+l ab-loop
    '';

    ".config/mpv/mpv.conf".text = ''
        volume=50
    '';
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

            prin "$(color 12)────────────────────────────────────────────" 
            info "OS" distro
            info "Uptime" uptime
            info "Packages" packages
            info "Shell" shell
            info "DE" de
            info "Terminal" term
            info "CPU" cpu
            info "Memory" memory
            prin "$(color 12)────────────────────────────────────────────"
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
        colors=(12 14)
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
        ascii_colors=(distro)
        ascii_bold="off"
        gap=3
        stdout="off"
    '';
# }}}

    #ranger{{{
    ".config/ranger/rc.conf".text = ''
        set show_hidden true
        set confirm_on_delete always
        set preview_images true
        set preview_images_method kitty
        set wrap_plaintext_previews true
        set draw_borders both
        set dirname_in_tabs true
        default_linemode devicons
    '';
    # }}}

    #ranger plugins{{{
    ".config/ranger/plugins/ranger_devicons".source = pkgs.fetchFromGitHub {
        owner = "alexanderjeurissen";
        repo = "ranger_devicons";
        rev = "86a0810e744d33278d5dd74f57336c066a806c9d";
        sha256 = "0vhnvmcv5qq8hnxpzvq47cls0pax84ar9rzw9pxzliabfs6fy3g2";
    };
    # }}}

    # Give highest priority to apple emoji {{{
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
# }}}


    };
    #}}}

    #xsession{{{
    xsession = {
        enable = true;
        scriptPath = ".hm-xsession";
        profileExtra = "xrandr --output DVI-D-1 --scale 1.33333333333333x1.33333333333333 --panning 2560x1440 ";

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

        #i3wm{{{
        windowManager.i3 = let mod = "Mod4"; in {
            enable = false;

            config = {
                keybindings = {/*{{{*/
                    "${mod}+Return" = "exec kitty";
                    "${mod}+d" = "exec dmenu_run";
                    "${mod}+q" = "kill";
                    "${mod}+h" = "focus left";
                    "${mod}+j" = "focus down";
                    "${mod}+k" = "focus up";
                    "${mod}+l" = "focus right";
                    "${mod}+Shift+h" = "move left";
                    "${mod}+Shift+j" = "move down";
                    "${mod}+Shift+k" = "move up";
                    "${mod}+Shift+l" = "move right";
                    "${mod}+b" = "split h";
                    "${mod}+v" = "split v";
                    "${mod}+f" = "fullscreen";
                    "${mod}+Shift+space" = "floating toggle";
                    "${mod}+space" = "focus mode_toggle";
                    "${mod}+1" = "workspace 1";
                    "${mod}+2" = "workspace 2";
                    "${mod}+3" = "workspace 3";
                    "${mod}+4" = "workspace 4";
                    "${mod}+5" = "workspace 5";
                    "${mod}+6" = "workspace 6";
                    "${mod}+7" = "workspace 7";
                    "${mod}+8" = "workspace 8";
                    "${mod}+9" = "workspace 9";
                    "${mod}+10" = "workspace 10";
                    "${mod}+Shift+1" = "move container to workspace 1";
                    "${mod}+Shift+2" = "move container to workspace 2";
                    "${mod}+Shift+3" = "move container to workspace 3";
                    "${mod}+Shift+4" = "move container to workspace 4";
                    "${mod}+Shift+5" = "move container to workspace 5";
                    "${mod}+Shift+6" = "move container to workspace 6";
                    "${mod}+Shift+7" = "move container to workspace 7";
                    "${mod}+Shift+8" = "move container to workspace 8";
                    "${mod}+Shift+9" = "move container to workspace 9";
                    "${mod}+Shift+10" = "move container to workspace 10";
                    "${mod}+Shift+r" = "restart";
                    "${mod}+Shift+e" = "exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'\"";
                };/*}}}*/

                gaps = {
                    inner = 15;
                    outer = 6;
                };
            };

            extraConfig = ''
                default_border pixel 4
                exec xrandr --output DVI-D-1 --scale-from 2560x1440
                '';
        };
# }}}

    pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
        size = 16;
    };


    };/*}}}*/

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

    nixpkgs.overlays = [ (import ../overlays/packages.nix) ];

}
# vim:ft=nix foldmethod=marker shiftwidth=4:
