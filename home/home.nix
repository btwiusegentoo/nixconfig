# Don't forget to setup swapfile and use cachix just in case something have to compile.
# sometime haskell package starts compiling

{ config, pkgs, lib, ... }:

let
    unstable = pkgs.unstable;

    plugins = pkgs.callPackage ./customvimplugins.nix {};

    # Haskell packages{{{
    haskell-env = unstable.haskell.packages.ghc884.ghcWithPackages (
        hp: with hp; [
        xmonad
        xmonad-contrib
        xmonad-extras
        apply-refact
        ghcide
        stylish-haskell
        cabal-install
        hlint
        xmobar
        ]
    );
    # }}}

in
{

  # Packages to install{{{
  home.packages = with pkgs; [
    trash-cli
    tree
    bc
    gitAndTools.diff-so-fancy
    ripgrep
    fd
    fzf
    binutils
    killall
    python
    neofetch
    pfetch
    nodePackages.node2nix
    cabal2nix
    nix-index
    niv
    binutils
    glibc
    haskell-env
    rnix-lsp
    scrot
    feh
    tty-clock
    appimage-run
    lightlocker
    circleci-cli
    fontforge
    # nixpkgs
    nixpkgs-fmt
    nixpkgs-review
    # rust
    unstable.cargo
    # tui apps
    cava
    unstable.nnn
    htop
    unstable.ytop
    ncpamixer
    unstable.mps-youtube
    unstable.lazygit
    # gui apps
    gimp
    krita
    kdenlive
    olive-editor
    blender
    discord
    unstable.tdesktop
    pavucontrol
    spotify
    # node packages
    nodePackages.gitmoji-cli
    # dependencies
    ffmpeg-full
    frei0r
    unstable.universal-ctags
    libnotify
    xsel # used by xmonad emoji prompt
    unstable.youtube-dl
    unstable.neovim-remote
    unstable.direnv
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
    cowsay
    cmatrix
    espeak
    figlet
    fortune
    asciiquarium
  ];
  #}}}

  # keyboard layout{{{
  home.keyboard = {
    layout = "us";
    variant = "dvorak";
  }; #}}}

  # services {{{
    services = {

        # picom{{{
        picom = {
        enable = true;
        fade = true;
        fadeDelta = 3;
        backend = "glx";
        experimentalBackends = true;
        opacityRule = [
            "90:class_g  = 'Zathura'"
            "90:class_g  = 'TelegramDesktop'"
            "90:class_g  = 'Discord'"
            "100:class_g = 'keynav'"
        ];
        extraOptions = ''
            detect-client-opacity = true;
            detect-rounded-corners = true;
            blur:
            {
                method = "kawase";
                strength = 10;
                background = false;
                background-frame = false;
                background-fixed = false;
            };
            corner-radius = 12;
            rounded-corners-exclude = [
                "window_type = 'dock'",
                "_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
                "class_g = 'keynav'",
            ];
            round-borders = 1;
            round-borders-exclude = [
                "class_g = 'keynav'"
            ];
        '';
        };
        # }}}

        # dunst{{{
        dunst = {
        enable = true;
        settings = {
            global = {
            frame_color = "#959DCB";
            separator_color = "#959DCB";
            transparency = 10;
            alignment = "left";
            geometry = "300x5-30+20";
            };
            urgency_low = {
            background = "#444267";
            foreground = "#676E95";
            };
            urgency_normal = {
            background = "#32374D";
            foreground = "#959DCB";
            };
            urgency_critical = {
            background = "#F07178";
            foreground = "#959DCB";
            };
        };
        };
        # }}}

        keynav.enable = true;

        lorri.enable = true;

    };
    #}}}

  # programs {{{
    programs = {

        home-manager.enable = true;

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

        # Abbreviation. similar to alias{{{
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
            "top" = "ytop";
            "untar" = "tar -xvzf";
            #Git
            "g" = "git";
            "ga" = "git add";
            "gall" = "git add --all";
            "gc" = "git clone";
            "gmc" = "gitmoji -c";
            "gco" = "git commit";
            "gp" = "git push";
            "gb" = "git branch";
            "gd" = "git diff";
            "gdst" = "git diff --staged";
            "gst" = "git status";
            "gch" = "git checkout";
            "gf" = "git fetch";
            "gmv" = "git mv";
            "gl" = "git log --graph --color=always";
            "glo" = "git log --graph --color=always --oneline";
            #config files alias
            "chome" = "nvim ~/.nixconfig/home/home.nix";
            "cnix" = "nvim ~/.nixconfig/configuration.nix";
            "cmonad" = "nvim ~/.nixconfig/haskell/xmonad.hs";
            "cmobar" = "nvim ~/.nixconfig/haskell/xmobar.hs";
            "cvim" = "nvim ~/.nixconfig/home/config/nvim.vim";
            #screenshot
            "scrotclipsel" = "scrot -s ~/tmp.png && xclip -selection clipboard -t image/png -i ~/tmp.png && rm ~/tmp.png";
            "scrotclip" = "scrot ~/tmp.png && xclip -selection clipboard -t image/png -i ~/tmp.png && rm ~/tmp.png";
            # nnn(filemanager)
            "n" = "nnn -a";
            "nh" = "nnn -a -H";
            "nnnplugins" = "curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh";
            # nixos
            "nixre" = "doas nixos-rebuild switch";
            "dnixtrash" = "doas nix-collect-garbage -d";
            "nixtrash" = "nix-collect-garbage -d";
            "nsh" = "nix-shell";
            "nfish" = "nix-shell --run fish";
            # frequently used apps
            "ncpa" = "ncpamixer";
            # misc
            "tty-clock" = "tty-clock -C 1 -c";
            "rickroll" = "curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash";
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

            if [ -n "$NVIM_LISTEN_ADDRESS" ];
                alias nvim="nvr -cc split --remote-wait +'set bufhidden=wipe'"
            end

            if [ -n "$NVIM_LISTEN_ADDRESS" ];
                export VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
                export EDITOR="nvr -cc split --remote-wait +'set bufhidden=wipe'"
            else
                export VISUAL="nvim"
                export EDITOR="nvim"
            end

            eval (direnv hook fish)
            ''; # }}}

        };
        #}}}

        #neovim{{{ 
        neovim = {
            enable = true;
            viAlias = true;
            vimAlias = true;
            withPython3 = true;
        configure = {
            # plugins {{{
            plug.plugins = with unstable.vimPlugins // plugins; [
                coc-nvim
                coc-json
                coc-css
                coc-snippets
                coc-clangd
                coc-prettier
                coc-html
                coc-pairs
                coc-tabnine
                palenight-vim
                nvim-colorizer-lua #norcalli/nvim-colorizer.lua
                indentLine
                vista-vim
                vim-sneak
                vim-fugitive
                gv-vim
                #vim-gitgutter
                nerdcommenter
                lightline-vim
                lightline-ale
                lightline-bufferline
                sky-color-clock-vim
                #vim-devicons
                vim-deviconsfork
                semshi
                vim-python-pep8-indent
                vim-fish
                vim-nix
                cosco-vim
                vim-cpp-modern
                indenthaskell
                vim-stylishask
                haskell-vim
                vim-tmux-navigator
                tmuxline
                quick-scope
                gv-vim
                tabular
                dashboard-nvim
                fzf-vim
                fzfWrapper
                vim-which-key
                indent-blankline
                vim-visual-multi
                lazygit-nvim
                vim-orgmode
                rust-vim
                coc-rust-analyzer
            ]; #}}}

            customRC = import ./config/nvim.vim;

        };

        };
        #}}}

        # alacritty{{{
        alacritty = {
        enable = true;
        settings = {
            env = {
            TERM = "xterm-256color";
            };
            window = {
            padding = {
                x = 5;
                y = 5;
            };
            dynamic_padding = false;
            decorations = "none";
            startup_mode = "Windowed";
            };
            scrolling = {
            history = 250;
            multiplier = 1;
            };
            font = {
            normal = {
                family = "Tamzen";
                style = "Regular";
            };
            bold = {
                family = "Tamzen";
                style = "Bold";
            };
            italic = {
                family = "Tamzen";
                style = "Regular";
            };
            size = 12;
            offset = {
                x = 0;
                y = 6;
            };
            glyph_offset = {
                x = 0;
                y = 3;
            };
            };
            draw_bold_text_with_bright_colors = false;
            background_opacity = 0.9;
            key_bindings = [
            {
                key = "V";
                mods = "Control|Shift";
                action = "Paste";
            }
            {
                key = "C";
                mods = "Control|Shift";
                action = "Copy";
            }
            {
                key = "Up";
                mods = "Control|Shift";
                action = "ScrollPageUp";
            }
            {
                key = "Down";
                mods = "Control|Shift";
                action = "ScrollPageDown";
            }
            ];

            custom_cursor_colors = true;

            cursor = {
            style = "Beam";
            };

            # Base16 Material Palenight 256 - alacritty color config{{{
            # Nate Peterson
            colors = {
            # Default colors
            primary = {
                background = "0x292d3e";
                foreground = "0x959dcb";
            };
            # Colors the cursor will use if `custom_cursor_colors` is true
            cursor = {
                text = "0x202331";
                cursor = "0xc792ea";
            };
            # Normal colors
            normal = {
                black = "0x292d3e";
                red = "0xf07178";
                green = "0xc3e88d";
                yellow = "0xffcb6b";
                blue = "0x82aaff";
                magenta = "0xc792ea";
                cyan = "0x89ddff";
                white = "0x959dcb";
            };
            # Bright colors
            bright = {
                black = "0x676e95";
                red = "0xf07178";
                green = "0xc3e88d";
                yellow = "0xffcb6b";
                blue = "0x82aaff";
                magenta = "0xc792ea";
                cyan = "0x89ddff";
                white = "0xffffff";
            };
            indexed_colors = [
                {
                index = 16;
                color = "0xf78c6c";
                }
                {
                index = 17;
                color = "0xff5370";
                }
                {
                index = 18;
                color = "0x444267";
                }
                {
                index = 19;
                color = "0x32374d";
                }
                {
                index = 20;
                color = "0x8796b0";
                }
                {
                index = 21;
                color = "0x959dcb";
                }
            ];
            }; # }}}

        };
        }; # }}}

        # tmux{{{
        tmux = {
        enable = true;
        plugins = with pkgs; [
            tmuxPlugins.vim-tmux-navigator
            tmuxPlugins.prefix-highlight
        ];
        keyMode = "vi";
        extraConfig = ''
            set-option -g default-terminal "screen-256color"
            set -ga terminal-overrides ",*256col*:Tc"
            set-option -g prefix C-Space
            bind C-Space send-prefix
            bind s split-window -h
            bind v split-window -v
            source-file ./.palenight-tmux
        '';
        }; # }}}

        # bat{{{
        bat = {
        enable = true;
        config = {
            theme = "palenight";
            style = "numbers,changes,header";
            italic-text = "always";
        };

        themes = {
            palenight = builtins.readFile (
            pkgs.fetchgit {
                url = "https://github.com/equinusocio/material-theme";
                rev = "614b7e8bc7369c32e852297d42253643ebf90d55";
                sha256 = "1gjfisksvqa2d08na0yln7yxny4i16wrmvlfnwllbqrgwh26v94g";
            } + "/schemes/Material-Theme-Palenight.tmTheme"
            );
        };
        };

        # }}}

        # lsd{{{
        lsd = {
        enable = true;
        enableAliases = false; # let me use custom alias.
        };
        # }}}

        # mpv{{{
        mpv = {
        enable = true;
        config = {
            volume = 50;
            background = "#292D3E";
        };
        bindings = {
            h = "seek -10";
            j = "add volume -2";
            k = "add volume 2";
            l = "seek 10";
            "Ctrl+l" = "ab-loop";
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
            url.start_pages = [ "https://btwiusegentoo.github.io/start.html" ];
            url.default_page = "https://btwiusegentoo.github.io/start.html";
            scrolling = {
            smooth = true;
            bar = "always";
            };

            fonts = {
            default_family = "SFNS Diplay";
            web.family.fixed = "Tamzen";
            completion = {
                category = "13pt Tamzen";
                entry = "13pt Tamzen";
            };
            contextmenu = "13pt Tamzen";
            debug_console = "13pt Tamzen";
            default_size = "9pt";
            downloads = "13pt Tamzen";
            hints = "13pt Tamzen";
            keyhint = "13pt Tamzen";
            messages = {
                error = "13pt Tamzen";
                info = "13pt Tamzen";
                warning = "13pt Tamzen";
            };
            prompts = "13pt Tamzen";
            statusbar = "13pt Tamzen";
            tabs.selected = "13pt Tamzen";
            tabs.unselected = "13pt Tamzen";
            #tabs = "13pt Tamzen";
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
                fg = "#959DCB";
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
            }; # }}}
        };
        # }}}

        extraConfig = ''
            config.bind('yd', 'spawn mpv {url}')
            config.bind('yf', 'hint links spawn mpv --force-window yes {hint-url}')
            c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}'}
        '';

        };
        # }}}

        # zathura{{{
        zathura = {
        enable = true;
        options = {
            recolor = "true";
            recolor-lightcolor = "#292D3E";
            recolor-darkcolor = "#A6ACCD";
            default-bg = "#292D3E";
            default-fg = "#A6ACCD";
            statusbar-bg = "#202331";
            statusbar-fg = "#A6ACCD";
            inputbar-bg = "#202331";
            inputbar-fg = "#A6ACCD";
            highlight-color = "#444267";
            highlight-active-color = "#82aaff";
        };
        };
        # }}}

        # starship{{{
        starship = {
        enable = true;
        enableFishIntegration = true;
        package = unstable.starship;
        settings = {
            add_newline = true;

            character = {
            style_success = "#c792ea";
            use_symbol_for_status = true;
            symbol = ">";
            vicmd_symbol = "<";
            error_symbol = "☓";
            };

            directory = {
            style = "cyan";
            };

            nix_shell = {
            disabled = false;
            use_name = true;
            symbol = " ";
            };

        };
        };
        # }}}

        # fzf {{{
        fzf = {
            enable = true;
            enableFishIntegration = true;
            defaultCommand = "fd --type f";
            defaultOptions = [ "--color=bg+:0,bg:#292D3E,spinner:#89DDFF,hl:#82AAFF,fg:#8796B0,header:#82AAFF,info:#FFCB6B,pointer:#89DDFF,marker:#89DDFF,fg+:#959DCB,prompt:#c792ea,hl+:#82AAFF" ];
        };
        #}}}
    };

    #}}}

  # home files{{{
  home.file = {

    # xmobar{{{
    ".xmonad/xmobar.hs".source = ../haskell/xmobar.hs;
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
    ''; # }}}

    # tmux theme
    ".palenight-tmux".source = ../.palenight-tmux;

    # scripts{{{
    #  example to make executable
    #".scripts/pymodoro.py".source = pkgs.writeScript "pymodoro.py" (builtins.readFile ( pkgs.fetchurl {
    #url = "https://raw.githubusercontent.com/dattanchu/pymodoro/master/pymodoro/pymodoro.py";
    #sha256 = "076gd0kkc3mn1rkw1hmhxf9iiyl0qz4rs5mjlaqpby3ww14dp1mn";
    #} ) );

    # }}}

    # emojis
    "textfiles/UnicodeData.txt".source = ../textfiles/UnicodeData.txt;

    # neofetch ascii
    "textfiles/neofetchascii.txt".source = ../textfiles/neofetchascii.txt;

  };
  #}}}

    # .config files{{{
    xdg.configFile = {

        #Coc{{{
        "nvim/coc-settings.json".text = ''
        {

            "html.autoClosingTags": true,
            "html.format.enable": true,
            "html.format.indentInnerHtml": true,

            "explorer.icon.enableNerdfont": true,

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
            },

            "coc.source.emoji.filetypes": [
                "markdown",
                "html",
                "css",
                "gitcommit",
                "org"
            ]

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
        "neofetch/config.conf".text = ''
        print_info() {

            prin "$(color 4)────────────────────────────────────────────" 
            info "Distro"     distro
            info "Uptime" uptime
            info "Shell"  shell
            info "Session"     de
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
        separator="\t"
        block_range=(0 15)
        color_blocks="on"
        block_width=3
        block_height=1
        col_offset="auto"
        image_backend="ascii"
        image_source="$HOME/textfiles/neofetchascii.txt"
        ascii_distro="nixos"
        ascii_colors=(5)
        ascii_bold="off"
        gap=5
        stdout="off"
        '';
        # }}}

        # fontconfig{{{
        "fontconfig/conf.d/10-prefer-emoji.conf".text = ''
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

        "fontconfig/conf.d/10-symbols.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
            <alias>
                <family>Tamzen</family>
                <prefer>
                    <family>GohuFont Nerd Font</family> 
                    <family>Apple Color Emoji</family>
                </prefer>
            </alias>
        </fontconfig>
        '';

        "fontconfig/conf.d/65-nonlatin.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
            <!-- Default font for the ja_JP locale (no fc-match pattern) -->
            <match>
                <test compare="contains" name="lang">
                    <string>ja</string>
                </test>
                <edit mode="prepend" name="family">
                    <string>Noto Sans CJK JP</string>
                </edit>
            </match>
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
        ''; # }}}

        # ncpamixer{{{
        "ncpamixer.conf".text = ''
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
        ''; # }}}

        # keynav{{{
        "keynav/keynavrc".text = ''
            super+semicolon start        
        '';
        # }}}

        # nixpkgs{{{
        "nixpkgs/config.nix".text = ''
            { allowUnfree = true; } 
        '';
        # }}}

    };
    # }}}

    # xsession{{{
    xsession = {
        enable = true;
        scriptPath = ".hm-xsession";
        profileExtra = "xrandr --output DVI-D-0 --scale 1.33333333333333x1.33333333333333 --panning 2560x1440 ";

        #xmonad{{{
        windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ../haskell/xmonad.hs;
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

    }; #}}}

    # xresources config{{{
    xresources.properties = {
        "Xft.dpi" = 96;
    };
    # }}}

    # Home Manager config{{{
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

    #}}}

    # local env variables{{{
    home.sessionVariables = {
        "XDG_CONFIG_HOME" = "$HOME/.config";
        "MANPAGER" = "sh -c 'col -bx | bat -l man -p'";
        "NNN_PLUG" = "p:preview-tabbed";
        "COLORTERM" = "truecolor";
    }; # }}}

    nixpkgs.config = import ../nixpkgs/config.nix;

    nixpkgs.overlays = import ../overlays/all-overlays.nix { inherit pkgs; };

}
# vim:ft=nix fdm=marker sw=4:
