# This file is generated from "README.org"
{
  programs.qutebrowser.enable = true;
  programs.qutebrowser.searchEngines = {
      DEFAULT = "https://google.com/search?q={}";
  };
  programs.qutebrowser.settings.qt.args = [ "enable-native-gpu-memory-buffers" "enable-gpu-rasterization" "use-gl=egl" "ignore-gpu-blacklist" "num-raster-threads=4" ];
  programs.qutebrowser.keyBindings.normal."yd" = "spawn mpv --ytdl-format='bestvideo[height<=?1080][vcodec!=vp9]+bestaudio/best' {url}";
  programs.qutebrowser.keyBindings.normal."yf" = "hint links spawn mpv --ytdl-format='bestvideo[height<=?1080][vcodec!=vp9]+bestaudio/best' --force-window yes {hint-url}";
  programs.qutebrowser.settings.url.start_pages = [ "https://btwiusegentoo.github.io/start.html" ];
  programs.qutebrowser.settings.url.default_page = "https://btwiusegentoo.github.io/start.html";
  programs.qutebrowser.settings.zoom.default = "90%";
  programs.qutebrowser.settings.hints.chars = "aoeuidhtns";
  programs.qutebrowser.settings.fonts = {
    default_family = "Liberation Sans";
    web.family.standard = "Liberation Sans";
    web.family.serif = "Liberation Sans";
    web.family.sans_serif = "Liberation Sans";
    web.family.fixed = "Iosevka Nerd Font";
    completion = {
      category = "10pt Iosevka Nerd Font";
      entry = "10pt Iosevka Nerd Font";
    };
    contextmenu = "10pt Iosevka Nerd Font";
    debug_console = "10pt Iosevka Nerd Font";
    default_size = "10.5pt";
    downloads = "10pt Iosevka Nerd Font";
    hints = "11pt Liberation Sans";
    keyhint = "10pt Iosevka Nerd Font";
    messages = {
      error = "10pt Iosevka Nerd Font";
      info = "10pt Iosevka Nerd Font";
      warning = "10pt Iosevka Nerd Font";
    };
    prompts = "10pt Iosevka Nerd Font";
    statusbar = "10pt Iosevka Nerd Font";
    tabs.selected = "10pt Iosevka Nerd Font";
    tabs.unselected = "10pt Iosevka Nerd Font";
    #tabs = "10pt Iosevka Nerd Font";
  };
  
  programs.qutebrowser.settings.colors = {
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
  programs.qutebrowser.settings.completion.web_history.max_items = 30;
  programs.qutebrowser.extraConfig = ''
      c.editor.command = ["emacsclient", "-c", "--eval", "(find-file \"{}\")", "(org-mode)"]
    '';
}
