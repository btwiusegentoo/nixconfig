# This file is generated from "README.org"
{
  programs.alacritty.enable = true;
  programs.alacritty.settings.colors = {
    # Default colors
    primary = {
      background = "0x292d3e";
      foreground = "0x959dcb";
    };
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
  };
  programs.alacritty.settings.font = {
    normal = {
      family = "Iosevka Nerd Font";
      style = "Medium";
    };
    bold = {
      family = "Iosevka Nerd Font";
      style = "Bold";
    };
    italic = {
      family = "Iosevka Nerd Font";
      style = "Italic";
    };
    size = 10;
    offset = {
      x = 0;
      y = 0;
    };
    glyph_offset = {
      x = 0;
      y = 0;
    };
  };
  programs.alacritty.settings.env.TERM = "xterm-256color";
  programs.alacritty.settings.window.padding = {
      x = 12;
      y = 0;
  };
  programs.alacritty.settings.window.dynamic_padding = false;
  programs.alacritty.settings.window.decorations = "none";
  programs.alacritty.settings.window.startup_mode = "Windowed";
  programs.alacritty.settings.scrolling = {
      history = 10000;
      multiplier = 3;
  };
  programs.alacritty.settings.draw_bold_text_with_bright_colors = true;
  programs.alacritty.settings.background_opacity = 0.90;
  programs.alacritty.settings.key_bindings = [
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
  programs.alacritty.settings.cursor = {
    style = "Beam";
  };
  programs.alacritty.settings.custom_cursor_colors = true;
}
