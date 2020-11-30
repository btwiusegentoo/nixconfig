# This file is generated from "README.org"
{
  programs.mpv.enable = true;
  programs.mpv.config.background = "#292D3E";
  programs.mpv.config.hwdec = "auto";
  programs.mpv.config.hwdec-codecs = "all";
  programs.mpv.bindings = {
      h = "seek -10";
      j = "add volume -2";
      k = "add volume 2";
      l = "seek 10";
      "Ctrl+l" = "ab-loop";
  };
}
