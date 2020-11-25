# This file is generated from "README.org"
{
  programs.mpv = {
    enable = true;
    config.background = "#292D3E";
    config.hwdec = "auto";
    config.hwdec-codecs = "all";
    bindings = {
        h = "seek -10";
        j = "add volume -2";
        k = "add volume 2";
        l = "seek 10";
        "Ctrl+l" = "ab-loop";
    };
  };
}
