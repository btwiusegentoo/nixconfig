# This file is generated from "README.org"
{ pkgs, ... }:
{
  fonts = {
        fontconfig = {
          enable = true;
          allowBitmaps = true;
          useEmbeddedBitmaps = true;
          defaultFonts = {
            emoji = [ "Apple Color Emoji" ];
            monospace = [ "Spleen" ];
          };
        };
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Gohu" ]; })
      unstable.dejavu_fonts
      unstable.spleen
      san-francisco-font
      apple-color-emoji
      noto-fonts-cjk
      emacs-all-the-icons-fonts
    ];
  };
}
