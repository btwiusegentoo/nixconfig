# This file is generated from "README.org"
{ pkgs, ... }:
{
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Gohu" ]; })
    unstable.dejavu_fonts
    unstable.spleen
    san-francisco-font
    apple-color-emoji
    noto-fonts-cjk
    emacs-all-the-icons-fonts
  ];
      fonts.fontconfig = {
        defaultFonts = {
          emoji = [ "Apple Color Emoji" ];
          monospace = [ "Spleen" ];
        };
      };
  fonts.fontconfig.enable = true;
  fonts.fontconfig.allowBitmaps = true;
  fonts.fontconfig.useEmbeddedBitmaps = true;
  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
}
