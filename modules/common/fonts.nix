# This file is generated from "README.org"
{ pkgs, ... }:
{
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
    unstable.dejavu_fonts
    spleen
    apple-color-emoji
    noto-fonts-cjk
    emacs-all-the-icons-fonts
    etBook
    liberation_ttf_v2
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
  fonts.fontDir.enable = true;
  fonts.enableGhostscriptFonts = true;
}
