# This file is generated from "README.org"
{ pkgs, ... }:
{
  fonts = {
    <<system-font-config>>
    <<enable-font-dir>>
    <<enable-ghostscript-fonts>>
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
