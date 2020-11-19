{ pkgs }:
{
  fontconfig = {
    enable = true;
    allowBitmaps = true;
    useEmbeddedBitmaps = true; # THIS NEED TO BE ENABLED TO DISPLAY EMOJI
  };
  enableFontDir = true;
  enableGhostscriptFonts = true;
  fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Gohu" ]; })
    unstable.dejavu_fonts
    unstable.tamzen
    unstable.spleen
    #(tamzen-nerdfont.override { size = "10x20";})
    san-francisco-font
    apple-color-emoji
    noto-fonts-cjk
    emacs-all-the-icons-fonts
  ];
}
