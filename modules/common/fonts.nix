{ pkgs }:

{
    fontconfig = {
        enable = true;
        allowBitmaps = true;
        useEmbeddedBitmaps = false;
    };
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
        (nerdfonts.override { fonts = ["Gohu"]; })
        unstable.dejavu_fonts
        unstable.tamzen
        #(tamzen-nerdfont.override { size = "10x20";})
        san-francisco-font
        apple-color-emoji
        noto-fonts-cjk
        emacs-all-the-icons-fonts
    ];
}
