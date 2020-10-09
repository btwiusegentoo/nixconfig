{ pkgs }:

{
    enable = true;
    config = {
        theme = "palenight";
        style = "numbers,changes,header";
        italic-text = "always";
    };

    themes = {
        palenight = builtins.readFile (
            pkgs.fetchgit {
                url = "https://github.com/equinusocio/material-theme";
                rev = "614b7e8bc7369c32e852297d42253643ebf90d55";
                sha256 = "1gjfisksvqa2d08na0yln7yxny4i16wrmvlfnwllbqrgwh26v94g";
            } + "/schemes/Material-Theme-Palenight.tmTheme"
        );
    };
}
