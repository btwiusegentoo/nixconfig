{ pkgs }:
let
    unstable = pkgs.unstable;

in
{
    enable = true;
    enableFishIntegration = true;
    package = unstable.starship;
    settings = {
        add_newline = true;

        character = {
            style_success = "#c792ea";
            use_symbol_for_status = true;
            symbol = "𝝺";
            vicmd_symbol = " ";
            error_symbol = "☓ ";
        };

        directory = {
            style = "cyan";
        };

        nix_shell = {
            disabled = false;
            use_name = true;
            symbol = " ";
        };

    };
}
