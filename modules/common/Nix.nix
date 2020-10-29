{ pkgs, ... }:

{
    package = pkgs.nixUnstable;

    extraOptions = ''
        experimental-features = nix-command flakes
        builders-use-substitutes = true
    '';
}
