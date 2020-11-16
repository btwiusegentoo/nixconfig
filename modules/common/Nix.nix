{ pkgs, ... }:

{
    nix.package = pkgs.nixUnstable;

    nix.extraOptions = ''
        experimental-features = nix-command flakes
        builders-use-substitutes = true
    '';
}
