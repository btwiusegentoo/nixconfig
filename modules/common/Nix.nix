{ pkgs, unstable, ... }:

{
    nix.package = pkgs.nixFlakes;

    nix.extraOptions = ''
        experimental-features = nix-command flakes
        builders-use-substitutes = true
    '';
}
