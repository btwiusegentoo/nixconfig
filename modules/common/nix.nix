{ pkgs, ... }:
{
  nix.package = pkgs.nixFlakes;

  nix.extraOptions = ''
    experimental-features = nix-command flakes
    builders-use-substitutes = true
  '';
  nix.allowedUsers = [ "@wheel" ];
  nix.trustedUsers = [ "root" "@wheel" ];
}
