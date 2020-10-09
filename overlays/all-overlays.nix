{ pkgs }:

[
(import ./packages.nix)
(import ./overrides.nix { inherit pkgs; })
]
