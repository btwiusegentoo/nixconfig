{ pkgs, unstable, master }:

[
(import ./packages.nix)
(import ./overrides.nix { inherit pkgs unstable master; })
]
