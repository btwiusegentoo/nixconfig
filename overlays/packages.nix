self: super: rec {
    san-francisco-font = super.callPackage ../packages/san-francisco-font { };
    #apple-color-emoji = super.callPackage ../packages/apple-color-emoji { };
    apple-color-emoji = super.callPackage ../../nixpkgs/pkgs/data/fonts/apple-color-emoji { };
    tamzen-nerdfont = super.callPackage ../packages/tamzen-nerdfont { };
    mosh-master = super.callPackage ../packages/mosh-master { };
}

# vim: shiftwidth=4:
