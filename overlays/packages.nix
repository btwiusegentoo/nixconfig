self: super: rec {
    san-francisco-font = super.callPackage ../packages/san-francisco-font { };
    apple-color-emoji = super.callPackage ../packages/apple-color-emoji { };
    tamzen-nerdfont = super.callPackage ../packages/tamzen-nerdfont { };
    mosh-master = super.callPackage ../packages/mosh-master { };
    scientifica = super.callPackage ../packages/scientifica { };
}

# vim: shiftwidth=4:
