self: super: rec {
    san-francisco-font = super.callPackage ../packages/san-francisco-font { };
    apple-color-emoji = super.callPackage ../packages/apple-color-emoji { };
    mosh-master = super.callPackage ../packages/mosh-master { };
}

# vim: shiftwidth=4:
