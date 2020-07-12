self: super: rec {
    san-francisco-font = super.callPackage ../packages/san-francisco-font { };
    apple-color-emoji = super.callPackage ../packages/apple-color-emoji { };
}

# vim: shiftwidth=4:
