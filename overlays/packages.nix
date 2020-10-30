self: super: rec {
    san-francisco-font = super.callPackage ../packages/san-francisco-font { };
    apple-color-emoji = super.callPackage ../packages/apple-color-emoji { };
    tamzen-nerdfont = super.callPackage ../packages/tamzen-nerdfont { };
    palenight-gtk-theme = super.callPackage ../packages/palenight-gtk-theme { };
    vifmimg = super.callPackage ../packages/vifmimg { };
}

# vim: shiftwidth=4:
