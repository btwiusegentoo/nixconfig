{ pkgs, ... }:

{
    "nvim/coc-settings.json".source = ../../configs/coc-settings.json;
    "neofetch/config.conf".source = ../../configs/neofetch.conf;
    "ncpamixer.conf".source = ../../configs/ncpamixer.conf;

    "fontconfig/conf.d/10-prefer-emoji.conf".source = ../../configs/fontconfig/10-prefer-emoji.conf;
    "fontconfig/conf.d/10-symbols.conf".source = ../../configs/fontconfig/10-symbols.conf;
    "fontconfig/conf.d/65-nonlatin.conf".source = ../../configs/fontconfig/65-nonlatin.conf;


    # keynav{{{
    "keynav/keynavrc".text = ''
            super+semicolon start
            s warp
            m click 6
            w click 5
            v click 4
            z click 7
        '';
    # }}}

    # flashfocus
    "flashfocus/flashfocus.yml".source = ../../configs/flashfocus.yml;

    # nixpkgs{{{
    "nixpkgs/config.nix".text = ''
            { allowUnfree = true; }
        '';
    # }}}

    "vifm/vifmrc".source = ../../configs/vifmrc.vim;

    "PulseEffects/output/Perfect EQ.json".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/JackHack96/PulseEffects-Presets/master/Perfect%20EQ.json";
        name = "perfecteq.json";
        sha256 = "04mjqsiajqdzp43fn16j6sfz3972yfpqq6s2vw0cnim5bp9a642b";
    };

    # stylesheet css
    "qutebrowser/css/palenight-all-sites.css".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/btwiusegentoo/palenight-everything-css/main/palenight-all-sites.css";
        sha256 = "12l5mg3xp716n3z2grx4cqj1inc3w8wfd9hpjny184lb3asx2as5";
    };

    # statusbar nyancat
    "qutebrowser/qutenyan".source = pkgs.fetchgit {
        url = "https://gitlab.com/jgkamat/qutenyan";
        rev = "1fd054f061b36c8c342a9e7747cb69c33bf72160";
        sha256 = "0x37kx3czh1g1as4yasx8j33gab03ajxgy23d53wzvxvfrmyza3q";
    };

    ## for local testing purposes
    #"qutebrowser/css/palenight-all-sites.css".source = ../../projects/palenight-everything-css/palenight-all-sites.css;
}
