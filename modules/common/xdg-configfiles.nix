# This file is generated from "README.org"
{ pkgs, ... }:

{
  "nvim/coc-settings.json".source = ../../configs/coc-settings.json;
  "neofetch/config.conf".source = ../../configs/neofetch.conf;
  "ncpamixer.conf".source = ../../configs/ncpamixer.conf;

  "fontconfig/conf.d/10-prefer-emoji.conf".source = ../../configs/fontconfig/10-prefer-emoji.conf;
  "fontconfig/conf.d/10-symbols.conf".source = ../../configs/fontconfig/10-symbols.conf;
  "fontconfig/conf.d/65-nonlatin.conf".source = ../../configs/fontconfig/65-nonlatin.conf;
  "fontconfig/conf.d/10-hinting.conf".source = ../../configs/fontconfig/10-hinting.conf;

  "tridactyl/tridactylrc".text = ''
      <<tridactylrc>>
  '';

  "keynav/keynavrc".text = ''
    super+semicolon start
    s warp
    m click 6
    w click 5
    v click 4
    z click 7
  '';

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
}
