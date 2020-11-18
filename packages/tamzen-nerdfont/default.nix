{ fetchFromGitHub, stdenv, fetchurl, size ? [ ] }:

# select size from
# 10x20 5x9 6x12 7x13 7x14 8x15 8x16
let
  knownSize = "7x14";
  selectedSize =
    if (size == [ ]) then
      knownSize
    else
      size
  ;

in
stdenv.mkDerivation rec {
  name = "tamzen-nerdfont-${version}";
  version = "1.11";

  src = fetchFromGitHub {
    "owner" = "btwiusegentoo";
    "repo" = "tamzen-nerdfont";
    "rev" = "5ccaef6ad5187818c883648a19841b1ea34a418d";
    "sha256" = "0ay1gk9ql51xkl2gq302lv9sbz4djqmw1zg7anm04hvszga8dmh5";
  };

  phases = [ "installPhase" ];

  sourceRoot = "./";

  installPhase = ''
    mkdir -p $out/share/fonts/tamzen-nerdfont
    cp ${src}/patchedtamzen/Tamzen${selectedSize}r.ttf $out/share/fonts/tamzen-nerdfont
    cp ${src}/patchedtamzen/Tamzen${selectedSize}b.ttf $out/share/fonts/tamzen-nerdfont
  '';

  meta = {
    description = ":love_letter: Bitmapped programming font, based on Tamsyn";
    homepage = https://github.com/btwiusegentoo/tamzen-nerdfont;
    maintainers = [ ];
  };

}
# vim: foldmethod=marker shiftwidth=4 ft=nix:
