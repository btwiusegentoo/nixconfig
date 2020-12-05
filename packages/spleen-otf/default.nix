# This file is generated from "README.org"
{ stdenv, lib, fetchurl, mkfontscale }:

let
  pname = "spleen-otf";
  version = "1.8.2";
in
stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  src = fetchurl {
    url = "https://github.com/fcambus/spleen/releases/download/${version}/spleen-${version}.tar.gz";
    sha256 = "1pvg34xzi2cqgwpcl56j55rlwz44qasv3fa1jln2b1il8272s9hn";
  };
  phases = [ "unpackPhase" "installPhase" ];
  sourceRoot = "./";
  unpackCmd = ''
    tar xvf $curSrc --strip=1
  '';
  installPhase = ''
    d="$out/share/fonts/misc"
    install -D -m 644 *.otf -t "$d"
    install -D -m 644 *.psfu -t "$out/share/consolefonts"
    install -m644 fonts.alias-spleen $d/fonts.alias

    # create fonts.dir so NixOS xorg module adds to fp
    ${mkfontscale}/bin/mkfontdir "$d"
  '';

}
