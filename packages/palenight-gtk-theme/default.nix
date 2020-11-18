{ stdenv, fetchFromGitHub, sassc, inkscape, optipng, ... }:

stdenv.mkDerivation rec {
  version = "0.2.0";
  pname = "palenight-gtk-theme";

  src = fetchFromGitHub {
    owner = "jaxwilko";
    repo = "gtk-theme-framework";
    rev = "b695bf4cf153691e485c09c1c744c46b6f025a74";
    sha256 = "0bfhv7ywgmw2s6kfxb7wgpri7gh6yd89gaazj1dxgz690b7mliv7";
  };

  nativeBuildInputs = [ sassc inkscape optipng ];

  postPatch = ''
    patchShebangs .
  '';

  buildPhase = ''
    ./main.sh -vcf
  '';

  installPhase = ''
    mkdir -p $out/share/themes
    mv ./dist/palenight/theme ./dist/palenight/palenight
    cp -r ./dist/palenight/palenight $out/share/themes
  '';

}
