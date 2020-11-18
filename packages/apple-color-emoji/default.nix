{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "apple-color-emoji-${version}";
  version = "2020-06-30";

  src = fetchurl {
    url = https://github.com/samuelngs/apple-emoji-linux/releases/download/latest/AppleColorEmoji.ttf;
    sha256 = "0xaclj29b7xgqin8izkabrm2znp1m01894fngyxrhwbf9nbncp4g";
  };

  phases = [ "unpackPhase" "installPhase" ];

  sourceRoot = "./";

  unpackCmd = ''
    cp $curSrc ./AppleColorEmoji.ttf
  '';

  installPhase = ''
    mkdir -p $out/share/fonts/apple-color-emoji
    cp *.ttf $out/share/fonts/apple-color-emoji
  '';

  meta = {
    description = "Apple Color Emoji is a color typeface used by iOS and macOS to display emoji";
    homepage = https://github.com/samuelngs/apple-emoji-linux;
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
    maintainers = [ ];
  };

}

# vim: foldmethod=marker shiftwidth=4 ft=nix:
