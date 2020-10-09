{ stdenv, lib }:

stdenv.mkDerivation rec {
    version = "2.1";
    name = "scientifica";

    src = fetchTarball "https://github.com/NerdyPepper/scientifica/releases/download/v2.1/scientifica-v2.1.tar";

    installPhase = ''
        mkdir -p $out/share/fonts/scientifica
        cp ttf/*.ttf $out/share/fonts/scientifica 
    '';
}
