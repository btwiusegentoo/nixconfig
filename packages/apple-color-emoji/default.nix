{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
    name = "apple-color-emoji";
    version = "2020-04-13";

    src = pkgs.fetchFromGitHub {
        owner = "samuelngs";
        repo = "apple-emoji-linux";
        rev = "8c247a77eb0abe9fca1011a6ab75520dc761ff37";
        sha256 = "1r0laf5g4gjrrv2xzr1l5bvkn7px9k02p6wknxaafm3mrmpxrk23";
    };

    nativeBuildInputs = with pkgs; [ which imagemagick optipng zopfli pngquant ]
                ++ (with python3Packages; [ python fonttools nototools ]);

    enableParallelBuilding = true;

    installPhase = ''
        install -m444 -Dt $out/share/fonts/apple-color-emoji AppleColorEmoji.ttf 
    '';

    meta = with stdenv.lib; {
        description = "Color typeface used by iOS and macOS to display emoji";
        homepage = "https://github.com/samuelngs/apple-emoji-linux";
        license = [ licenses.ofl licenses.asl20 ];
        maintainers = with maintainers; [ btwiusegentoo ];
    };
}
