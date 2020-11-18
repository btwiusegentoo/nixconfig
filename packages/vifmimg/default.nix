{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  version = "unstable-2020-06-23";
  pname = "vifmimg";

  src = fetchFromGitHub {
    owner = "cirala";
    repo = "vifmimg";
    rev = "afe4f159e1acc9a54dd3a2dc28850f11f97c1ebf";
    sha256 = "11pbfayww7pwyk8a7jgvijx9i96ahyvn5y84gpn0yjd0b9wf75bn";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp vifm* $out/bin
  '';
}
