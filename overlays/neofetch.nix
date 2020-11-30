# This file is generated from "README.org"
final: prev:
{
  neofetch = prev.neofetch.overrideAttrs (old: rec {
    version = "unstable-2020-11-02";
    src = prev.fetchFromGitHub {
      owner = "dylanaraps";
      repo = "neofetch";
      rev = "5dfce0f9c3068d4d8a49d0b7182bdace61b8f4d0";
      sha256 = "131r07wllkvrcgw0ndyf5avqpzqkym56ii0qck6qyjfa8ylx6s31";
    };
  });
}
