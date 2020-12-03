# This file is generated from "README.org"
final: prev:
{
  neovim-unwrapped = prev.neovim-unwrapped.overrideAttrs (old: rec {
    ## use neovim nightly
    version = "master";
    src = builtins.fetchGit {
      url = "https://github.com/neovim/neovim";
      rev = "8fb786e415d1c3538452885455b2268d13f640a6";
      sha256 = "1smq0y7r3s2qqsi11vr9jb5xmch8p9krfxmx9rrmfg7qn38awzkg";
    };
  });
}
