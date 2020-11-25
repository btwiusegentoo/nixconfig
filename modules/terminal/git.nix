# This file is generated from "README.org"
{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    package = pkgs.unstable.git;
    extraConfig.init.defaultBranch = "main";
    extraConfig.url."git@github.com:" = {
        pushinsteadOf = "https://github.com/";
    };
    userName = "btwiusegentoo";
    userEmail = "66811008+btwiusegentoo@users.noreply.github.com";
  };
}
