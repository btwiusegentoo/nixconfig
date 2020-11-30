# This file is generated from "README.org"
{ pkgs, ... }:
{
  programs.git.enable = true;
  programs.git.package = pkgs.unstable.git;
  programs.git.extraConfig.init.defaultBranch = "main";
  programs.git.extraConfig.url."git@github.com:" = {
      pushinsteadOf = "https://github.com/";
  };
  programs.git.userName = "btwiusegentoo";
  programs.git.userEmail = "66811008+btwiusegentoo@users.noreply.github.com";
}
