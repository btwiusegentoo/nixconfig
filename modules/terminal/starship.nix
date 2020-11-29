# This file is generated from "README.org"
{ pkgs, ... }:
{
programs.starship.enable = true;
programs.starship.enableFishIntegration = true;
programs.starship.package = pkgs.unstable.starship;
  programs.starship = {
    settings = {
      add_newline = true;

      character = {
        success_symbol = "[𝝺](#c792ea)";
        vicmd_symbol = "[ ](bold green)";
        error_symbol = "[☓ ](bold red)";
      };

      directory = {
        style = "bold cyan";
      };

      nix_shell = {
        disabled = false;
        symbol = " ";
      };

    };
  };
}
