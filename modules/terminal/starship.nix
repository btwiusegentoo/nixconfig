# This file is generated from "README.org"
{ pkgs, ... }:
{
programs.starship.enable = true;
programs.starship.enableFishIntegration = true;
programs.starship.package = pkgs.unstable.starship;
programs.starship.settings.add_newline = true;
programs.starship.settings.character = {
  success_symbol = "[𝝺](#c792ea)";
  vicmd_symbol = "[ ](bold green)";
  error_symbol = "[☓ ](bold red)";
};
programs.starship.settings.directory = {
  style = "bold cyan";
};
programs.starship.settings.nix_shell = {
  disabled = false;
  symbol = " ";
};
}
