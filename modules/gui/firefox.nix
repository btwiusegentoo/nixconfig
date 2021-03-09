# This file is generated from "README.org"
{ pkgs, ... }:
{
  programs.firefox.enable = true;
  programs.firefox.package = pkgs.firefox;
  programs.firefox.extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    tridactyl
    ublock-origin
  ];
  programs.firefox.profiles.default.settings."browser.startup.homepage" = "https://btwiusegentoo.github.io/start.html";
  programs.firefox.profiles.default.settings."browser.search.isUS" = true;
  programs.firefox.profiles.default.isDefault = true;
  programs.firefox.profiles.default.name = "Default";
}
