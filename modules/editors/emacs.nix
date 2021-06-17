# This file is generated from "README.org"
{ pkgs, ...}:
{
  #programs.emacs.enable = true;
  programs.emacs.package = pkgs.unstable.emacsPgtkGcc;
  programs.emacs.extraPackages = (epkgs: [ epkgs.vterm ]);
  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [ "-c" ];
    };
    socketActivation.enable = false;
  };
}
