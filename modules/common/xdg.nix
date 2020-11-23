# This file is generated from "README.org"
{ pkgs, ... }:

{
  enable = true;
  userDirs.enable = true;
  mime.enable = true;
  mimeApps = {
  enable = true;
  defaultApplications = {
      "text/html" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
  };
  };
  configFile = import ./xdg-configfiles.nix { inherit pkgs; };
}
