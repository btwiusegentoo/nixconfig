# This file is generated from "README.org"
{ pkgs, ... }:
{
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    config = {
        General = {
            ControllerMode = "bredr";
        };
    };
  };
}
