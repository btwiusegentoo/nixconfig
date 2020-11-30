# This file is generated from "README.org"
{ pkgs, ... }:
{
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;
  hardware.bluetooth.config = {
      General = {
          ControllerMode = "bredr";
      };
  };
}
