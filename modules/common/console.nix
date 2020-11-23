# This file is generated from "README.org"
{ pkgs, ... }:
{
  console = {
    colors = [ "434759" "f07178" "c3e88d" "ffcb6b" "82aaff" "c792ea" "89ddff" "d0d0d0" "434758" "ff8b92" "ddffa7" "ffe585" "9cc4ff" "e1acff" "a3f7ff" "fefefe" ];
    earlySetup = true;
    packages = with pkgs; [ unstable.spleen ];
    font = "spleen-6x12";
    keyMap = "dvorak";
  };
}
