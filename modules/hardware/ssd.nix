# This file is generated from "README.org"
{
  services.fstrim.enable = true;
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
}
